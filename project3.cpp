#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <map>
#include <set>

using namespace std;

enum class tokentype{
    LEFT_PAREN,    //0
    RIGHT_PAREN,   //1
    INT,           //2
    STRING,        //3
    DOT,           //4
    FLOAT,         //5
    NIL,           //6
    T,             //7
    QUOTE,         //8
    SYMBOL,        //9
    ATOM,          //10
};

enum class nodetype{ ATOM, CONS, NIL };

struct Token{
    tokentype type;
    string content;
    int col;
    int row;

    Token(tokentype t, string c, int column, int rownum) : type(t), content(c), col(column), row(rownum) {}
};

struct TreeNode{
    nodetype type;
    tokentype atomtype;
    string content;

    TreeNode* left;
    TreeNode* right;

    TreeNode(string c, tokentype t) : type(nodetype::ATOM), atomtype(t), content(c), left(nullptr), right(nullptr) {}

    TreeNode(string c, TreeNode* l, TreeNode* r) : type(nodetype::CONS), content(c), left(l), right(r) {}
    
    TreeNode(nodetype t) : type(nodetype::NIL), left(nullptr), right(nullptr) {} 
};

struct Numbertype{
    bool isFloat;
    float floatValue;
    int intValue;
};

struct UserFunction {
    vector<string> parameters;
    TreeNode* body; 

    UserFunction(vector<string> p, TreeNode* b) : parameters(p), body(b) {}
};


set<string> reserved = {
    "cons", "list", "quote", "define", "car", "cdr", "atom?", "pair?", "list?", "null?", "integer?", "real?", "exit",
    "number?", "string?", "boolean?", "symbol?", "+", "-", "*", "/", "not", "and", "or", ">", ">=", "<", "<=", "=", 
    "string-append", "string>?", "string<?", "string=?", "eqv?", "equal?", "begin", "if", "cond", "clean-environment",
    "let", "lambda", "verbose?", "verbose"
};

int col = 0;
int row = 1;
int lp = 0;
int rp = 0;
int q = 0;
int errortype = 0;
int toplevel = 0;
bool existtree = false;
bool evalerror = false;
bool syntaxerror = false;
bool eof = false;
bool after = true;
bool needprint = true;
bool verbose = true;
string errorop = "";
vector<Token> tokens;
Token errortoken(tokentype::SYMBOL, "ERROR", 0, 0);
TreeNode* evalerrortoken = new TreeNode(nodetype::NIL);
map<string, TreeNode*> definetable;
map<string, TreeNode*> localtable;
map<string, string> functionalias;
map<TreeNode*, UserFunction*> lambdatable;

TreeNode* eval(TreeNode* node, bool islet = false);
TreeNode* list(TreeNode* node);

TreeNode* truenode() {
    return new TreeNode("#t", tokentype::T);
}

TreeNode* falsenode() {
    return new TreeNode("nil", tokentype::NIL);
}

TreeNode* exitnode() {
    TreeNode* left = new TreeNode("#<procedure exit>", tokentype::SYMBOL);
    TreeNode* right = new TreeNode(nodetype::NIL); 
    TreeNode* exitNode = new TreeNode("(", left, right); 
    return exitNode;
}

void reset(){
    col = 0;
    row = 1;
    lp = 0;
    rp = 0;
    q = 0;
    errortype = 0;
    existtree = false;
    syntaxerror = false;
    evalerror = false;
    eof = false;
    needprint = true;
    toplevel = 0;
    errorop = "";
    tokens = {};
}

bool alldigit(string &inp){
    int count = col + 1;
    while(count < inp.size() && !isspace(inp[count])){
        if(inp[count] == ';'){
            col = count ;
            return true;
        }

        if(inp[count] == '(' || inp[count] == ')'){
            col = count;
            return true;
        } //如: .4389(1 2)

        if(!isdigit(inp[count])) return false;

        count++;
    } 

    col = count;
    return true;
}

Token returnsymbol(string &inp){
    int start = col;

    while(col < inp.size() && !isspace(inp[col]) && inp[col] != '(' && inp[col] != ')' && inp[col] != ';' && inp[col] != '\'' && inp[col] != '\"'){
        col++;
    }

    return Token(tokentype::SYMBOL, inp.substr(start, col - start), start+1, row);
}

string roundto(string num) {
    stringstream str(num);
    double val;
    str >> val;

    stringstream out;
    out << fixed << setprecision(3) << val;
    return out.str();
}

TreeNode* copy(TreeNode* node) {
    if (node == nullptr) return nullptr;

    TreeNode* newNode = new TreeNode(node->content, node->atomtype);
    newNode->type = node->type;

    newNode->left = copy(node->left);
    newNode->right = copy(node->right);

    return newNode;
}

string erasezero(string num){
    int i = 0;
    while(i < num.size()-1){
        if(num[i] == '0' && num[i+1] != '.'){
            num.erase(0, 1);
        }

        else return num;
    }

    return num;
}

Token gettoken(string &inp){
    while(col < inp.size() && isspace(inp[col])) col++; 

    if(col >= inp.size()) return Token(tokentype::SYMBOL, "", col, row);

    char c = inp[col];

    if(c == '(') { col++; return Token(tokentype::LEFT_PAREN, "(", col, row); }
    if(c == ')') { col++; return Token(tokentype::RIGHT_PAREN, ")", col, row); }
    if(c == '\'') { col++; return Token(tokentype::QUOTE, "quote", col, row); }

    if(c == '.'){
        int start = col;
        if(isspace(inp[col + 1]) || col + 1 >= inp.size() || inp[col + 1] == '(' || inp[col + 1] == ')'){
            col++;
            return Token(tokentype::DOT, ".", col, row);
        }
        
        if(alldigit(inp)){
            string num = "";
            num = num + (inp.substr(start, col - start));
            return Token(tokentype::FLOAT, num, start+1, row);
        }
    }

    if((inp.substr(col, 3) == "nil" && (col + 3 >= inp.size() || isspace(inp[col + 3]) || inp[col + 3] == ')' || inp[col + 3] == '(')) ||
        (c == '#' && inp[col + 1] == 'f' && ( isspace(inp[col + 2]) || col + 2 >= inp.size() || inp[col + 2] == ')' || inp[col + 2] == '(' ))){
        if(c == '#'){
            col = col + 2;
            return Token(tokentype::NIL, "nil", col - 1, row);
        }

        col = col + 3;
        return Token(tokentype::NIL, "nil", col - 2, row);
    }

    if((c == 't' && (col + 1 >= inp.size() || isspace(inp[col + 1]) || inp[col + 1] == '(' || inp[col + 1] == ')' )) ||
        (c == '#' && inp[col + 1] == 't' && ( isspace(inp[col + 2]) || col + 2 >= inp.size() || inp[col + 2] == '(' || inp[col + 2] == ')' ))){
        if(c == 't'){
            col++;
            return Token(tokentype::T, "#t", col, row);
        }

        col = col + 2;
        return Token(tokentype::T, "#t", col - 1, row);
    }

    if(c == '"'){
        int start = col;
        string str = "\"";
        col++;
        while(col < inp.size()){
            if(inp[col] == '"'){
                str = str + "\"";
                col++;
                return Token(tokentype::STRING, str, start+1, row);
            }
    
            if(inp[col] == '\\'){
                if(inp[col + 1] == 'n') str = str + '\n';
                else if(inp[col + 1] == 't') str = str + '\t';
                else if(inp[col + 1] == '\\') str = str + '\\';
                else if(inp[col + 1] == '\"') str = str + '\"';
                else{
                    str = str + '\\';
                    str = str + inp[col+1];
                }   

                col++;
            }

            else    str = str + inp[col];

            col++;
        }

        return Token(tokentype::STRING, "error", col+1, row);
    }

    if(isdigit(c) || ((c == '-' ||c == '+') && ( isdigit(inp[col + 1]) || inp[col + 1] == '.'))){
        int start = col;
        string num = "";
        if(c == '-' || isdigit(c))  num = num + inp[col];
        col++;
        while(col < inp.size()){
            if(isdigit(inp[col])){
                num = num + inp[col];
                col++;
            }

            else if(isspace(inp[col])){
                num = erasezero(num);
                return Token(tokentype::INT, num, start+1, row); 
            }

            else if(inp[col] == '(' || inp[col] == ')'){
                num = erasezero(num);
                return Token(tokentype::INT, num, start+1, row);  ;
            }

            else if(inp[col] == ';'){
                col = inp.size();
                num = erasezero(num);
                return Token(tokentype::INT, num, start+1, row); 
            }

            else if(inp[col] == '.'){
                int temp = col;
                if(alldigit(inp)){
                    if((inp[start] == '+' || inp[start] == '-') && inp[start + 1] == '.' && col == start + 2){
                        col = start;
                        return returnsymbol(inp);
                    }

                    if((inp[start] == '+' || inp[start] == '-') && inp[start + 1] == '.')   num = num + "0";
                    num = num + (inp.substr(temp, col - temp));
                    num = erasezero(num);
                    return Token(tokentype::FLOAT, num, start+1, row);  
                }

                else{
                    col = start;
                    return returnsymbol(inp);
                }
            }

            else if(inp[col] == '\"'){
                num = erasezero(num);
                return Token(tokentype::INT, num, start+1, row); 
            }

            else if(!isdigit(inp[col])){
                col = start;
                return returnsymbol(inp);
            }

            else    col++;
        }

        num = erasezero(num);
        return Token(tokentype::INT, num, start+1, row);
    }

    if(c == ';'){
        col = inp.size();
        return Token(tokentype::SYMBOL, "", col, row);
    }

    return returnsymbol(inp);
}

bool checkexit(TreeNode* root){
    if(root == nullptr) return false;
    if(root->left != nullptr && root->right != nullptr){
        if(root->left->content == "#<procedure exit>" && root->content == "(" && root->right->type == nodetype::NIL){
            return true;
        }        
    }

    return false;
}

bool isreserved(string content){
    return reserved.find(content) != reserved.end();
}

bool readinput(){
    string inp;
    col = 0;
    if(!getline(cin, inp)){
        eof = true;
        return false;
    }    
    
    if(inp == ""){
        row++;
        return readinput();
    }

    while(col < inp.size() && isspace(inp[col])) col++; 
    if(inp[col] == ';' || col == inp.size()){
        row++;
        return readinput();
    }

    while(col < inp.size()){
        Token token = gettoken(inp);
        if(token.type == tokentype::LEFT_PAREN) lp++;
        else if(token.type == tokentype::RIGHT_PAREN) rp++;
        else if(token.type == tokentype::QUOTE){
            q++;
            tokens.push_back(token);
            continue;
        }

        
        if(lp == rp){
            inp = inp.substr(col, inp.size());
            col = 0;
            row = 1;
        }
        
        if(token.content.empty()) return true;
        tokens.push_back(token);
    }
    
    if(tokens.size() >= 3)
        if(tokens[0].content == "(" && tokens[1].content == "exit" && tokens[2].content == ")")   syntaxerror = true;
    
    return true;
} 

TreeNode* parse(vector<Token>& tokens, int& index){
    if(index >= tokens.size()){
        eof = true;
        syntaxerror = true;
        return nullptr;
    }

    Token token = tokens[index];
    index++;
    if(token.type == tokentype::LEFT_PAREN){
        while(index >= tokens.size()){
            row++;
            if(!readinput()) return nullptr;
        }

        if(index < tokens.size() && tokens[index].type == tokentype::RIGHT_PAREN){
            index++;
            return new TreeNode("nil", tokentype::NIL);
        }

        TreeNode* left = parse(tokens, index);
        if(syntaxerror) return nullptr;
        TreeNode* root = new TreeNode("(", left, nullptr);
        TreeNode* cur = root;

        while(index < tokens.size()){
            Token peek = tokens[index];

            if(peek.type == tokentype::DOT){
                index++; 
                
                while(index >= tokens.size()){
                    row++;
                    if(!readinput()) return nullptr;
                }

                TreeNode* right = parse(tokens, index);
                if(syntaxerror) return nullptr;
                if(right->atomtype == tokentype::NIL) right = new TreeNode(nodetype::NIL);
                cur->right = right;
                if(cur->right->content == "(")  cur->right->content = ".("; 
                
                if(index >= tokens.size()){
                    row++;
                    while(readinput()){
                        row++;
                        if(tokens[index].type == tokentype::RIGHT_PAREN){
                            index++;
                            return root;
                        }

                        else{
                            syntaxerror = true;
                            errortoken = tokens[index];
                            errortype = 1;
                            return nullptr;                            
                        }
                    }

                    if(eof) return nullptr;
                } 
                
                if(tokens[index].type != tokentype::RIGHT_PAREN){
                    syntaxerror = true;
                    errortoken = tokens[index];
                    errortype = 1;
                    return nullptr;
                }

                index++; 
                return root;
            } 
            
            else if(peek.type == tokentype::RIGHT_PAREN){
                index++; 
                cur->right = new TreeNode(nodetype::NIL);
                return root;
            } 
            
            else{
                TreeNode* left = parse(tokens, index);
                if (syntaxerror) return nullptr;
                TreeNode* cons = new TreeNode("", left, nullptr);
                cur->right = cons;
                cur = cons;
            }
        }

        row++;
        while(readinput()){
            while(index < tokens.size()){
                if(tokens[index].type == tokentype::RIGHT_PAREN){
                    index++;
                    cur->right = new TreeNode(nodetype::NIL);
                    return root;
                }
    
                else if(tokens[index].type == tokentype::DOT){
                    index++; 
                    while(index >= tokens.size()){
                        row++;
                        if(!readinput()) return nullptr;
                    }
                    
                    TreeNode* right = parse(tokens, index); 
                    if(syntaxerror) return nullptr;
                    if(right->atomtype == tokentype::NIL) right = new TreeNode(nodetype::NIL);
                    cur->right = right;
                    if(cur->right->content == "(")  cur->right->content = ".("; 
                    
                    if(index >= tokens.size()){
                        row++;
                        while(readinput()){
                            if(tokens[index].type == tokentype::RIGHT_PAREN){
                                index++;
                                return root;
                            }
    
                            else{
                                syntaxerror = true;
                                errortoken = tokens[index];
                                errortype = 1;
                                return nullptr;                            
                            }
                            
                            row++;
                        }

                        if(eof) return nullptr;
                    } 
                    
                    if(tokens[index].type != tokentype::RIGHT_PAREN){
                        syntaxerror = true;
                        errortoken = tokens[index];
                        errortype = 1;
                        return nullptr;
                    }
    
                    index++; 
                    return root;
                }
                
                else{
                    TreeNode* left = parse(tokens, index);
                    if (syntaxerror) return nullptr;
                    TreeNode* cons = new TreeNode("", left, nullptr);
                    cur->right = cons;
                    cur = cons;    
                }
            }    
            
            row++;
        }

        if(eof) return nullptr;
    }
    
    else if(token.type == tokentype::QUOTE){
        while (index >= tokens.size()) {
            row++;
            if(!readinput()) return nullptr;
        }
    
        TreeNode* right = parse(tokens, index);
        if(syntaxerror) return nullptr;
    
        TreeNode* quote = new TreeNode("quote", tokentype::QUOTE);
        TreeNode* nil = new TreeNode(nodetype::NIL);
        TreeNode* rightsub = new TreeNode("", right, nil);
        TreeNode* full = new TreeNode("(", quote, rightsub);

        return full;
    }
    
    else if(token.type == tokentype::DOT){
        syntaxerror = true;
        errortoken = token;
        errortype = 2;
        return nullptr;
    }

    else if(token.type == tokentype::RIGHT_PAREN){
        syntaxerror = true;
        errortoken = token;
        errortype = 2;
        return nullptr;
    }
    
    else if((token.type == tokentype::STRING && token.content == "error") ||
            (token.content[0] == '\"' && token.content[token.content.size() - 1] != '\"') || 
            (token.content.size() == 1 && token.content[0] == '\"')){
        syntaxerror = true;
        errortoken = token;
        errortype = 3;
        return nullptr;        
    }

    return new TreeNode(token.content, token.type);
}

TreeNode* checkpri(TreeNode* node){
    if(isreserved(node->content) || functionalias.count(node->content) ){
        TreeNode* temp = copy(node);
        string pro = "#<procedure ";
        if(isreserved(node->content))
            pro = pro + node->content +">";
        else
            pro = pro + functionalias[node->content] +">";
        
        temp->content = pro;
        return temp;
    }    

    return nullptr;
}

string restorename(const string& str){
    if(str.size() > 13 && str.substr(0, 12) == "#<procedure " && str.back() == '>')
        return str.substr(12, str.size() - 13);
    
    return str;
}

bool isprocedure(const string& str){
    string temp = str;
    return temp != restorename(str);
}

void print(TreeNode* root, int& lprint){
    if(root == nullptr) return;
    if(root->type == nodetype::ATOM){
        for (int i = 0; i < lprint && !after; i++) cout << "  ";
        if(root->atomtype == tokentype::FLOAT)
            cout << roundto(root->content) << endl;
        else
            cout << root->content << endl;
        after = false;
    }
    
    else if(root->type == nodetype::NIL){
        lprint--;
        for (int i = 0; i < lprint && !after; i++) cout << "  ";
        cout << ')' << endl;
        after = false;
    }
    
    else if(root->type == nodetype::CONS){
        if(root->content == "("){
            for (int i = 0; i < lprint && !after; i++) cout << "  ";
            cout << "( ";
            lprint++;
            after = true;
        }

        print(root->left, lprint);
        if((root->left->type == nodetype::ATOM && root->right->type == nodetype::ATOM) || (root->left->content == "(" && root->right->type == nodetype::ATOM)){
            for (int i = 0; i < lprint && !after; i++) cout << "  ";
            cout << "." << endl;
        }
        
        print(root->right, lprint);
        
        if((root->left->type == nodetype::ATOM && root->right->type == nodetype::ATOM) ||(root->left->content == "(" && root->right->type == nodetype::ATOM)){
            lprint--;
            for (int i = 0; i < lprint && !after; i++) cout << "  ";
            cout << ")" << endl;
            after = false;
        }
    }    
}

TreeNode* makelist(vector<TreeNode*>& elems){
    if(elems.empty()){
        return falsenode();
    }

    TreeNode* head = new TreeNode("(", elems[0], nullptr);
    TreeNode* current = head;

    for(int i = 1; i < elems.size(); i++){
        TreeNode* next = new TreeNode("", elems[i], nullptr);
        current->right = next;
        current = next;
    }

    current->right = new TreeNode(nodetype::NIL);

    return head;
}

Numbertype stringtrans(TreeNode* node){
    if(node->atomtype == tokentype::INT){
        return { false, 0, std::stoi(node->content) };
    }
    
    else if(node->atomtype == tokentype::FLOAT){
        return { true, std::stof(node->content), 0 };
    }
    
    return { false, 0, 0 };
}

TreeNode* makenumnode(bool isFloat, float floatVal, int intVal){
    if(isFloat)
        return new TreeNode(to_string(floatVal), tokentype::FLOAT);
    else
        return new TreeNode(to_string(intVal), tokentype::INT);
}

void nonlist(TreeNode* node, int error){
    TreeNode* cur = node;
    while(cur->type != nodetype::NIL && cur->right != nullptr){
        cur = cur->right;
    }
    
    if(cur->type == nodetype::ATOM){
        errortype = 4;
        evalerrortoken = copy(node);    
    }

    else errortype = error;
}

bool ispredicates(const string& op){
    return op == "#<procedure atom?>" || op == "#<procedure pair?>" || op == "#<procedure list?>"
        || op == "#<procedure null?>" || op == "#<procedure integer?>" || op == "#<procedure real?>" || op == "#<procedure number?>"
        || op == "#<procedure string?>" || op == "#<procedure boolean?>" || op == "#<procedure symbol?>" ;
}

bool isarithmetic(const string& op){
    return op == "#<procedure +>" || op == "#<procedure ->" || op == "#<procedure *>" || op == "#<procedure />"; 
}

bool islogic(const string& op){
    return op == "#<procedure not>" || op == "#<procedure and>" || op == "#<procedure or>";
}

bool isstring(const string& op){
    return op == "#<procedure string-append>" || op == "#<procedure string>?>" || op == "#<procedure string<?>" || op == "#<procedure string=?>";
}

bool iscompare(const string& op){
    return  op == "#<procedure =>" || op == "#<procedure >>" || op == "#<procedure >=>" || op == "#<procedure <>" || op == "#<procedure <=>";
}


TreeNode* atom(TreeNode* node){
    if(node->atomtype == tokentype::SYMBOL || node->atomtype == tokentype::QUOTE || node->atomtype == tokentype::ATOM){
        if(localtable.find(node->content) != localtable.end()){
            return localtable[node->content];
        }
        
        if(definetable.find(node->content) != definetable.end()){
            return definetable[node->content];
        }

        else if(checkpri(node) != nullptr){
            return checkpri(node);            
        }

        else{
            errortype = 1;
            errorop = node->content;
            evalerror = true;
            return nullptr;
        }
    }
        
    else return node;
}

TreeNode* quote(TreeNode* node){
    if(node->right->type == nodetype::NIL || node->right->right->type != nodetype::NIL){
        errortype = 2;
        errorop = "quote";
        evalerror = true;
        toplevel--;
        return nullptr;
    }
        
    if(node->right->left->type == nodetype::ATOM && node->right->left->atomtype == tokentype::SYMBOL){
        node->right->left->atomtype = tokentype::ATOM;
    }

    toplevel--;
    return node->right->left;
}

TreeNode* define(TreeNode* node){
    if(node->right->type == nodetype::NIL || node->right->right->type == nodetype::NIL){
        errortype = 3;
        evalerror = true;
        evalerrortoken = copy(node);
        toplevel--;
        return nullptr;
    }

    TreeNode* target = node->right->left;
    if(target->type == nodetype::CONS){
        TreeNode* function = target->left;
        if(function->atomtype != tokentype::SYMBOL || isreserved(function->content)){
            errortype = 3;
            evalerror = true;
            evalerrortoken = copy(node);
            toplevel--;
            return nullptr;
        }

        vector<string> parameters = {};
        TreeNode* paranode = target->right;
        while(paranode->type == nodetype::CONS){
            if(paranode->left->atomtype != tokentype::SYMBOL){
                errortype = 3;
                evalerror = true;
                evalerrortoken = copy(node);
                toplevel--;
                return nullptr;
            }
            parameters.push_back(paranode->left->content);
            paranode = paranode->right;
        }
        if(paranode->type != nodetype::NIL){
            errortype = 3;
            evalerror = true;
            evalerrortoken = copy(node);
            toplevel--;
            return nullptr;
        }

        TreeNode* bodylist = node->right->right;
        TreeNode* beginnode = new TreeNode("begin", tokentype::SYMBOL);
        TreeNode* beginexpr = new TreeNode("(", beginnode, bodylist);

        TreeNode* fn = new TreeNode("#<procedure " + function->content + ">", tokentype::SYMBOL);
        definetable[function->content] = fn;
        lambdatable[fn] = new UserFunction(parameters, beginexpr);


        if(verbose) cout << endl << "> " << function->content << " defined" << endl;
        needprint = false;
        toplevel--;
        return target;;
    }

    if(node->right->right->right->type != nodetype::NIL){
        errortype = 3;
        evalerror = true;
        evalerrortoken = copy(node);
        toplevel--;
        return nullptr;
    }
        
    TreeNode* name = node->right->left;
    if(name->atomtype != tokentype::SYMBOL || isreserved(name->content)){
        errortype = 3;
        evalerror = true;
        evalerrortoken = copy(node);
        toplevel--;
        return nullptr;            
    }
        
    TreeNode* val = eval(node->right->right->left);
    if(evalerror){
        if(errortype == 7) errortype = 6;
        toplevel--;
        return nullptr;
    }
    if(node->right->right->left->left != nullptr && node->right->right->left->left->atomtype == tokentype::QUOTE){
        definetable[name->content] = val;
    }
    else if(isreserved(val->content))
        functionalias[name->content] = val->content;
    else
        definetable[name->content] = val;

    if(verbose) cout << endl << "> " << name->content << " defined" << endl;
    needprint = false;
    toplevel--;
    return name;    
}

TreeNode* cons(TreeNode* node){
    if(node->right->type == nodetype::NIL || node->right->right->type == nodetype::NIL || node->right->right->right->type != nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = "cons";
        toplevel--;
        return nullptr;
    }
    
    TreeNode* left = eval(node->right->left);
    if(evalerror){
        if(errortype == 6) errortype = 7;
        toplevel--;
        return nullptr;
    } 
    TreeNode* right = eval(node->right->right->left);
    if(evalerror){
        if(errortype == 6) errortype = 7;
        toplevel--;
        return nullptr;       
    } 
    
    toplevel--;
    if(right->atomtype == tokentype::NIL)
        right = new TreeNode(nodetype::NIL);
    else if(right->content == "("){
        TreeNode* temp = copy(right);
        temp->content = "";
        return new TreeNode("(", left, temp);
    }

    return new TreeNode("(", left, right);
}

TreeNode* list(TreeNode* node){
    if(node->right->type == nodetype::NIL){
        toplevel--;
        return falsenode();
    }

    TreeNode* result = new TreeNode(nodetype::NIL);
    TreeNode** tail = &result;
    

    TreeNode* evaled = eval(node->right->left);
    if(evalerror){
        if(errortype == 6) errortype = 7;
        toplevel--;
        return nullptr;
    } 
    *tail = new TreeNode("(", evaled, new TreeNode(nodetype::NIL));

    tail = &((*tail)->right);
    node = node->right->right;
    while(node->type != nodetype::NIL){
        TreeNode* evaled = eval(node->left);
        if(evalerror){
            if(errortype == 6) errortype = 7;
            toplevel--;
            return nullptr;
        }

        *tail = new TreeNode("", evaled, new TreeNode(nodetype::NIL));
        tail = &((*tail)->right);
        node = node->right;
    }

    toplevel--;
    return result;
}

TreeNode* carcdr(const string& op, TreeNode* node){
    if(node->right->type == nodetype::NIL || node->right->right->type != nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = restorename(op);
        toplevel--;
        return nullptr;
    }

    TreeNode* target = eval(node->right->left);
    if(evalerror){
        if(errortype == 6) errortype = 7;
        toplevel--;
        return nullptr;        
    } 
    if(target->type != nodetype::CONS){
        errorop = restorename(op);
        errortype = 5;
        evalerrortoken = copy(target);
        evalerror = true;
        toplevel--;
        return nullptr;
    }

    if(op == "car" || op == "#<procedure car>"){
        if(target->left->type == nodetype::ATOM && target->left->atomtype == tokentype::SYMBOL){
            TreeNode* temp = copy(target->left);
            temp->atomtype = tokentype::ATOM;
            toplevel--;
            return temp;
        }

        toplevel--;
        return target->left;
    }

    else{ 
        TreeNode* cdrresult = target->right;
        toplevel--;
        if(cdrresult->type == nodetype::NIL)
            return new TreeNode("nil", tokentype::NIL);
        else if(cdrresult->type == nodetype::CONS) 
            return new TreeNode("(", cdrresult->left, cdrresult->right);
        else 
            return cdrresult;
    }
}

TreeNode* begin(TreeNode* node){
    if(node->right->type == nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = "begin";
        toplevel--;
        return nullptr;
    }

    TreeNode* cur = node->right;
    TreeNode* result = nullptr;

    while(cur->type != nodetype::NIL){
        result = eval(cur->left);
        if(evalerror && errortype != 6) return nullptr;
        evalerror = false;
        cur = cur->right;
    }

    if(result == nullptr){
        evalerror = true;
        evalerrortoken = copy(node);
        errortype = 6;
    }

    if(result != nullptr) evalerror = false;
    toplevel--;
    return result;
}

TreeNode* predicates(const string& op, TreeNode* node){
    if(node->right->type == nodetype::NIL || node->right->right->type != nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = restorename(op);
        return nullptr;
    }

    TreeNode* target = eval(node->right->left);
    toplevel--;
    if(evalerror){
        if(errortype == 6) errortype = 7;
        return nullptr;        
    } 

    if(op == "atom?" || op == "#<procedure atom?>"){
        if((target->type == nodetype::ATOM  && target->atomtype != tokentype::SYMBOL ) || target->type == nodetype::NIL || isreserved(node->right->left->content))
            return truenode();
        else
            return falsenode();
    }

    if(op == "pair?" || op == "#<procedure pair?>"){
        if(target->type == nodetype::CONS)
            return truenode();
        else
            return falsenode();
    }

    if(op == "list?" || op == "#<procedure list?>"){
        TreeNode* cur = target;
        while(cur != nullptr && cur->type == nodetype::CONS){
            cur = cur->right;
        }

        if((cur != nullptr && cur->type == nodetype::NIL) || (cur != nullptr && cur->atomtype == tokentype::NIL))
            return truenode();
        else
            return falsenode();
    }

    if(op == "null?" || op == "#<procedure null?>"){
        if(target->atomtype == tokentype::NIL)
            return truenode();
        else
            return falsenode();
    }

    if(op == "integer?" || op == "#<procedure integer?>"){
        if(target->atomtype == tokentype::INT)
            return truenode();
        else
            return falsenode();
    }

    if(op == "real?" || op == "#<procedure real?>"){
        if(target->atomtype == tokentype::INT || target->atomtype == tokentype::FLOAT)
            return truenode();
        else
            return falsenode();
    }

    if(op == "number?" || op == "#<procedure number?>"){
        if(target->atomtype == tokentype::INT || target->atomtype == tokentype::FLOAT)
            return truenode();
        else
            return falsenode();
    }

    if(op == "string?" || op == "#<procedure string?>"){
        if(target->atomtype == tokentype::STRING)
            return truenode();
        else
            return falsenode();
    }

    if(op == "boolean?" || op == "#<procedure boolean?>"){
        if(target->atomtype == tokentype::T || target->atomtype == tokentype::NIL)
            return truenode();
        else
            return falsenode();
    }

    if(op == "symbol?" || op == "#<procedure symbol?>"){
        if((target->atomtype == tokentype::SYMBOL || target->atomtype == tokentype::ATOM) && !isreserved(node->right->left->content))
            return truenode();
        else
            return falsenode();
    }

    toplevel--;
    return falsenode();  
}

TreeNode* arithmetic(const string& op, TreeNode* node){
    if(node->right->type == nodetype::NIL || node->right->right->type == nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = restorename(op);
        toplevel--;
        return nullptr;
    }

    int lprint = 0;
    bool usefloat = false;
    bool firstnum = true;
    float floatresult = (op == "*" || op == "/" || op == "#<procedure *>" || op == "#<procedure />") ? 1.0f : 0.0f;
    int intresult = (op == "*" || op == "/" || op == "#<procedure *>" || op == "#<procedure />") ? 1 : 0;
    for(TreeNode* cur = node->right; cur->type != nodetype::NIL; cur = cur->right){
        TreeNode* left = eval(cur->left);
        if(evalerror){
            if(errortype == 6) errortype = 7;
            toplevel--;
            return nullptr;             
        }
        
        if(left->atomtype != tokentype::FLOAT && left->atomtype != tokentype::INT){
            errorop = restorename(op);
            errortype = 5;
            evalerrortoken = copy(left);
            evalerror = true;
            toplevel--;
            return nullptr;
        }
        
        Numbertype num = stringtrans(left);
        if(num.isFloat) usefloat = true;
        float value = num.isFloat ? num.floatValue : (float)num.intValue;

        if(firstnum){
            floatresult = value;
            intresult = num.isFloat ? (int)value : num.intValue;
            firstnum = false;
        }
        
        else{
            if(op == "+" || op == "#<procedure +>"){
                floatresult += value;
                intresult += num.isFloat ? (int)value : num.intValue;
            }
            
            else if(op == "-" || op == "#<procedure ->"){
                floatresult -= value;
                intresult -= num.isFloat ? (int)value : num.intValue;
            }
            else if(op == "*" || op == "#<procedure *>"){
                floatresult *= value;
                intresult *= num.isFloat ? (int)value : num.intValue;
            }
            else if(op == "/" || op == "#<procedure />"){
                if(value == 0 && num.intValue == 0){
                    evalerror = true;
                    cout << endl << "> ERROR (division by zero) : /" << endl;
                    toplevel--;
                    return nullptr;
                }
                
                if(usefloat)
                    floatresult /= value;
                else
                    intresult /= num.isFloat ? (int)value : num.intValue;
            }
        }
    }

    toplevel--;
    if(usefloat)
        return makenumnode(true, floatresult, 0);
    else
        return makenumnode(false, 0.0f, intresult);
}

TreeNode* logic(const string& op, TreeNode* node){
    if(node->right->type == nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = restorename(op);
        toplevel--;
        return nullptr;
    }
    
    if(op == "not" || op == "#<procedure not>"){
        if(node->right->right->type != nodetype::NIL){
            errortype = 2;
            evalerror = true;
            errorop = restorename(op);
            toplevel--;
            return nullptr;
        }

        TreeNode* target = eval(node->right->left);
        if(evalerror){
            if(errortype == 6) errortype = 7;
            toplevel--;
            return nullptr;             
        } 

        toplevel--;
        if(target->atomtype == tokentype::NIL)
            return truenode();
        else
            return falsenode();
    }

    if(op == "and" || op == "#<procedure and>"){
        if(node->right->right->type == nodetype::NIL){
            errortype = 2;
            evalerror = true;
            errorop = restorename(op);
            toplevel--;
            return nullptr;
        }

        TreeNode* cur = node->right;
        TreeNode* result = nullptr;
        while(cur->type != nodetype::NIL){
            result = eval(cur->left);
            if(evalerror){
                if(errortype == 6) errortype = 9;
                toplevel--;
                return nullptr;         
            }

            if(result->atomtype == tokentype::NIL){
                toplevel--;
                return falsenode();
            }

            cur = cur->right;
        }

        toplevel--;
        return result == nullptr ? truenode() : result;
    }

    if(op == "or" || op == "#<procedure or>"){
        if(node->right->right->type == nodetype::NIL){
            errortype = 2;
            evalerror = true;
            errorop = restorename(op);
            toplevel--;
            return nullptr;
        }

        TreeNode* cur = node->right;
        while(cur->type != nodetype::NIL){
            TreeNode* result = eval(cur->left);
            if(evalerror){
                if(errortype == 6) errortype = 9;
                toplevel--;
                return nullptr;         
            }

            if(result->content != "nil" && result->content != "#f"){
                toplevel--;
                return result;
            }

            cur = cur->right;
        }
        
        toplevel--;
        return falsenode();
    }

    toplevel--;
    return nullptr;
}

TreeNode* evalstring(const string& op, TreeNode* node){
    TreeNode* ans = truenode();
    if(node->right->type == nodetype::NIL || node->right->right->type == nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = restorename(op);
        toplevel--;
        return nullptr;
    }

    if(op == "string-append" || op == "#<procedure string-append>"){
        string result = "";
        TreeNode* cur = node->right;
        while(cur->type != nodetype::NIL) {
            TreeNode* target = eval(cur->left);
            if(evalerror){
                if(errortype == 6) errortype = 7;
                toplevel--;
                return nullptr;         
            }

            if(target->atomtype != tokentype::STRING){
                errorop = restorename(op);
                errortype = 5;
                evalerrortoken = copy(target);
                evalerror = true;
                toplevel--;
                return nullptr;
            }

            result += target->content.substr(1, target->content.length() - 2);  
            cur = cur->right;
        }

        toplevel--;
        return new TreeNode("\"" + result + "\"", tokentype::STRING);
    }

    else{
        TreeNode* cur = node->right;
        TreeNode* prev = eval(cur->left);
        if(evalerror){
            if(errortype == 6) errortype = 7;
            toplevel--;
            return nullptr;         
        }
        if(prev->atomtype != tokentype::STRING){
            errorop = restorename(op);
            errortype = 5;
            evalerrortoken = copy(prev);
            toplevel--;
            evalerror = true;
            return nullptr;
        }

        cur = cur->right;        
        while(cur->type != nodetype::NIL){
            TreeNode* next = eval(cur->left);
            if(evalerror){
                if(errortype == 6) errortype = 7;
                toplevel--;
                return nullptr;         
            }
            if(next->atomtype != tokentype::STRING){
                errorop = restorename(op);
                errortype = 5;
                evalerrortoken = copy(next);
                toplevel--;
                evalerror = true;
                return nullptr;
            }

            if(op == "string>?" || op == "#<procedure string>?>"){
                if(prev->content.substr(1, prev->content.length() - 2) <= next->content.substr(1, next->content.length() - 2))
                    ans = falsenode();
            }

            else if(op == "string<?" || op == "#<procedure string<?>"){
                if(prev->content.substr(1, prev->content.length() - 2) >= next->content.substr(1, next->content.length() - 2))
                    ans = falsenode();                
            }

            else{
                if(prev->content.substr(1, prev->content.length() - 2) != next->content.substr(1, next->content.length() - 2))
                    ans = falsenode();
            }

            prev = next;
            cur = cur->right;
        }

        toplevel--;
        return ans;
    }

    evalerror = true;
    cout << endl << "> ERROR (unknown string operator) : " << op << endl;
    toplevel--;
    return nullptr;
}

TreeNode* compare(const string& op, TreeNode* node){
    TreeNode* ans = truenode();
    if(node->right->type == nodetype::NIL || node->right->right->type == nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = restorename(op);
        toplevel--;
        return nullptr;
    }

    TreeNode* cur = node->right;
    TreeNode* prevNode = eval(cur->left);
    if(evalerror){
        if(errortype == 6) errortype = 7;
        toplevel--;
        return nullptr;         
    }
    if(prevNode->atomtype != tokentype::INT && prevNode->atomtype != tokentype::FLOAT){
        errorop = restorename(op);
        errortype = 5;
        evalerrortoken = copy(prevNode);

        evalerror = true;
        toplevel--;
        return nullptr;
    }

    Numbertype prevNum = stringtrans(prevNode);
    cur = cur->right;

    while(cur->type != nodetype::NIL){
        TreeNode* nextNode = eval(cur->left);
        if(evalerror){
            if(errortype == 6) errortype = 7;
            toplevel--;
            return nullptr;         
        }
        if(nextNode->atomtype != tokentype::INT && nextNode->atomtype != tokentype::FLOAT){
            errorop = restorename(op);
            errortype = 5;
            evalerrortoken = copy(nextNode);

            evalerror = true;
            toplevel--;
            return nullptr;
        }

        Numbertype nextNum = stringtrans(nextNode);

        float prevVal = prevNum.isFloat ? prevNum.floatValue : static_cast<float>(prevNum.intValue);
        float nextVal = nextNum.isFloat ? nextNum.floatValue : static_cast<float>(nextNum.intValue);

        if(op == "<" || op == "#<procedure <>"){
            if(!(prevVal < nextVal)) ans = falsenode();
        }
        
        else if(op == "<=" || op == "#<procedure <=>"){
            if(!(prevVal <= nextVal)) ans = falsenode();
        }
        
        else if(op == "=" || op == "#<procedure =>"){
            if(prevVal != nextVal) ans = falsenode();
        }
        
        else if(op == ">" || op == "#<procedure >>"){
            if(!(prevVal > nextVal)) ans = falsenode();
        }
        
        else if(op == ">=" || op == "#<procedure >=>"){
            if(!(prevVal >= nextVal)) ans = falsenode();
        }
        
        prevNum = nextNum;
        cur = cur->right;
    }

    toplevel--;
    return ans;
}

TreeNode* eqv(TreeNode* node){
    if(node->right->type == nodetype::NIL || node->right->right->type == nodetype::NIL || node->right->right->right->type != nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = "eqv?";
        toplevel--;
        return nullptr;
    }

    TreeNode* left = eval(node->right->left);
    if(evalerror){
        if(errortype == 6) errortype = 7;
        toplevel--;
        return nullptr;         
    }
    TreeNode* right = eval(node->right->right->left);
    if(evalerror){
        if(errortype == 6) errortype = 7;
        toplevel--;
        return nullptr;         
    }

    bool result = false;

    if(left->type != right->type){
        result = false;
    }
    
    else if(left->type == nodetype::ATOM && left->atomtype != tokentype::STRING){
        result = (left->atomtype == right->atomtype) &&
                 (left->content == right->content);
    }
    
    else{
        result = (left == right);
    }

    toplevel--;
    return result ? truenode() : falsenode();
}

bool equalrec(TreeNode* a, TreeNode* b){
    if(a->type != b->type) return false;

    if(a->type == nodetype::ATOM){
        return (a->atomtype == b->atomtype) && (a->content == b->content);
    }

    if(a->type == nodetype::NIL && b->type == nodetype::NIL){
        return true;
    }

    return equalrec(a->left, b->left) && equalrec(a->right, b->right);
}

TreeNode* equal(TreeNode* node){
    evalerrortoken = copy(node);
    if(node->right->type == nodetype::NIL || node->right->right->type == nodetype::NIL || node->right->right->right->type != nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = "equal?";   
        toplevel--;
        return nullptr;
    }

    TreeNode* left = eval(node->right->left);
    if(evalerror){
        if(errortype == 6) errortype = 7;
        toplevel--;
        return nullptr;         
    }
    TreeNode* right = eval(node->right->right->left);
    if(evalerror){
        if(errortype == 6) errortype = 7;
        toplevel--;
        return nullptr;         
    }

    bool result = equalrec(left, right);

    toplevel--;
    return result ? truenode() : falsenode();
}

TreeNode* evalif(TreeNode* node){  
    int lprint = 0, para = 0;
    TreeNode* temp = node;
    while(temp->type != nodetype::ATOM && temp->type != nodetype::NIL){
        para++;
        temp = temp->right;
    }
    
    TreeNode* result = nullptr;
    if(node->right->type == nodetype::NIL || node->right->right->type == nodetype::NIL || para > 4 || para < 3){
        evalerror = true;
        errortype = 2;
        errorop = "if";
        toplevel--;
        return nullptr;
    }

    TreeNode* test = eval(node->right->left);
    if(evalerror){
        if(errortype == 6) errortype = 8;
        toplevel--;
        return nullptr;         
    }

    if(test->atomtype != tokentype::NIL){
        result = eval(node->right->right->left);
        if(evalerror) return nullptr;
        toplevel--;
        return result;
    } 
    else if(node->right->right->right->type != nodetype::NIL){
        toplevel--;
        return eval(node->right->right->right->left);
    }
    else{
        errortype = 6;
        evalerrortoken = copy(node);
        evalerror = true;    
    }

    toplevel--;
    return nullptr;

}

TreeNode* condition(TreeNode* node){
    int lprint = 0;

    if(node->right->type == nodetype::NIL){
        evalerror = true;
        errortype = 3;
        evalerrortoken = copy(node);
        toplevel--;
        return nullptr;
    }

    TreeNode* clause = node->right;

    for(TreeNode* temp = clause; temp->type != nodetype::NIL; temp = temp->right){
        TreeNode* currentclause = temp->left;

        if(currentclause->type != nodetype::CONS){
            evalerror = true;
            errortype = 3;
            evalerrortoken = copy(node);
            toplevel--;
            return nullptr;
        }

        if(currentclause->right->type == nodetype::NIL || currentclause->right->type == nodetype::ATOM){
            evalerror = true;
            errortype = 3;
            evalerrortoken = copy(node);
            toplevel--;
            return nullptr;
        }

        nonlist(currentclause, 0);
        if(errortype == 4){
            evalerrortoken = copy(node);
            evalerror = true;
            toplevel--;
            return nullptr;
        }
    }

    TreeNode* temp = clause;
    int totalclauses = 0;

    for(TreeNode* t = clause; t->type != nodetype::NIL; t = t->right)
        totalclauses++;

    int currentindex = 0;

    while(temp->type != nodetype::NIL){
        currentindex++;
        TreeNode* currentclause = temp->left;

        TreeNode* test = currentclause->left;
        bool islast = (currentindex == totalclauses);
        bool iselse = (test->type == nodetype::ATOM && test->atomtype == tokentype::SYMBOL && test->content == "else");

        if(iselse && islast){
            TreeNode* actionlist = currentclause->right;
            while(actionlist->right->type != nodetype::NIL){
                TreeNode* dummy = eval(actionlist->left);
                if(evalerror && errortype != 6){
                    toplevel--;
                    return nullptr;
                } 
                evalerror = false;
                actionlist = actionlist->right;
            }

            TreeNode* result = eval(actionlist->left);
            if(result != nullptr) evalerror = false;
            toplevel--;
            return result;
        }

        TreeNode* testresult = eval(test);
        if(evalerror){
            if(errortype == 6) errortype = 8;
            toplevel--;
            return nullptr;
        } 

        if(testresult->atomtype != tokentype::NIL){
            TreeNode* actionlist = currentclause->right;
            while(actionlist->right->type != nodetype::NIL){
                TreeNode* dummy = eval(actionlist->left);
                if(evalerror && errortype != 6){
                    toplevel--;
                    return nullptr;
                }
                evalerror = false;
                actionlist = actionlist->right;
            }

            TreeNode* result = eval(actionlist->left);
            if(result != nullptr) evalerror = false;
            toplevel--;
            return result;
        }

        temp = temp->right;
    }

    evalerror = true;
    errortype = 6;
    evalerrortoken = copy(node);
    toplevel--;
    return nullptr;
}

TreeNode* userfunc(TreeNode* node, bool islet = false){
    int argsize = 0;
    TreeNode* func = eval(node->left);
    TreeNode* temp = copy(node);
    temp->left = func; 
    UserFunction* fn = lambdatable[temp->left];
    vector<string>& para = fn->parameters;
    TreeNode* arg = temp->right;

    vector<TreeNode*> arglist;
    TreeNode* walker = arg;
    TreeNode* count = arg;
    while(count->type == nodetype::CONS){
        argsize++;
        count = count->right;
    }

    if(argsize != para.size()){
        errortype = 2;
        evalerror = true;
        errorop = restorename(temp->left->content);
        toplevel--;
        return nullptr;
    }

    while(walker->type == nodetype::CONS){
        TreeNode* val = eval(walker->left);
        if(evalerror){
            if(errortype == 6 && !islet) errortype = 7;
            if(errortype == 6 && islet){
                errortype = 10;
                evalerrortoken = copy(walker->left);
            }
            toplevel--;
            return nullptr;
        }
        arglist.push_back(val);
        walker = walker->right;
    }

    map<string, TreeNode*> origintable = localtable;
    if(!islet) localtable.clear();
    for(int i = 0; i < para.size(); ++i)
        localtable[para[i]] = arglist[i];

    TreeNode* result = eval(fn->body); 
    if(evalerror && errortype == 6){
        evalerrortoken = copy(node);
        evalerrortoken->left->content = restorename(evalerrortoken->left->content);
    } 

    localtable = origintable;

    if(result != nullptr) evalerror = false;
    toplevel--;
    return result;
}

TreeNode* lambda(TreeNode* node){
    if(node->right->type == nodetype::NIL || node->right->right->type == nodetype::NIL){
        errortype = 3;
        evalerror = true;
        evalerrortoken = copy(node);
        toplevel--;
        return nullptr;
    }

    TreeNode* arglist = node->right->left;
    TreeNode* bodylist = node->right->right;

    vector<string> para;
    TreeNode* cur = arglist;
    while(cur->type == nodetype::CONS){
        if(cur->left->atomtype != tokentype::SYMBOL || isreserved(cur->left->content)){
            errortype = 3;
            evalerror = true;
            evalerrortoken = copy(node);
            toplevel--;
            return nullptr;
        }
        para.push_back(cur->left->content);
        cur = cur->right;
    }
    if(cur->type != nodetype::NIL && cur->atomtype != tokentype::NIL){
        errortype = 3;
        evalerror = true;
        evalerrortoken = copy(node);
        toplevel--;
        return nullptr;
    }

    if(bodylist->type != nodetype::CONS){
        errortype = 3;
        evalerror = true;
        evalerrortoken = copy(node);
        toplevel--;
        return nullptr;
    }

    TreeNode* beginnode = new TreeNode("begin", tokentype::SYMBOL);
    TreeNode* beginexpr = new TreeNode("(", beginnode, bodylist);

    TreeNode* lambdalabel = new TreeNode("#<procedure lambda>", tokentype::SYMBOL);
    lambdatable[lambdalabel] = new UserFunction(para, beginexpr);

    toplevel--;
    return lambdalabel;
}

TreeNode* let(TreeNode* node){
    if(node->right->type == nodetype::NIL || node->right->right->type == nodetype::NIL){
        errortype = 3;
        evalerror = true;
        evalerrortoken = copy(node);
        toplevel--;
        return nullptr;
    }

    TreeNode* bindings = node->right->left; 
    TreeNode* body = node->right->right;    

    vector<TreeNode*> paranames;
    vector<TreeNode*> args;

    TreeNode* cur = bindings;
    while(cur->type == nodetype::CONS){
        TreeNode* pair = cur->left;

        if(pair->type != nodetype::CONS || pair->right->type != nodetype::CONS || pair->right->right->type != nodetype::NIL){
            errortype = 3;
            evalerror = true;
            evalerrortoken = copy(node);
            toplevel--;
            return nullptr;
        }

        TreeNode* var = pair->left;
        TreeNode* val = pair->right->left;

        if(var->atomtype != tokentype::SYMBOL || isreserved(var->content)){
            errortype = 3;
            evalerror = true;
            evalerrortoken = copy(node);
            toplevel--;
            return nullptr;
        }

        paranames.push_back(var);
        args.push_back(val);

        cur = cur->right;
    }

    if(cur->type != nodetype::NIL && cur->atomtype != tokentype::NIL){
        errortype = 3;
        evalerror = true;
        evalerrortoken = copy(node);
        toplevel--;
        return nullptr;
    }

    TreeNode* paralist = makelist(paranames);
    TreeNode* arglist = makelist(args);

    TreeNode* beginnode = new TreeNode("begin", tokentype::SYMBOL);
    TreeNode* beginexpr = new TreeNode("(", beginnode, body);

    TreeNode* lambdanode = new TreeNode("lambda", tokentype::SYMBOL);
    TreeNode* lambdaexpr = new TreeNode("(", lambdanode, new TreeNode("(", paralist, beginexpr));

    TreeNode* letexpr = new TreeNode("(", lambdaexpr, arglist);

    TreeNode* result = eval(letexpr, true);
    if(evalerror && errortype == 6){
        evalerrortoken = copy(node);  
    }

    toplevel--;
    return result;
}

TreeNode* exit(TreeNode* node){
    if(node->right->type != nodetype::NIL){
        errortype = 2;
        evalerror = true;
        errorop = "exit";
        return nullptr;
    }

    return exitnode();
}

void clear(){
    for(auto& pair : definetable){
        if(pair.second != nullptr){
            //delete pair.second;
            pair.second = nullptr;
        }
    }

    for(auto& pair : localtable){
        if(pair.second != nullptr){
            //delete pair.second;
            pair.second = nullptr;
        }
    }

    for(auto& pair : lambdatable){
        if(pair.second != nullptr){
            //delete pair.second;
            pair.second = nullptr;
        }
    }
    definetable.clear();
    localtable.clear();
    functionalias.clear();
    lambdatable.clear();
}

TreeNode* eval(TreeNode* node, bool islet ){
    if(node == nullptr) return nullptr;

    if(node->type == nodetype::ATOM) return atom(node);

    if(node->type == nodetype::CONS && node->left->atomtype == tokentype::SYMBOL && node->left->content == "lambda") {
        toplevel++;
        return lambda(node);
    }

    if(node->type == nodetype::CONS){
        toplevel++;
        if(node->left->type == nodetype::CONS && node->left->left->atomtype == tokentype::SYMBOL && node->left->left->content == "lambda") {
            TreeNode* temp = copy(node);
            TreeNode* built = lambda(node->left);
            if(evalerror) return nullptr;
            //node->left = built;
            TreeNode* result = userfunc(node, islet);
            if(evalerror && errortype == 6) evalerrortoken = copy(temp);
            return result; 
        }

        TreeNode* funcNode = eval(node->left);
        if(evalerror){
            if(errortype == 6) errortype = 10;
            return nullptr;
        }
        string op = funcNode->content;
        nonlist(node, 0);
        if(errortype == 4){
            evalerror = true;
            evalerrortoken = copy(node);
            errorop = restorename(op);
            return nullptr;
        }

        if(lambdatable.count(funcNode)){
            /*TreeNode* temp = copy(node);
            temp->left = funcNode;
            
            return userfunc(temp);*/
            return userfunc(node);
        }
        if(op == "#<procedure quote>"){
            
            return quote(node);
        }
        else if(op == "#<procedure define>"){
            if(toplevel > 1){
                cout << endl << "> ERROR (level of DEFINE)" << endl;
                evalerror = true;
                return nullptr;
            }
            
            return define(node);
        }
        else if(op == "#<procedure cons>"){
            
            return cons(node);
        } 
        else if(op == "#<procedure list>"){
            
            return list(node);
        }
        else if(op == "#<procedure car>" || op == "#<procedure cdr>"){
            
            return carcdr(op, node);
        }
        else if(op == "#<procedure begin>"){
            
            return begin(node);
        }
        else if(ispredicates(op)){
            
            return predicates(op, node);
        }
        else if(isarithmetic(op)){
            
            return arithmetic(op, node);
        } 
        else if(islogic(op)){
            
            return logic(op, node);
        } 
        else if(isstring(op)){
            return evalstring(op, node);
        }
        else if(iscompare(op)){
            return compare(op, node);
        }
        else if(op == "#<procedure eqv?>"){
            return eqv(node);
        }
        else if(op == "#<procedure equal?>"){
            return equal(node);
        } 
        else if(op == "#<procedure if>"){
            return evalif(node);
        }
        else if(op == "#<procedure cond>"){
            return condition(node);
        } 
        else if(op == "#<procedure lambda>"){
            /*TreeNode* temp = copy(node);
            temp->left = funcNode;*/
            return userfunc(node, islet);
        }
        else if(op == "#<procedure let>"){
            return let(node);
        }
        else if(op == "#<procedure verbose?>"){
            return verbose ? truenode() : falsenode();
        }        
        else if(op == "#<procedure verbose>"){
            if(node->right->type == nodetype::NIL || node->right->right->type != nodetype::NIL){
                errortype = 2;
                evalerror = true;
                errorop = restorename(op);
                return nullptr;
            }
            if(node->right->left->atomtype == tokentype::NIL){
                verbose = false;
                needprint = false;
                return falsenode();
            }
            
            else{
                TreeNode* result = eval(node->right->left);
                if(evalerror){
                    if(errortype == 6) errortype = 7;
                    return nullptr;
                } 
                verbose = true;
                return truenode();
            }
        }
        else if(op == "#<procedure exit>"){
            if(toplevel > 1){
                cout << endl << "> ERROR (level of EXIT)" << endl;
                evalerror = true;
                return nullptr;
            } 

            return exit(node);
        }
        else if(op == "#<procedure clean-environment>"){
            if(toplevel > 1){
                cout << endl << "> ERROR (level of CLEAN-ENVIRONMENT)" << endl;
                evalerror = true;
                return nullptr;
            }
            
            if(node->right->type == nodetype::NIL){
                clear();
                if(verbose) cout << endl << "> environment cleaned" << endl;
                needprint = false;
                return truenode();
            }
            
            else{
                cout << endl << "> ERROR (incorrect number of arguments) : clean-environment" << endl;
                evalerror = true;
                return nullptr;
            }
        }
        
        else{
            int l = 0;
            cout << endl << "> ERROR (attempt to apply non-function) : ";
            if(funcNode->type != nodetype::ATOM) print(funcNode, l);
            else{
                if(!isreserved(op) && funcNode->atomtype == tokentype::FLOAT) cout << roundto(op) << endl;
                else cout << op << endl;
            } 

            evalerror = true;
            return nullptr;
        }
    }

    return node;
}

void printsyntaxerror(){
    if(errortype == 3)   cout << endl << "> ERROR (no closing quote) : END-OF-LINE encountered at Line " << errortoken.row << " Column "<< errortoken.col << endl;
    else if(errortype == 2){
        cout << endl << "> ERROR (unexpected token) : atom or '(' expected when token at Line "<< errortoken.row << " Column " << errortoken.col << " is >>" << errortoken.content << "<<" << endl;
    }

    else cout << endl << "> ERROR (unexpected token) : ')' expected when token at Line "<< errortoken.row << " Column " << errortoken.col << " is >>" << errortoken.content << "<<" << endl;
}

void printevalerror(){
    int lprint = 0;
    if(errortype == 1)
        cout << endl << "> ERROR (unbound symbol) : " << errorop << endl; 
    else if(errortype == 2)
        cout << endl << "> ERROR (incorrect number of arguments) : " << errorop << endl;
    else if(errortype == 3){
        if(evalerrortoken->left->content == "define")
            cout << endl << "> ERROR (DEFINE format) : ";
        else if(evalerrortoken->left->content == "cond")
            cout << endl << "> ERROR (COND format) : ";
        else if(evalerrortoken->left->content == "lambda")
            cout << endl << "> ERROR (LAMBDA format) : ";
        else if(evalerrortoken->left->content == "let")
            cout << endl << "> ERROR (LET format) : ";
        print(evalerrortoken, lprint);
    }        
    
    else if(errortype == 4){
        cout << endl << "> ERROR (non-list) : ";
        print(evalerrortoken, lprint);
    }
    else if(errortype == 5){
        cout << endl << "> ERROR (" << errorop << " with incorrect argument type) : ";
        print(evalerrortoken, lprint);
    }
    else if(errortype == 6 || errortype == 10){
        cout << endl << "> ERROR (no return value) : ";
        print(evalerrortoken, lprint);
    }
    else if(errortype == 7){
        cout << endl << "> ERROR (unbound parameter) : ";
        print(evalerrortoken, lprint);        
    }
    else if(errortype == 8){
        cout << endl << "> ERROR (unbound test-condition) : ";
        print(evalerrortoken, lprint);        
    }
    else if(errortype == 9){
        cout << endl << "> ERROR (unbound condition) : ";
        print(evalerrortoken, lprint);        
    }
}

int main(){
    string question ;
    cout << "Welcome to OurScheme!" << endl;
    getline(cin, question);
    string inp;    
    int index = 0, start = index, lprint = 0;
    bool already = false;
    TreeNode* root = nullptr;
    do{
        reset();
        already = false;
        root = nullptr;
        if(!readinput())break;
        lprint = 0;
        index = 0; 
        while(index < tokens.size()){
            existtree = true;
            evalerror = false;
            root = parse(tokens, index);
            if(syntaxerror || checkexit(root) || eof){
                index = tokens.size();
            }   
            
            else{
                root = eval(root);
                //if(checkexit(root)) break;

                if(evalerror){
                    printevalerror();
                    already = true;
                    root = nullptr;   
                    toplevel = 0; 
                    continue;
                }

                else{
                    if(checkexit(root)) break;
                    if(needprint){
                        cout << endl << "> " ;
                        print(root, lprint);                        
                    }

                    errorop = "";
                    needprint = true;
                    existtree = false;
                    root = nullptr;        
                    toplevel = 0;
                }
            }
        }

        if(checkexit(root) || ( tokens.size() >= 3 && tokens[0].content == "(" && tokens[1].content == "exit" && tokens[2].content == ")") ){
            cout << endl << "> ";
            break;
        }

        else if(syntaxerror){
            printsyntaxerror();
        }

        else if(evalerror && !already){
            printevalerror();
        }

    }   while(!eof && !checkexit(root));

    if(eof) cout << endl << "> ERROR (no more input) : END-OF-FILE encountered";
    cout << endl << "Thanks for using OurScheme!";
}