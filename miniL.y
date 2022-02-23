/* cs152-miniL phase3 */


%{
#include <string>
#include <stdio.h>
#include <algorithm>
#include <vector>
#include <iostream>
#include "lib.h"
#include "y.tab.h"
#include "stdbool.h"
using namespace std;
#include <stdlib.h>
extern int line;

void yyerror(const char *msg);
extern int yylex();
extern FILE* yyin;
extern int yyparse();
string *temp_gen();
string decl_temp(string *s);
string decl_temp2(string *s);
string decl_temp2(string s);
string call_temp();
string call_temp(int x);
int counts = 0;
enum Type { Integer, Array, Add, Mult, Sub, Div, Mod, Params };
vector<string> reserve_words { 
   "pee", "function", "beginparams", "endparams", "beginlocals", "endlocals", "beginbody", "endbody", "integer", "array", "of", "if", "then", "endif", "else", "while", "do", "beginloop", "endloop", "continue", "break", "read", "write", "not", "true", "false", "return"
};

vector<string> confirm_words {
   "add", "sub", "mult", "div", "mod"
};

//make sure 1 main is declare X
//make sure rhs var is validate X <- technically
//array <= 0 X
//validate functions like add() and not addy() X

struct CodeNode {
    std::string name;
    string name2;
    std::string code;
    Type type;
    Type func;
    string ret;
};

void add_symb_table(int i, string node, Type t);
void check_dup_table();
void print_table();
void check_type();
void check_exist();
void check_existRHS();
void check_statement(string node);
void add_main(string node);
void check_main();
void clear_vectors();
void print_sym_table();

struct collection {
   string name;
   bool exist;
   Type type;
};

vector<collection> sym_table;
vector<collection> sym_table2;
vector<collection> sym_table3;
vector<string> main_hold;

void clear_vectors() {
   cout<<"Start SYMBOL TABLE--------------"<<endl;
   for(int i = 0; i < sym_table.size(); i ++) {
      cout<<sym_table[i].name<<endl;
   }
   cout<<"END SYMBOL TABLE--------------"<<endl;
   sym_table.clear();
   sym_table2.clear();
   sym_table3.clear();
}
void add_main(string node) {
   main_hold.push_back(node);
}

void check_main() {
   bool flag = false;
   for(int i = 0; i < main_hold.size(); i ++) {
      //cout<<"\n"<<"start: "<<main_hold[i]<<endl; 
      if(main_hold[i] == "main") {
         //cout<<"HEPPEN"<<endl;
         flag = true;
      }
   }
   if(flag == false) {
      cout<<"There is no declare Main "<<endl;
      exit(0);
   }
}  

void add_symb_table(int i, string node, Type t) {
   string id = node;
   bool word = true;
   int currline = 0;
   for(int i = 0; i < reserve_words.size(); i ++) {
      if(node == reserve_words[i]) {
         currline = i + 4 + 2;
         word = false;
      }  
   }
   if(word == false) {
      cout<<"ERROR: Line: "<< currline << " This is a reserve word"<<endl;
      exit(0);
   } else if (word == true) {
      if(i == 1) {
         sym_table.push_back({id, false, t});
      } else if(i == 2) {
         if(t == Array){
            id = (id.front());
            sym_table2.push_back({id, false, t});
         } else {
            sym_table2.push_back({id, false, t});
         }
      } else if(i == 3) {
         sym_table3.push_back({id, false, t});
      }
   }
}

void check_dup_table() {
   //cout << "Check Table: " << sym_table2.size() << endl;
   int currline = 0;
   for(int i = 0; i < sym_table.size(); i ++) {
         //cout<<"Name: "<<sym_table[i].name <<endl;
         for(int j = 0; j < i; j ++) {
            if(i != j) {
               if(sym_table[i].name == sym_table[j].name) {
                  currline = i + 4 + 1; //curr + statement + 1
                  cout << "ERROR: Line: "<< currline << " Redefination of "<< sym_table[i].name << endl;
                  exit(0);
               }
            }
         }
   }
}

void check_type() {
   int currline = 0;
   for(int i = 0; i < sym_table2.size(); i ++) {
      for(int j = 0; j < sym_table.size(); j ++) {
         if(sym_table2[i].name == sym_table[j].name && sym_table[j].type == Array) {
            if(sym_table2[i].type != Array) {
               currline = i + 7 + sym_table.size();
               cout<<"ERROR: Line: "<< currline << " Array should have index"<<endl;
               exit(0);
            } 
         } else if(sym_table2[i].name == sym_table[j].name && sym_table[j].type == Integer) {
            if(sym_table2[i].type != Integer) {
               currline = i + 7 + sym_table.size();
               cout<<"ERROR: Line: "<<sym_table2[i].name<<" "<< currline << " Integer should not have index"<<endl;
               exit(0);
            }
         }
      }
   }

}

void check_statement(string node) {
   bool word = true;
   int currline = 0;
   //cout<<"NAME: "<<node<<endl;
   for(int i = 0; i < confirm_words.size(); i ++) {
      //cout<<"NAME: "<<node<<" "<<confirm_words[i]<<endl;
      if(node == confirm_words[i]) {
         word = false;
      }  
   }
   if(word == true) {
      currline = 6 + 1 + sym_table.size() + sym_table2.size();
      cout<<"ERROR: Line: "<< currline << " This is a reserve word "<<node<<endl;
      exit(0);
   } 
}

void check_existRHS() {
   bool existed = false;
   int currline = 0;
   for(int i = 0; i < sym_table3.size(); i++) {
      if(sym_table3[i].name.length() == 1) {
         for(int j = 0; j < sym_table.size(); j++) {
            //cout<<"Statement: "<<sym_table3[i].name<<", "<<sym_table[j].name<<endl;
            currline = line; 
            //has trouble detecting errors like a = [b + c]
               if(sym_table3[i].name == sym_table[j].name || isdigit(*(sym_table3[i].name.c_str()))==true) {
                  existed = true;
                  break;
               }
            
         }
         if(existed == false) {
            currline = i+7+sym_table.size();
            //cout<<"line: "<<line<<endl;
            cout<<"ERROR2: Line: "<< currline << 
            " This variable "<<sym_table3[i].name<<" does not exist"<<endl;
            exit(0);
         }
         existed = false;
      }
      
   }
}

void check_exist() {
   bool existed = false;
   int currline = 0;
   for(int i = 0; i < sym_table2.size(); i++) {
      for(int j = 0; j < sym_table.size(); j++) {
         //cout<<"Statement: "<<sym_table2[i].name<<", "<<sym_table[j].name<<endl;
         currline = line; 
         if(sym_table2[i].name == sym_table[j].name) {
            existed = true;
            break;
         }
         
      }
      if(existed == false) {
         currline = i+7+sym_table.size();
         //cout<<"line: "<<line<<endl;
         cout<<"ERROR: Line: "<< currline << 
         " This variable "<<sym_table2[i].name<<" does not exist"<<endl;
         exit(0);
      }
      existed = false;
      
   }
}

void print_table() {
   check_dup_table();
   check_type();
   check_exist();
   check_existRHS();
}

%}


/*Rename to program because of syntax document*/
%start Program

%union{
  int ival;
  char* identval;
  struct CodeNode* code_node;
  //struct CodeNode code_node;
}

%define parse.error verbose
%locations

/*Change int to NUMBER b/c int and integer is confusing*/
%token <ival> NUMBER
%token <identval> IDENT


%token FUNCTION
%token BEGIN_PARAMS
%token END_PARAMS
%token BEGIN_LOCALS
%token END_LOCALS
%token BEGIN_BODY
%token END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO BEGINLOOP ENDLOOP CONTINUE BREAK READ WRITE NOT TRUE FALSE RETURN
%left SUB ADD MULT DIV MOD
%token SEMICOLON COLON COMMA
%left L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET
%left EQ NEQ LT GT LTE GTE
%right ASSIGN

%type <code_node> Declarations
%type <code_node> Declaration
%type <code_node> Statements
%type <code_node> Statement
%type <code_node> Function
%type <code_node> Functions
%type <code_node> Identifier
%type <code_node> Identifiers
%type <code_node> Var
%type <code_node> BoolExp
%type <code_node> Comp
%type <code_node> Expression
%type <code_node> Expressions
%type <code_node> MultExp
%type <code_node> Term
%type <code_node> ElseStatement

%type <code_node> Program


%% 

Program: Functions {
   //printf("Program -> Functions\n");
   CodeNode *node = $1;
   printf("%s\n", node->code.c_str()); 
   

};

Functions: %empty {
   //printf("Functions -> epsilon\n");
   CodeNode *node = new CodeNode;
   $$ = node;

}  | Function Functions {
   //printf("Functions -> Function Functions\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $2;
   CodeNode *node = new CodeNode;
   node->code = code_node1->code;
   check_main();
   node->code += code_node2->code;
   $$ = node;

};

Identifier: IDENT {
   //printf("ident -> IDENT %s\n", yylval.identval);
   CodeNode *node = new CodeNode;
   std::string name = $1;// + std::string("boo");
   node->code = name;
   $$ = node;
   
};
Identifiers: Identifier {
   //printf("Identifiers -> Identifier\n");
   CodeNode *node = new CodeNode;
   std::string name = $1->code;
   node->code = name;
   $$ = node;

};

Function: FUNCTION Identifier SEMICOLON BEGIN_PARAMS Declarations END_PARAMS BEGIN_LOCALS Declarations END_LOCALS BEGIN_BODY Statements END_BODY {
   //printf("Function -> FUNCTION Identifier SEMICOLON BEGIN_PARAMS Declarations SEMICOLON END_PARAMS BEGIN_LOCALS Declarations SEMICOLON END_LOCALS BEGIN_BODY Statements SEMICOLON END_BODY\n");
   CodeNode *node = new CodeNode;
   std::string func_name = $2->code;
   node->code = "";
   node->code += std::string("func ") + func_name + std::string("\n");
   add_main(func_name);

   CodeNode *declarations = $5;
   node->code += declarations->code;

   CodeNode *local_declarations = $8;
   node->code += local_declarations->code;

   CodeNode *statements = $11;
   node->code += statements->code;
   


   node->code += std::string("endfunc") + std::string("\n");
   $$ = node;
   
   //cout<<"Start SYMBOL TABLE--------------"<<endl;
   print_table();
   clear_vectors();
   //cout<<"END SYMBOL TABLE--------------"<<endl;

   
};

Declarations: %empty {
   //printf("Declarations -> epsilon\n");
   CodeNode *node = new CodeNode;
   $$ = node;
} | Declaration SEMICOLON Declarations {
   //printf("Declarations -> Declaration SEMICOLON Declarations\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
   node->code = code_node1->code + code_node2->code;
   $$ = node;
};

Declaration: Identifiers COLON INTEGER {
   //printf("Declaration -> Identifiers COLON INTEGER\n");
   CodeNode *value = $1;
   Type t = Integer;
   string name = value->code;
   add_symb_table(1, name, t);

   CodeNode *code_node = new CodeNode;
   std::string id = $1->code;
   code_node->code = std::string(". ") + id + std::string("\n");
   $$ = code_node;

} | Identifiers COLON ARRAY L_SQUARE_BRACKET Term R_SQUARE_BRACKET OF INTEGER {
   //printf("Declaration -> Identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");
   CodeNode *value = $1;
   Type t = Array;
   string name = value->code;
   add_symb_table(1, name, t);
   int a = stoi($5->code);
   if(a <= 0) {
      cout<<"ERROR: "<<4+sym_table.size()<<" SIZE IS <= 0"<<endl;
      exit(0);
   }
   
   CodeNode *code_node = new CodeNode;
   CodeNode *node = $5;
   std::string id = $1->code;
   code_node->code += std::string(".[] ") + id + std::string(", ") + node->code + std::string("\n");
   $$ = code_node;
};

Statements: %empty {
   //printf("Statements -> epsilon\n");
   CodeNode *node = new CodeNode;
   $$ = node;

}  | Statement SEMICOLON Statements {
   //printf("Statements -> Statement SEMICOLON Statements\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
   node->code = code_node1->code + code_node2->code;
   $$ = node;
};

ElseStatement: ELSE Statements {
   //printf("ElseStatement -> ELSE Statements\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("*= ");
   $$ = node;

} | %empty {
   //printf("ElseStatement -> epsilon\n");
   CodeNode *node = new CodeNode;
   $$ = node;
};
Statement: Var ASSIGN Expression {
   //printf("Statement -> Var ASSIGN Expression\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;

   add_symb_table(2, $1->code, $1->func);
   add_symb_table(3, $3->code, $3->func);
   //need to add what happens if you assign an array out of its value ex:a[-1] := 1;
   
   node->code = string("= ") + code_node1->code + string(", ") + code_node2->code + string("\n");
   
   if(code_node2->func == Add || code_node2->func == Sub) {
      node->code = code_node2->code + string("\n") 
         + string("= ") + code_node1->code + string(", ") + code_node2->name + string("\n");
   
   } else if(code_node2->func == Mult || code_node2->func == Div || code_node2->func == Mod) {
      //printf("Mult in Statement");
      node->code = code_node2->code + string("\n") + string("= ") 
         + code_node1->code + string(", ") + code_node2->name + string("\n");


   }
   if(code_node2->func == Params) {
      //printf("Param");
      node->code = string("") + code_node2->code + string("\n") 
         + string("= ") +  code_node1->code + string(", ") + code_node2->name + string("\n");
   }
   
   if(code_node1->func == Array) {
      node->code = string(".[] ") + code_node1->code + string(", ") + code_node2->code + string("\n");
      if(code_node2->func == Add) {
         //node->code = string(". ") + code_node2->name + string("\n");
         node->code = code_node2->code + string("\n");
         node->code += string("[]= ") + code_node1->code + string(", ") + code_node2->name + string("\n");
      }
      if(code_node2->func == Mult) {
         //printf("Mult in Array in Statement");
         //node->code = string(". ") + code_node2->name + string("\n");
         node->code = code_node2->code + string("\n");
         //node->code += string("[]= ") + code_node1->code + string(", ") 
         //   + code_node1->name + string("\n");
         
      }
   }

   $$ = node;

} | IF BoolExp THEN Statements ElseStatement ENDIF {
   //printf("Statement -> IF BoolExp THEN Statements ElseStatement ENDIF\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("*= ");
   $$ = node;

} | WHILE BoolExp BEGINLOOP Statements ENDLOOP {
   //printf("Statement -> WHILE BoolExp BEGINLOOP Statements ENDLOOP\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("*= ");
   $$ = node;

} | DO BEGINLOOP Statements ENDLOOP WHILE BoolExp {
   //printf("Statement -> DO BEGINLOOP Statements ENDLOOP WHILE BoolExp\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("*= ");
   $$ = node;

} | READ Var {
   //printf("Statement -> READ Var\n");
   CodeNode *code_node1 = $2;
   CodeNode *node = new CodeNode;
   node->code = std::string("!!= ") + code_node1->code;
   $$ = node;

} | WRITE Var {
   //printf("Statement -> WRITE Var\n");
   CodeNode *code_node1 = $2;
   CodeNode *node = new CodeNode;
   if(code_node1->func == Array) {
      string *temp = temp_gen();
      node->code = ". " + *temp + string("\n");
      node->code += string("=[] ") + *temp + string(", ") + code_node1->code + string("\n");
      node->code += string(".> ") + *temp + string("\n");
   } else {
      node->code = string(".> ") + code_node1->code + string("\n");
   }
   $$ = node;

} | CONTINUE {
   //printf("Statement -> CONTINUE\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("= ");
   $$ = node;

} | BREAK {
   //printf("Statement -> BREAK\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("= ");
   $$ = node;

} | RETURN Expression {
   //printf("Statement -> RETURN Expression\n");
   CodeNode *node = new CodeNode;
   CodeNode *code_node1 = $2;
   node->code = code_node1->ret + string("") + code_node1->code + string("\n") 
      + std::string("ret ") + code_node1->name + string("\n");
   $$ = node;

};


BoolExp: Expression Comp Expression {
   //printf("BoolExp -> Expression Comp Expression\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
   node->code = code_node1->code + code_node2->code;
   $$ = node;

} | NOT BoolExp {
   //printf("BoolExp -> NOT BoolExp\n");
   CodeNode *code_node1 = $2;
   CodeNode *node = new CodeNode;
   node->code = $2->code + std::string("++");
   $$ = code_node1;

};

Comp: ASSIGN {
   //printf("Comp -> ASSIGN\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("+= ");
   $$ = node;

} | NEQ {
   //printf("Comp -> NEQ\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("+= ");
   $$ = node;
} | LT {
   //printf("Comp -> LT\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("+= ");
   $$ = node;

} | GT {
   //printf("Comp -> GT\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("+= ");
   $$ = node;

} | LTE {
   //printf("Comp -> LTE\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("+= ");
   $$ = node;

} | GTE {
   //printf("Comp -> GTE\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("+= ");
   $$ = node;

}  | EQ {
   //printf("Comp -> EQ\n");
   CodeNode *node = new CodeNode;
   node->code = std::string("+= ");
   $$ = node;

};

Expressions: %empty {
   //printf("\n");
   CodeNode *node = new CodeNode;
   node->code = string("");
   $$ = node;
}  | COMMA Expression Expressions {
   CodeNode *node = new CodeNode;
   CodeNode *code_node1 = $2;
   node->code = code_node1->code;
   node->func = code_node1->func;
   node->name = code_node1->name;
   $$ = node;
};

Expression: MultExp {
   //printf("Expression -> MultExp\n");
   CodeNode *code_node1 = $1;
   CodeNode *node = new CodeNode;
   node->code = code_node1->code;
   node->func = code_node1->func;
   node->name = code_node1->name;
   node->name2 = code_node1->name2;
   node->ret = code_node1->ret; //only needed here to return exp
   $$ = node;

} | MultExp ADD Expression{
   //printf("Expression -> MultExp ADD Expression\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
   //printf("Expression Add");
   if(code_node1->func == Array) {
      /*printf("Array in Expression Add ");
      string *temp = temp_gen();
      node->code = string(". ") + call_temp(0) + string("\n");
      node->code += string("=[] ") + call_temp(0) + string(", ") + code_node1->code + string("\n");
     
      string *temp1 = temp_gen();
      node->code += ". " + call_temp(0) + string("\n");
      node->code += string("+ ") + call_temp(0) + string(", ") 
         + call_temp(1) + string(", ") + code_node2->code;
      node->name = *temp1;*/

      //printf("Array in Expression Add1 ");
      string *temp2 = temp_gen();
      string *temp = temp_gen();
      node->code = string(". ") + *temp + string("\n");
      node->code += string("=[] ") + *temp + string(", ") + code_node1->code + string("\n");
     
      string *temp1 = temp_gen();
      node->code += ". " + *temp1 + string("\n");
      node->code += string("+ ") + *temp1 + string(", ") 
         + *temp + string(", ") + code_node2->code;
      node->name = *temp2;
      node->name2 = *temp1;
      
   } else {
      //printf("Else in Expression Add ");
      string *temp = temp_gen();
      node->code = string(". ") + *temp + string("\n");
      node->code += string("+ ") + *temp + string(", ") 
         + code_node1->code + string(", ") + code_node2->code;
      node->name = *temp;
      node->ret = string("= ") + code_node1->code + string(", $0\n") 
         + string("= ") + code_node2->code + string(", $1\n");   
   }
   node->func = Add;
   $$ = node;

} | MultExp SUB Expression{
   //printf("Expression -> MultExp SUB Expression\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
   node->func = Sub;
   string *temp = temp_gen();
   node->code = string(". ") + *temp + string("\n");
   node->code += string("- ") + *temp + string(", ") 
      + code_node1->code + string(", ") + code_node2->code;
   node->name = *temp;
   $$ = node;

}; 


MultExp: Term {
   //printf("MultExp -> Term\n");
   CodeNode *code_node1 = $1;
   CodeNode *node = new CodeNode;
   node->code = code_node1->code;
   node->func = code_node1->func;
   node->name = code_node1->name;
   node->name2 = code_node1->name2;
   node->ret = code_node1->ret;
   $$ = node;

} | Term MULT Term{
   //printf("MultExp -> Term MULT Term\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
    if(code_node1->func == Array) {
      //printf("Mult in Array ");
      //string *temp = temp_gen();
      node->code = string(". ") + code_node2->name + string("\n");
      node->code += string("=[] ") + code_node2->name + string(", ") + code_node1->code + string("\n");
      
      //string *temp2 = temp_gen();
      //node->code += string(". ") + *temp2 + string("\n");
      //node->code += string("=[] ") + *temp2 + string(", ") + code_node2->code;
      node->code += string("") + code_node2->code + string("\n");

     
      string *temp1 = temp_gen();
      node->code += string(". ") + *temp1 + string("\n");
      node->code += string("* ") + *temp1 + string(", ") + code_node2->name + string(", ") 
         + code_node2->name2 + string("\n");

      node->code += string("[]= ") + code_node1->code + string(", ") + *temp1;
      node->name = *temp1;

      /*string *temp = temp_gen();
      node->code = string(". ") + call_temp(3) + string("\n");
      node->code += string("=[] ") + call_temp(3) + string(", ") + code_node1->code + string("\n");
     
      node->code += string("") + code_node2->code + string("\n");

      string *temp1 = temp_gen();
      node->code += string(". ") + call_temp(1) + string("\n");
      node->code += string("* ") + call_temp(1) + string(", ") + call_temp(4) + string(", ") 
         + code_node2->name + string("\n");

      node->code += string("[]= ") + code_node1->code + string(", ") + *temp1;
      node->name = *temp1;*/
      
   } else {
      //printf("Else ");
      string *temp = temp_gen();
      node->code = string(". ") + *temp + string("\n");
      node->code += string("* ") + *temp + string(", ") 
         + code_node1->code + string(", ") + code_node2->code;
      node->name = *temp;
      node->ret = string("= ") + code_node1->code + string(", $0\n") 
         + string("= ") + code_node2->code + string(", $1\n");
   }
   node->func = Mult;

   $$ = node;

} | Term DIV Term{
   //printf("MultExp -> Term DIV Term\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
   node->func = Div;
   string *temp = temp_gen();
   node->code = string(". ") + *temp + string("\n");
   node->code += string("/ ") + *temp + string(", ") 
      + code_node1->code + string(", ") + code_node2->code;
   node->name = *temp;
  
   $$ = node;

} | Term MOD Term{
   //printf("MultExp -> Term MOD Term\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
   node->func = Mod;
   string *temp = temp_gen();
   node->code = string(". ") + *temp + string("\n");
   node->code += string("% ") + *temp + string(", ") 
      + code_node1->code + string(", ") + code_node2->code;
   node->name = *temp;
   
   $$ = node;

}; 

Term:  Var {
   //printf("Term -> Var\n");
   CodeNode *node = new CodeNode;
   CodeNode *code_node1 = $1;
   node->code = code_node1->code;
   node->func = code_node1->func;
   node->name = code_node1->name;
   node->name2 = code_node1->name2;
   $$ = node;
   

} | NUMBER {
   //printf("Term -> NUMBER\n");
   CodeNode *node = new CodeNode;
   string code = to_string($1);
   node->code = code;
   
   $$ = node;

} | L_PAREN Expression R_PAREN {
   //printf("Term -> L_PAREN Expression R_PAREN\n");
   CodeNode *node = new CodeNode;
   CodeNode *code_node1 = $2;
   node->code = code_node1->code;
   node->func = code_node1->func;
   node->name = code_node1->name;
   node->name2 = code_node1->name2;
   $$ = node;

} | Identifier L_PAREN Expression R_PAREN {
   //printf("Term -> L_PAREN Expression R_PAREN\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
   node->code = code_node1->code;
   //node->func = Params;
   $$ = node;

} | Identifier L_PAREN Expression Expressions R_PAREN { /*Check this rule*/
   //printf("Term -> L_PAREN Expression COMMA R_PAREN\n"); 
   CodeNode *node = new CodeNode;
   CodeNode *code_node1 = $3;
   CodeNode *code_node2 = $4;
   CodeNode *code_node3 = $1;
   if(code_node2->func == Add) {
      //printf("Add in Param");
      node->code = string("param ") + code_node1->code + string("\n")
         + code_node2->code + string("\n");
      //string *temp = temp_gen();
      node->code += string("param ") + code_node2->name + string("\n");
      string *temp1 = temp_gen();
      node->code += ". " + *temp1 + string("\n");
      node->code += "call " + code_node3->code + string(", ") + *temp1 + string("");
      node->name = *temp1;

   } else {
      //printf("Just Param");
      node->code = string("param ") + code_node1->code + string("\n")
         + string("param ") + code_node2->code + string("\n");
      string *temp = temp_gen();
      node->code += ". " + *temp + string("\n");
      node->code += "call " + code_node3->code + string(", ") + *temp;
      node->name = *temp;
   }

   string a = code_node3->code;
   check_statement(a);

   node->func = Params;
   $$ = node;

}; 

Var: Identifier {
   //printf("Var -> Identifier\n");
   CodeNode *node = new CodeNode;
   CodeNode *code_node1 = $1;
   node->code = code_node1->code;
   node->func = Integer;
   node->name = code_node1->name;
   
   $$ = node;
   

} | Identifier L_SQUARE_BRACKET Expression R_SQUARE_BRACKET {
   //printf("Var -> Identifier L_SQUARE_BRACKET Expression R_SQUARE_BRACKET\n");
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
   node->code = code_node1->code + string(", ") + code_node2->code + string("");
   node->func = Array;
   //node->name = code_node1->name;
   $$ = node;

/*} | Identifier L_SQUARE_BRACKET Expression R_SQUARE_BRACKET ASSIGN Expression {
   CodeNode *code_node1 = $1;
   CodeNode *code_node2 = $3;
   CodeNode *node = new CodeNode;
   printf("VAR []");
   $$ = node;*/
};


%% 

string *temp_gen() {
   std::string* temp = new std::string();
   *temp = std::string("__temp") + std::to_string(counts) + std::string("");
   counts++;
   return temp;
}

string call_temp() {
   string temp = "";
   return temp = std::string("__temp") + std::to_string((counts)) + std::string("");
}

string call_temp(int x) {
   string temp = "";
   return temp = std::string("__temp") + std::to_string((counts-x)) + std::string("");
}

string decl_temp(string *s){
   return "" + *s + "\n";
}

string decl_temp2(string *s){
   return ". " + *s + "";
}

string decl_temp2(string s){
   return ". " + s + "";
}

int main(int argc, char **argv) {
   if (argc > 1) {
      yyin = fopen(argv[1], "r");
      if (yyin == 0) {
         printf("Error Open File %s\n", argv[0]);
      }
   }
   yyparse();
   //print_table();
   return 0;
}

void yyerror(const char *msg) {
   printf("** Line %d: %s\n", yylloc.first_line, msg);
}