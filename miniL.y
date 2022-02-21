/* cs152-miniL phase3 */


%{
#include <string>
#include <stdio.h>
#include <vector>
#include <iostream>
#include "lib.h"
#include "y.tab.h"
#include "stdbool.h"
using namespace std;
#include <stdlib.h>

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
int count = 0;
enum Type { Integer, Array, Add, Mult, Sub, Div, Mod, Params };
struct CodeNode {
    std::string name;
    string name2;
    std::string code;
    Type type;
    Type func;
    string ret;
};

struct Symbol {
   string name;
   Type type;
};

struct Function {
  std::string name;
  std::vector<CodeNode> declarations;
};

std::vector <Function> symbol_table;


Function *get_function() {
  int last = symbol_table.size()-1;
  return &symbol_table[last];
}

bool find(std::string value) {
  Function *f = get_function();
  for(int i=0; i < f->declarations.size(); i++) {
    CodeNode *s = &f->declarations[i];
    if (s->name == value) {
      return true;
    }
  }
  return false;
}

void add_function_to_symbol_table(std::string value) {
  Function f; 
  f.name = value; 
  symbol_table.push_back(f);
}

void add_variable_to_symbol_table(std::string *value, Type t) {
  CodeNode s;
  //s.name = value->code;
  s.type = t;
  Function *f = get_function();
  f->declarations.push_back(s);
}

void print_symbol_table(void) {
  printf("symbol table:\n");
  printf("--------------------\n");
  for(int i=0; i<symbol_table.size(); i++) {
    printf("function: %s\n", symbol_table[i].name.c_str());
    for(int j=0; j<symbol_table[i].declarations.size(); j++) {
      printf("  locals: %s\n", symbol_table[i].declarations[j].name.c_str());
    }
  }
  printf("--------------------\n");
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
   node->code = code_node1->code + code_node2->code;
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

   //node->code += std::string("\n Begin Param \n");

   CodeNode *declarations = $5;
   node->code += declarations->code;

   //node->code += std::string("\n Begin Local \n");

   CodeNode *local_declarations = $8;
   node->code += local_declarations->code;

   //node->code += std::string("\n Begin Body \n");

   CodeNode *statements = $11;
   node->code += statements->code;
   


   node->code += std::string("endfunc") + std::string("\n");
   $$ = node;

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
   //CodeNode *value = $1;
   //Type t = Integer;
   //add_variable_to_symbol_table(value, t);

   CodeNode *code_node = new CodeNode;
   std::string id = $1->code;
   code_node->code = std::string(". ") + id + std::string("\n");
   $$ = code_node;

} | Identifiers COLON ARRAY L_SQUARE_BRACKET Term R_SQUARE_BRACKET OF INTEGER {
   //printf("Declaration -> Identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");
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
   /*
      a[0] = 1;
      a = 1;
   */
   node->code = string("[=] ") + code_node1->code + string(", ") + code_node2->code + string("\n");
   
   if(code_node2->func == Add || code_node2->func == Sub) {
      node->code = code_node2->code + string("\n") 
         + string("= ") + code_node1->code + string(", ") + code_node2->name + string("\n");
   
   } else if(code_node2->func == Mult || code_node2->func == Div || code_node2->func == Mod) {
      printf("Mult in Statement");
      node->code = code_node2->code + string("\n") + string("= ") 
         + code_node1->code + string(", ") + code_node2->name + string("\n");


   }
   if(code_node2->func == Params) {
      printf("Param");
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
         printf("Mult in Array in Statement");
         //node->code = string("[.] ") + code_node2->name + string("\n");
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

      printf("Array in Expression Add1 ");
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
      printf("Else in Expression Add ");
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
      printf("Mult in Array ");
      //string *temp = temp_gen();
      node->code = string("{.} ") + code_node2->name + string("\n");
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
      printf("Else ");
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
      printf("Add in Param");
      node->code = string("param ") + code_node1->code + string("\n")
         + code_node2->code + string("\n");
      //string *temp = temp_gen();
      node->code += string("param ") + code_node2->name + string("\n");
      string *temp1 = temp_gen();
      node->code += ". " + *temp1 + string("\n");
      node->code += "call " + code_node3->code + string(", ") + *temp1 + string("");
      node->name = *temp1;

   } else {
      printf("Just Param");
      node->code = string("param ") + code_node1->code + string("\n")
         + string("param ") + code_node2->code + string("\n");
      string *temp = temp_gen();
      node->code += ". " + *temp + string("\n");
      node->code += "call " + code_node3->code + string(", ") + *temp;
      node->name = *temp;
   }
   node->func = Params;
   $$ = node;

}; 

Var: Identifier {
   //printf("Var -> Identifier\n");
   CodeNode *node = new CodeNode;
   CodeNode *code_node1 = $1;
   node->code = code_node1->code;
   node->func = code_node1->func;
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
   *temp = std::string("__temp") + std::to_string(count) + std::string("");
   count++;
   return temp;
}

//void writeChange(bool &b) {
//   writeCheck = true;
//}

//bool writeChecks() {
//   return writeCheck;
//}

string call_temp() {
   string temp = "";
   return temp = std::string("__temp") + std::to_string((count)) + std::string("");
}

string call_temp(int x) {
   string temp = "";
   return temp = std::string("__temp") + std::to_string((count-x)) + std::string("");
}

string decl_temp(string *s){
   return "'" + *s + "\n";
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
   print_symbol_table();
   yyparse();
   return 0;
}

void yyerror(const char *msg) {
   printf("** Line %d: %s\n", yylloc.first_line, msg);
}