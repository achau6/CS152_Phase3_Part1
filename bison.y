%{
#include<iostream>
#include<stdio.h>
#include<string>
#include<vector>
#include<string.h>


extern int yylex(void);
void yyerror(const char *msg);
extern int currLine;

char *identToken;
int numberToken;
int  count_names = 0;


enum Type { Integer, Array };

struct CodeNode {
    std::string name;
	std::string code; 
	Type type; 
};

struct Symbol {
  std::string name;
  Type type;
};

struct Function {
  std::string name;
  std::vector<Symbol> declarations;
};

std::vector <Function> symbol_table;


Function *get_function() {
  int last = symbol_table.size()-1;
  return &symbol_table[last];
}

bool find(std::string &value) {
  Function *f = get_function();
  for(int i=0; i < f->declarations.size(); i++) {
    Symbol *s = &f->declarations[i];
    if (s->name == value) {
      return true;
    }
  }
  return false;
}

void add_function_to_symbol_table(std::string &value) {
  Function f; 
  f.name = value; 
  symbol_table.push_back(f);
}

void add_variable_to_symbol_table(std::string &value, Type t) {
  Symbol s;
  s.name = value;
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


%union {
  char *op_val;
  struct CodeNode *code_node; 
}

%define parse.error verbose
%start prog_start
%token BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY
%token FUNCTION 
%token INTEGER 
%token WRITE
%token SUB ADD MULT DIV MOD
%token SEMICOLON COLON COMMA ASSIGN
%token <op_val> NUMBER 
%token <op_val> IDENT
%type <op_val> symbol 

%type <code_node> prog_start
%type <code_node> functions
%type <code_node> function
%type <code_node> declarations
%type <code_node> declaration
%type <code_node> statements
%type <code_node> statement


%%

prog_start: functions
{
	CodeNode* node = $1;
	std::cout << node->code << std::endl;
}

functions: 
/* epsilon */
{ 
	CodeNode* node = new CodeNode();
	$$ = node;
}
| function functions
{
	CodeNode* node = new CodeNode();
	CodeNode* code_node1 = $1;
	CodeNode* code_node2 = $2;
	node->code += code_node1->code + code_node2->code;
	$$ = node;
};

function: FUNCTION IDENT 
{
  // midrule:
  // add the function to the symbol table.
  std::string func_name = $2;
  add_function_to_symbol_table(func_name);
//   std::cout << "func" << func_name << std::endl;
  CodeNode* node = new CodeNode();
  node->name = func_name;
  node->code += "func" + func_name + "\n";
  $<code_node>$ = node;
}
	SEMICOLON
	BEGIN_PARAMS declarations END_PARAMS
	BEGIN_LOCALS declarations END_LOCALS
	BEGIN_BODY statements END_BODY
{
  node->name = "endfunc";
  node->code += "endfunc\n";
  $$ = node;
};

declarations: 
/* epsilon */
{
	CodeNode* node = new CodeNode();
	$$ = node;
}
| declaration SEMICOLON declarations
{
	CodeNode* node = new CodeNode();
	CodeNode* code_node1 = $1;
	CodeNode* code_node2 = $3;
	node->code += code_node1->code + code_node2->code;
	$$ = node;
};

declaration: 
	IDENT COLON INTEGER
{
  // add the variable to the symbol table.
  std::string value = $1;
  Type t = Integer;
  add_variable_to_symbol_table(value, t);
  
  CodeNode* node = new CodeNode();
  node->name = value;
  node->code += ". " + value + "\n";
//   std::cout << value;
  $$ = node;
};

statements: 
statement SEMICOLON
{
	// CodeNode* node = new CodeNode();
	// std::string value = $1;
	// node->name = value
  	// node->code = ". " + value + "\n";
	// $$ = node;
}
| statement SEMICOLON statements
{
};

statement: 
IDENT ASSIGN symbol ADD symbol
{
}
| IDENT ASSIGN symbol SUB symbol
{
}
| IDENT ASSIGN symbol MULT symbol
{
}
| IDENT ASSIGN symbol DIV symbol
{
}
| IDENT ASSIGN symbol MOD symbol
{
}

| IDENT ASSIGN symbol
{
}

| WRITE IDENT
{
}
;

symbol: 
IDENT 
{
  $$ = $1; 
}
| NUMBER 
{
  $$ = $1; 
}

%%

int main(int argc, char **argv)
{
   yyparse();
   print_symbol_table();
   return 0;
}

void yyerror(const char *msg)
{
   printf("** Line %d: %s\n", currLine, msg);
   exit(1);
}
