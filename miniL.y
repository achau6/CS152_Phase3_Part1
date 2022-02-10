/* cs152-miniL phase3 */


%{
void yyerror(const char *msg);
extern int yylex();

#include "lib.h"

%}

%union {
  int int_val;
}

%error-verbose

%token<int_val> DIGIT
%start program 

%% 

  /* write your rules here */
program: DIGIT {}

%% 

int main(int argc, char **argv) {
   yyparse();
   return 0;
}

void yyerror(const char *msg) {
    /* implement your error handling */
}