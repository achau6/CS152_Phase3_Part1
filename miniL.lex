   /* cs152-miniL */

%{
   /* write your C code here for defination of variables and including headers */
#include "miniL-parser.hpp"

%}

   /* some common rules, for example DIGIT */
DIGIT    [0-9]
   
%%
   /* specific lexer rules in regex */

{DIGIT}+       {printf("NUMBER %s\n", yytext); yylval.int_val = atoi(yytext); return DIGIT; }

%%
	/* C functions used in lexer */
   /* remove your original main function */