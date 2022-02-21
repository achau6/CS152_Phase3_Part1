  /* cs152-miniL-lex    */

%{
   /* write your C code here for defination of variables and including headers */
   #include "miniL-parser.hpp"
   #include "y.tab.h"
   
   int pos = 0;
   int line = 1;
%}


   /* some common rules, for example DIGIT */
DIGIT    [0-9]
LETTERS                    [a-zA-Z]
CHAR                       [0-9a-zA-Z_]
ALPHANUMBER                [0-9a-zA-Z]
IDENTIFIER_WORD            {LETTERS}({LETTERS}|[_]|{DIGIT})*(({LETTERS}|DIGIT)+)*
IDENTIFIER_WORD_ERROR      ({DIGIT}|[_])({LETTERS}|[_]|{DIGIT})*(({LETTERS}|DIGIT)+)*
IDENTIFIER_WORD_ERROR_TWO  {LETTERS}(({LETTERS}|[_]|{DIGIT})*({LETTERS}|DIGIT)+[_])*


%%
   /* specific lexer rules in regex */

"="            {printf("EQUAL\n"); pos += yyleng; }
{DIGIT}+       {yylval.ival = atoi(yytext); return NUMBER; pos += yyleng;}
function       {return FUNCTION; pos += yyleng;}
beginparams    {return BEGIN_PARAMS; pos+=yyleng;}
endparams      {return END_PARAMS; pos += yyleng;}
beginlocals    {return BEGIN_LOCALS; pos += yyleng;}
endlocals      {return END_LOCALS; pos += yyleng;}
beginbody      {return BEGIN_BODY; pos += yyleng;}
endbody        {return END_BODY; pos += yyleng;}
integer        {return INTEGER; pos += yyleng;}
array          {return ARRAY; pos += yyleng;}
of             {return OF; pos += yyleng;}
if             {return IF; pos += yyleng;}
then           {return THEN; pos += yyleng;}
endif          {return ENDIF; pos += yyleng;}
else           {return ELSE; pos += yyleng;}
while          {return WHILE; pos += yyleng;}
do             {return DO; pos += yyleng;}
beginloop      {return BEGINLOOP; pos += yyleng;}
endloop        {return ENDLOOP; pos += yyleng;}
continue       {return CONTINUE; pos += yyleng;}
break          {return BREAK; pos += yyleng;}
read           {return READ; pos += yyleng;}
write          {return WRITE; pos += yyleng;}
not            {return NOT; pos += yyleng;}
true           {return TRUE; pos += yyleng;}
false          {return FALSE; pos += yyleng;}
return         {return RETURN; pos += yyleng;}
"-"            {return SUB; pos += yyleng;}
"+"            {return ADD; pos += yyleng;}
"*"            {return MULT; pos += yyleng;}
"/"            {return DIV; pos += yyleng;}
"%"            {return MOD; pos += yyleng;}
";"            {return SEMICOLON; pos += yyleng;}
":"            {return COLON; pos += yyleng;}
","            {return COMMA; pos += yyleng;}
"("            {return L_PAREN; pos += yyleng;}
")"            {return R_PAREN; pos += yyleng;}
"["            {return L_SQUARE_BRACKET; pos += yyleng;}
"]"            {return R_SQUARE_BRACKET; pos += yyleng;}
"=="           {return EQ; pos += yyleng;}
"<>"           {return NEQ; pos += yyleng;}
"<"            {return LT; pos += yyleng;}
">"            {return GT; pos += yyleng;}
"<="           {return LTE; pos += yyleng;}
">="           {return GTE; pos += yyleng;}
":="           {return ASSIGN; pos += yyleng;}
("##").*       {line += yyleng;}
[ \t]+         {pos += yyleng;}
"\n"           {line ++; pos = 0;}
{IDENTIFIER_WORD}  	      {yylval.identval = yytext; return IDENT; pos += yyleng;}
{IDENTIFIER_WORD_ERROR}       printf("Start Error at line %d, column %d: can not begin \"%s\"\n", line, pos, yytext);
{IDENTIFIER_WORD_ERROR_TWO}   printf("End Error at line %d, column %d: can not end \"%s\"\n", line, pos, yytext);
.              {printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", line, pos, yytext); exit(0);}


%%
	/* C functions used in lexer */
   /* remove your original main function */