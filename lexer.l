%option noyywrap
%{   
#include<string.h>
#include "bison.tab.h"
   int currLine = 1, currPos = 1;
   
   extern char *identToken;
   extern int numberToken;
%}

DIGIT    [0-9]
LETTER   [a-zA-Z]   

%%

function       {currPos += yyleng; return FUNCTION;}
beginparams    {currPos += yyleng; return BEGIN_PARAMS;}
endparams      {currPos += yyleng; return END_PARAMS;}
beginlocals    {currPos += yyleng; return BEGIN_LOCALS;}
endlocals      {currPos += yyleng; return END_LOCALS;}
beginbody      {currPos += yyleng; return BEGIN_BODY;}
endbody        {currPos += yyleng; return END_BODY;}
integer        {currPos += yyleng; return INTEGER;}
write          {currPos += yyleng; return WRITE;}
"-"            {currPos += yyleng; return SUB;}
"+"            {currPos += yyleng; return ADD;}
"*"            {currPos += yyleng; return MULT;}
"/"            {currPos += yyleng; return DIV;}
"%"            {currPos += yyleng; return MOD;}
";"            {currPos += yyleng; return SEMICOLON;}
":"            {currPos += yyleng; return COLON;}
":="           {currPos += yyleng; return ASSIGN;}
{DIGIT}+       {
  currPos += yyleng; 
  char * token = new char[yyleng];
  strcpy(token, yytext);
  yylval.op_val = token;
  numberToken = atoi(yytext); 
  return NUMBER;
}

##(.)*\n       {/* do not print comments */ currLine++; currPos = 1;}

[ \t]+         {/* ignore spaces */ currPos += yyleng;}

"\n"           {currLine++; currPos = 1;}

({LETTER})|({LETTER}({LETTER}|{DIGIT}|"_")*({LETTER}|{DIGIT}))     {
   currPos += yyleng;
   char * token = new char[yyleng];
   strcpy(token, yytext);
   yylval.op_val = token;
   identToken = yytext; 
   return IDENT;
}

((("_")+)|(({DIGIT})+({LETTER}|"_")))({DIGIT}|{LETTER}|"_")*                { printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", currLine, currPos, yytext); exit(0);}

({LETTER})({DIGIT}|{LETTER}|"_")*("_")                                       {printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", currLine, currPos, yytext); exit(0);}


.   {printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", currLine, currPos, yytext); exit(0);}

%%



