/* Pink: Lexer/Scanner/Tokenizer

  what is the finite set which describes the possible
  tokens we could return?
  {
   nil
   <id>
   \
   ->
   :=
   ;
   :
  }

*/
%{
#include "parser.h" // for YYSTYPE, YYLTYPE, and token definitions

struct StrLoc;

void update_location(StrLoc* llocp, char* txt);
#define YY_USER_ACTION update_location(llocp, yytext);
%}

%option reentrant
/*%option extra-type="struct StrLoc*"*/
/*
%option bison-bridge
%option bison-locations
*/
  /*
  these three options change the lexers signature to:
	int yylex (YYSTYPE* lval, YYLTYPE* llocp, yyscan_t scanner);

  reentrant adds the last arg yyscan_t
  bison-bridge adds the first argument YYSTYPE.
  bison-locations adds the second argument YYLTYPE.
  */

%option outfile="lexer.c"
%option header-file="lexer.h"
%option warn
%option debug

SPACE   [[:space:]]

DIGIT   [[:digit:]]

%%

{SPACE}+  /* eat whitespace */

"nil" { return NIL; }
"Nil" { return NIL_TYPE; }
":"   { return COLON; }
":="  { return COLONEQUALS; }
"\\"  { return BSLASH; }
"->"  { return RARROW; }
"=>"  { return REQARROW; }
";"   { return SEMICOLON; }
"("   { return LPAREN; }
")"   { return RPAREN; }
"{"   { return LBRACE; }
"}"   { return RBRACE; }

[a-zA-Z_]+[[:alnum:]_]* { yylval->ID = strdup(yytext); return ID; }

"\0" /*
 this ignores the extra NULL characters in the buffer we manage for user input.
     */
.     printf ("unrecognized input: \"%s\"\n", yytext);

%%


void update_location(StrLoc* lloc, char* txt)
{
  lloc->first_line = lloc->last_line;
  lloc->first_column = lloc->last_column;

  for (int i = 0; txt[i] != '\0'; i++)
  {
    if (txt[i] == '\n') {
      lloc->last_line++;
      lloc->last_column = 1;
    }
    else {
      lloc->last_column++;
    }

  }

}
