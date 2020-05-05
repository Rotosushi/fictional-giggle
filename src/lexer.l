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

void update_location(YYLTYPE* lloc, char* txt);
#define YY_USER_ACTION update_location(yylloc, yytext);
%}

%option reentrant
%option bison-bridge
%option bison-locations
  /*
  these three options change the lexers signature to:
	int yylex (YYSTYPE* lval, YYLTYPE* llocp, yyscan_t scanner);

  reentrant adds the last arg yyscan_t
  bison-bridge adds the first argument YYSTYPE.
  bison-locations adds the second argument YYLTYPE.
  */

%option outfile="lexer.c"
%option header-file="lexer.h"
%option warn noyywrap
%option debug

SPACE   [[:space:]]

DIGIT   [[:digit:]]

ID      [a-zA-Z][a-zA-Z0-9_-]*



%%

{SPACE}+    /* eat whitespace */

"nil" { return NIL; }
":"   { return COLON; }
":="  { return COLONEQUALS; }
"\\"  { return BSLASH; }
"->"  { return RARROW; }
"=>"  { return REQARROW; }
";"   { return SEMICOLON; }

[a-zA-Z][a-zA-Z0-9_-]*    { yylval->ID = strdup(yytext); return ID; }

.     printf ("unrecognized input: <%s>\n", yytext);

%%


void update_location(YYLTYPE* lloc, char* txt)
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
