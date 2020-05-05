
/* prologue */
%{
#include "parser.h"
#include "lexer.h"

#include "ast.h"

/*
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
*/

void yyerror (YYLTYPE*, yyscan_t, char const *);
%}

%code requires {
#include "ast.h"

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void* yyscan_t;
#endif
}

%defines "parser.h"
%locations
%define lr.type ielr
%define parse.error verbose
%define parse.trace
%define api.pure full
%define api.push-pull push
%parse-param {yyscan_t scanner}
%lex-param {YYLTYPE* llocp, yyscan_t scanner}
/*
  signature of yylex && yyparse:

  int yylex (YYSTYPE* lvalp, YYLTYPE* llocp, yyscan_t scanner);

  int yypush_parse (yypstate* ps, int current_token, YYSTYPE* current_lval, yyscan_t scanner);
*/

/*
  our abstract syntax tree needs to represent
  the following terms:
    t := nil          # the bottom type, useful for saying not/none/zero/error
       | <id>:<type>  # names (variables), encapsulate state
       | <lambda>     # lambdas (functions), encapsulate behavior
       | <call>       # a lambda execution site, saying "here is where this behavior happens with this state"
       | <bind>       # bind this name to this term, once bound you can refer to the term later on in program text


       what is the finite set which describes the possible tokens we could see?
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
/* bison decls */
%union {
  Ast*  ast;
  char* str;
}
/*
  each of the nonterminal symbols within the grammar is associated
  with a field within the structure of the Ast.
  to support using the rules in the naturally recursive way
  each non-terminal value is only ever a pointer to the relevant node.
*/
%nterm <ast> term name lambda call bind type

%token NIL
%token <str> ID
%token COLON COLONEQUALS SEMICOLON
%token BSLASH
%token RARROW REQARROW
%token LPAREN RPAREN


%%
/* grammar */

term:
    name    { $$ = $1; }
  | lambda  { $$ = $1; }
  | call    { $$ = $1; }
  | bind    { $$ = $1; }
  | NIL     { $$ = CreateAstTypeNil(); }

name: /* [a-zA-Z][a-zA-Z0-9_-]+ */
  ID  { $$ = CreateAstId($1); }

lambda: /* \ name : type => term */
  BSLASH ID COLON type REQARROW term { $$ = CreateAstLambda($2, $4, $6); }

call: /* term term */
  term term { $$ = CreateAstCall($1, $2); }

bind: /* name := term */
  ID COLONEQUALS term { $$ = CreateAstBind($1, $3); }

type:
    %empty              { $$ = CreateAstTypeInfer(); }
  | NIL                 { $$ = CreateAstTypeNil();   }
  | type RARROW type    { $$ = CreateAstTypeFn($1, $3); }
  | LPAREN type RPAREN  { $$ = $2; }



%%

/* epilogue */
void yyerror (YYLTYPE* lloc, yyscan_t scanner, char const* s)
{

  fprintf (stderr, "%s\n", s);

}
