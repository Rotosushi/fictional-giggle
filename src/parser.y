
/* prologue */
%{
#include "parser.h"
#include "lexer.h"

#include "ast.h"


void yyerror (YYLTYPE*, Ast**, yyscan_t, char const *);
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
%define lr.type lalr
%define parse.error verbose
%define parse.trace
%define api.pure full
%define api.push-pull push
%parse-param {Ast** result} {yyscan_t scanner}
%lex-param {YYLTYPE* llocp} {yyscan_t scanner}
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

%define api.value.type union

%nterm <Ast*> term name entity lambda call bind type parenterm

%token NIL
%token <char*> ID
%token COLON COLONEQUALS SEMICOLON
%token BSLASH
%token RARROW REQARROW
%token LPAREN RPAREN
%token LBRACE RBRACE


%%
/* grammar */

input:
  /* return constructed ast to the caller via the result argument. */
  term { *result = $1; }

term:
    name      { $$ = $1; }
  | call      { $$ = $1; }
  | entity    { $$ = $1; }
  | bind      { $$ = $1; }
  | parenterm { $$ = $1; }
/*  | subterm { $$ = $1; }
  this is what I would want to make the
  explicit scope construct. but with the calculus
  being what it is, parens do this job to some extent.
*/

name: /* [a-zA-Z][a-zA-Z0-9_-]+ */
  ID  { $$ = CreateAstId($1, &@1); }

entity:
    type   { $$ = $1; }
  | lambda { $$ = $1; }


type: /* nil | type -> type | ( type ) */
    NIL                 { $$ = CreateAstEntityTypeNil(&@1); }
  | type RARROW type    { $$ = CreateAstEntityTypeFn($1, $3, &@$); }
  | LPAREN type RPAREN  { $$ = $2; }

lambda: /* \ name : arg_type => term */
    BSLASH ID COLON type REQARROW term { $$ = CreateAstEntityFn($2, $4, $6, &@$); }
  | BSLASH ID REQARROW term            { $$ = CreateAstEntityFn($2, CreateAstEntityTypePoly(), $4, &@$); }

call: /* term term */
  term term { $$ = CreateAstCall($1, $2, &@$); }

bind: /* name := term */
  ID COLONEQUALS term { $$ = CreateAstBind($1, $3, &@$); }

parenterm: /* ( term ) */
  LPAREN term RPAREN { $$ = $2; }
/* | LPAREN tuple RPAREN { $$ = $2; } */

/*
subterm: { term }
    LBRACE term RBRACE  { $$ = $2; }
*/
%%

/* epilogue */
void yyerror (YYLTYPE* lloc, Ast** result, yyscan_t scanner, char const* s)
{

  fprintf (stderr, "[%d:%d-%d:%d]%s\n", lloc->first_line
                                      , lloc->first_column
                                      , lloc->last_line
                                      , lloc->last_column
                                      ,s);

}
