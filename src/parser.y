
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

/*
%union {
  Ast*  ast;
  char* str;
}
*/
%define api.value.type union
/*
  each of the nonterminal symbols within the grammar is associated
  with a field within the taggeed union structure of the Ast.
  to support using the rules in the naturally recursive way
  each non-terminal value is only ever a pointer to the relevant node.
  this allows the Ast construction functions to be written
  in a way that allows for the highly compositional style
  of programming that structured programming encourages.
  so, astute observers will notice that ID's are bald
  char*'s. they will also probably notice that the
  Ast constructors that are used in the rules that
  utilize the ID nonterminals take char*s as parameters
  in those positions.
  this is the same mechanism that will be used when the
  language is extended to add int's, floats, chars,
  strings, and all other primitive literals to the language.
  the lexer will contain the logic to read in each literal
  and the parser will accept a token, and the language will
  accept a new type.

  if we want to give programmers the means to define new
  primitive type value literals, we could give them
  the means to define the logic of the recognizer,
  the type, and the token would probably need to be
  treated in a more general way from the perspective
  of the parser. the representation (the type) would need to
  be built up from defined types to be supported by the
  back end.

-------------------------------------------------------------------------

  so, if we take a slightly more experienced look at what entities
  appear in the untyped lambda calculus
  we have variables, lambdas, and application.
  the lamdba is taken to be the literal value that
   variables range over, and application is the
  basic action that can be performed upon lambda values.

  so, in expanding the calculus, and say, adding another
  type and literal value that can appear and be ranged over
  with variables; nil.

  we can treat nil literals and lambda literals as
  kinds of literal, each with a type. this allows then
  for the evaluator to construct statements like
  "evaluate until this term becomes a value" in a
  way that need not consider exactly which value we
  are evaluating down too.

  in considering extension of other kinds of actions that
  are recognized, (such as 'bind') the Ast simply must be extended to contain
  some new union member which represents the new action
  in the grammar.
*/
%nterm <Ast*> term name value lambda call bind type parenterm

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
  | value     { $$ = $1; }
  | call      { $$ = $1; }
  | bind      { $$ = $1; }
  | parenterm { $$ = $1; }
/*  | subterm { $$ = $1; } */

name: /* [a-zA-Z][a-zA-Z0-9_-]+ */
  ID  { $$ = CreateAstId($1); }

value:
    type   { $$ = $1; }
  | lambda { $$ = $1; }


type: /* nil | type -> type | ( type ) */
    NIL                 { $$ = CreateAstValueTypeNil(); }
  | type RARROW type    { $$ = CreateAstValueTypeFn($1, $3); }
  | LPAREN type RPAREN  { $$ = $2; }

lambda: /* \ name : arg_type => term */
    BSLASH ID COLON type REQARROW term { $$ = CreateAstValueFn($2, $4, $6); }
  //| BSLASH ID REQARROW term            { $$ = CreateAstLambda($2, CreateAstTypeInfer(), $4); }

call: /* term term */
  term term { $$ = CreateAstCall($1, $2); }

bind: /* name := term */
  ID COLONEQUALS term { $$ = CreateAstBind($1, $3); }

parenterm: /* ( term ) */
  LPAREN term RPAREN { $$ = $2; }

/*
subterm: { term }
    LBRACE term RBRACE  { $$ = $2; }
*/
%%

/* epilogue */
void yyerror (YYLTYPE* lloc, Ast** result, yyscan_t scanner, char const* s)
{

  fprintf (stderr, "%s\n", s);

}
