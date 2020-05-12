
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
%define lr.type ielr
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
*/
%nterm <Ast*> term name lambda call bind type subterm

%token NIL
%token <char*> ID
%token COLON COLONEQUALS SEMICOLON
%token BSLASH
%token RARROW REQARROW
%token LPAREN RPAREN
%token LBRACE RBRACE


%%
/* grammar */

/*
  question:
    how the heck do we get access to the resulting AST once
    bison is done parsing tokens?

    This solution is currently not working,
    at first I was trying to access the resulting AST
    via lvalp, thinking that $$ means lvalp->term,
    but it doesn't, it means (yyval.term). which
    is in fact a local static variable to yypush_parse
    and cannot be used to retrieve the final result because of that.
    lval is actually the lookahead token for the parser.
    which means we shouldn't rely on it. This means
    that the current solution is probably the preferred
    solution.
    but when I added the new param, {Ast** result}
    i thought i could just assign the newly created
    variable to the pointer allocated in main,
    via the pointer-to-pointer result.

    (this is rule
      term { *result = $1; }
    below)

    to gain the semantics of "I am going to modify
    what another pointer is pointing to"
    so we can use the action to modify the local
    variable in main.

    but the value of result
    changes from when we preform the reduction of the rule
    and assign the nil node, then the parser returns
    it returns YYPUSH_MORE in order to process the end-of-input
    token, but once that returns result is in an invalid state.
    hence the segmentation fault in the printer function.
    the Ast param is not NULL, it points to some garbage.
    acctually in the debugger result is invalid as soon as we
    return from yypush-parse.

*/


  /*
  debugging notes: RE: Getting to the result before it's destroyed?!@?
  using GDB, and stepping through, I can confirm that this line
  does exactly what I expect, and correctly assigns the newly
  allocated Ast node in $1 into the *result.
  at some point between when this assignment happens and
  yypush_parse returning, the value gets corrupted.


  !!!in fact, it happens on the return from yypush_parse.!!!

  okay, why is this happening?

  I though this was the solution...

  we have yypush_parse building up our Ast while
  if shift/reduces, and on each reduction we use
  the action to either create a new Ast node or
  simply pass along already created nodes.
  we can confirm that the type of each non-terminal
  is an Ast* by checking the union declaration
  in parser.h. while in gdb i can confirm that
  the parameter called result within yypush_parse
  is being assigned the address of a malloc'ed Ast
  node from the NIL term reduction, each ptr containing
  the same address. ($1 and result).
  and, I can see that this value is being maintained
  accross calls to yypush_parse ~irrespective~ of the
  value of result within main. inside each call
  of yypush_parse result is correct, and in main we see no
  state change. curious...

  the issue was the order of the pointer arguments in the call site of the
  yypush_parse function, because the last argument is an
  opaque ptr to the lexers internal state, c happily casts
  the Ast* to a void* and the void* to an Ast* and doesn't even
  warn me about it. i am unsure why/how the lexer was even running
  when it was given the wrong structure, but okay...

  i fixed it by passing a ptr-to-ptr, and modifying
  the ptr in the above context, and not relying on
  the value of the parameter after the call to yypush_parse,
  in addition to fixing the order of the arguments in my
  call to yypush_parse.
  */
input:
  term { *result = $1; }

term:
    type    { $$ = $1; }
  | name    { $$ = $1; }
  | lambda  { $$ = $1; }
  | call    { $$ = $1; }
  | bind    { $$ = $1; }
/*  | subterm { $$ = $1; } */


type:
    NIL                 { $$ = CreateAstTypeNil(); }
  | type RARROW type    { $$ = CreateAstTypeFn($1, $3); }
  | LPAREN type RPAREN  { $$ = $2; }

name: /* [a-zA-Z][a-zA-Z0-9_-]+ */
  ID  { $$ = CreateAstId($1); }

lambda: /* \ name : arg_type => term */
    BSLASH ID COLON type REQARROW term { $$ = CreateAstLambda($2, $4, $6); }
  | BSLASH ID REQARROW term            { $$ = CreateAstLambda($2, CreateAstTypeInfer(), $4); }

call: /* term term */
  term term { $$ = CreateAstCall($1, $2); }

bind: /* name := term */
  ID COLONEQUALS term { $$ = CreateAstBind($1, $3); }

/*
subterm:
    LBRACE term RBRACE  { $$ = $2; }
*/
%%

/* epilogue */
void yyerror (YYLTYPE* lloc, Ast** result, yyscan_t scanner, char const* s)
{

  fprintf (stderr, "%s\n", s);

}
