
/* prologue */
%{


#include "type.h"
#include "ast.h"

%}
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
  the type here needs to be able to directly
  represent each node in the grammar.
  of which there are 5.

  in the trivial calculator, we can get by with
  each term having the same type. namely int.

  here we need to express the abstract syntax tree
  in terms of a c-style union. which is an untagged
  union. since we are compiling in c11 mode I think,
  we can specify unnammed unions in the ast, so we can
  just say, AstTypeName.nil instead of refrencing it
  through AstTypeName.union.nil
  making our declarations here look more like we are using
  the regular bison union, but each AstNode also tracks its
  kind and type making the rest of the compilers job more
  straightforward.

  to paraphrase the bison documentation

  %nterm <typename> nonterminal

  this statement declares the nonterminal to be represented
  by the name 'typename' within the union type 'api.value.type'.
  additionally any number of nonterminals can be declared here
  and all will have the specified type

  my question:
    what is the type bison selects for %token directives that
    are used trivially within the grammar? especially in the
    circumstance in which I have specified my own custom type
    in api.value.type

  the best response I've found from the documentation:
  {from: https://www.gnu.org/software/bison/manual/html_node/Mfcalc-Declarations.html}
    "Since values can now have various types, it is necessary to
     associate a type with each grammar symbol whose semantic value is used."
     "The Bison construct %nterm is used for declaring
     nonterminal symbols, just as %token is used for declaring
     token types. Previously we did not use %nterm before because
     nonterminal symbols are normally declared implicitly by the
     rules that define them. ~~~But exp must be declared explicitly
     so we can specify its value type~~~"

  My assumed answer:
    don't worry about it unless you are relying on the terminal/nonterminal
    symbol having some particular type, in which case specify
    that type with a %nterm, %token, or %type directive

  this raises an additional question,
    if 'typename' is the expected name of the union
    field associated with the terminal/nonterminal symbol,
    then what about places where we parse some sub-field
    of an entity?
    such as, i am reading the name of the argument, i would
    like that stored as a trivial char* until i can record it
    into the full lambda ast node, after consuming that input.

    the way the documentation is worded makes me assume that
    even if i add some new terminal symbol it will be represented
    by a full Ast node by bison? or maybe they secretly aren't
    but only nonterminal lhs values are constructed as Ast nodes.
*/
%define api.value.type Ast

/*
  each of the nonterminal symbols within the grammar is associated
  with a field within the structure of the Ast
*/
/* this line is sus */
%nterm <Ast> term


%nterm <nil> nil
%nterm <name> name
%nterm <lambda> lambda
%nterm <call> call
%nterm <bind> bind


%token NIL
%token <name> ID
%token COLON
%token COLONEQUALS
%token SEMICOLON
%token BSLASH
%token LARROW




%%
/* grammar */


term:
    nil
  | name
  | lambda
  | call
  | bind


nil: /* nil */
  NIL { $$ = CreateAstNil(); }

name: /* [a-zA-Z][a-zA-Z0-9_-]+ */
  ID  { $$ = Create}

lambda: /* \ ID : TYPE -> term */
  BSLASH ID COLON ID LARROW term {}

call: /* term term */
  term term {}

bind: /* ID := term */
  name COLONEQUALS term {}





%%

/* epilogue */
