
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "parser.h"
#include "lexer.h"
#include "pink_kernel.h"
#include "precedencetable.h"
#include "stringset.h"
#include "ast.h"
#include "error.h"

/*
term:
      id
    | nil
    | Nil
    | lambda
    | term term
    | id := term
    | '(' term ')'
    | term binop term
    //| int
    //| nil_type
    //| int_type
    //| binop
    //| unop
*/


Parser* createParser()
{
  Parser* result    = (Parser*)malloc(sizeof(Parser));
  result->markstack = NULL;
  result->tokbuf    = NULL;
  result->texbuf    = NULL;
  result->locbuf    = NULL;
  result->idx       = 0;
  result->mkstsz    = 0;
  result->bufsz     = 0;
  result->end       = false;
  result->pTable    = CreatePrecedenceTable();
  result->binopSet  = createStringSet();
  result->unopSet   = createStringSet();
  InitializePrecedenceTable(result->pTable);
  InitializeBinops(result->binopSet);
  InitializeUnops(result->unopSet);
  return result;
}

void destroyParser(Parser* p)
{
  free(p->markstack);
  free(p->tokbuf);
  free(p->locbuf);
  for (int i = 1; i < p->bufsz; ++i)
    free(p->texbuf[i]);
  free(p->texbuf);
  DestroyPrecedenceTable(p->pTable);
  DestroyStringSet(p->binopSet);
  DestroyStringSet(p->unopSet);
  p->idx = 0;
  p->mkstsz = 0;
  p->bufsz = 0;
  free(p);
}


int     mark(Parser* p)
{
  /*
    push the current index onto the
    mark 'stack'

    ms -> [i0, i1, .., in]
    =>
    ms -> [i0, i1, .., in, in+1]
  */
  if (p->markstack == NULL) {
    p->markstack = (int*)malloc(sizeof(int));
    p->mkstsz = 1;
  } else {
    p->mkstsz += 1;
    p->markstack = (int*)realloc(p->markstack, sizeof(int) * p->mkstsz);
  }
  p->markstack[p->mkstsz-1] = p->idx;

  return p->idx;
}

void release(Parser* p)
{
  /*
    pop the top mark off the
    mark 'stack'

    ms -> [i0, i1, .., in-1, in]
    =>
    ms -> [i0, i1, .., in-1]
  */
  if (p->markstack == NULL) {
    return;
  } else {
    int mark = p->markstack[p->mkstsz-1];
    p->mkstsz -= 1;
    p->markstack = (int*)realloc(p->markstack, sizeof(int) * p->mkstsz);
    p->idx = mark;
  }
}

Token curtok(Parser* p)
{
  if (p->tokbuf == NULL)
    return END;

  return p->tokbuf[p->idx];
}

char* curtxt(Parser* p)
{
  if (p->texbuf == NULL)
    return NULL;

  return p->texbuf[p->idx];
}

StrLoc* curloc(Parser* p)
{
  if (p->locbuf == NULL)
    return NULL;

  return &p->locbuf[p->idx];
}

bool speculating(Parser* p)
{
  return p->mkstsz > 0;
}

void fillTokens(Parser* p, Scanner* s, int i)
{
  /*
    add (i) tokens to the buffer of tokens
    currently being processed.
  */
  if (p->idx + i > p->bufsz) {       // do we need more tokens than we have?
    int n = (p->idx + i) - p->bufsz; // how many more do we need exactly?
    //char* tkbf = NULL *txbf = NULL, *lcbf = NULL;
    // add (n) more slots to each buffer.

    p->tokbuf = (Token*)realloc(p->tokbuf,  sizeof(Token*) * p->bufsz + n);
    p->texbuf = (char**)realloc(p->texbuf,  sizeof(char**) * p->bufsz + n);
    p->locbuf = (StrLoc*)realloc(p->locbuf, sizeof(StrLoc*) * p->bufsz + n);

/*
    if (p->tokbuf != NULL && p->txbuf != NULL && p->lcbf) {
      tkbf = (Token*)malloc(sizeof(Token*) * (p->bufsz + n));
      txbf = (char**)malloc(sizeof(char**) * (p->bufsz + n));
      lcbf = (StrLoc*)malloc(sizeof(StrLoc*) * (p->bufsz + n));
    }
*/
    for (int j = 0; j < n; ++j) {
       /*
          grab an actual new token from the input FILE
       */
       Token t = ERR;

       t = yylex(p, s);

       p->tokbuf[p->bufsz + j] = t;
       p->texbuf[p->bufsz + j] = yytext(s);
       p->locbuf[p->bufsz + j] = *yylloc(s);
    }

    p->bufsz += n;
  }

}

void nexttok(Parser* p, Scanner* s)
{
  p->idx += 1;

  /*
    it is safe to reset the token buffer
    if we have parsed all available tokens
    and the parser is not speculating.
    otherwise we may still need to pick up
    tokens from the lexer to build up a full
    expression.
  */
  if (p->idx == p->bufsz && !speculating(p)) {
    for (int i = 0; i < p->bufsz; i++) {
      free(p->texbuf[i]);
    }

    free(p->tokbuf);
    free(p->texbuf);
    free(p->locbuf);
    p->bufsz = 0;
    p->idx = 0;
  }

  fillTokens(p, s, 1);
}

bool predicts_unop(Parser* p, char* op)
{
  if (isMember(op, p->unopSet)) return true;
  else return false;
}

bool predicts_binop(Parser* p, char* op)
{
  if (isMember(op, p->binopSet)) return true;
  else return false;
}

bool predicts_entity(Token t)
{
  switch (t) {
    case NIL: case NIL_TYPE:
    case BSLASH:
    {
      return true;
    }

    default:
    {
      return false;
    }
  }
}

bool predicts_type(Token t)
{
  switch(t) {
    case NIL_TYPE: case LPAREN:
      return true;
    default:
      return false;
  }
}

bool predicts_literal(Token t)
{
  switch(t) {
    case NIL: case BSLASH:
    {
      return true;
    }
    default:
    {
      return false;
    }
  }
}

bool predicts_primary(Token t)
{
  /*
  primary   := id
             | entity
             | unop-expr
             | parens
  */
  switch (t) {
    case NIL: case NIL_TYPE: case ID:
    case LPAREN: case BSLASH:
    {
      return true;
    }

    default:
    {
      return false;
    }
  }
}

bool predicts_end(Token t)
{
  switch(t) {
    case RPAREN: case END:
    case RBRACE: case REQARROW:
      return true;
    default:
      return false;
  }
}

Ast* parse_term(Parser* p, Scanner* s);
bool speculate_term(Parser* p, Scanner* s);

Ast* parse(Parser* parser, Scanner* scanner)
{
  Ast* result = NULL;
  parser->end = false;

  fillTokens(parser, scanner, 1);

  //mark(parser);
  //bool term = speculate_term(parser, scanner);
  //release(parser);

  result = parse_term(parser, scanner);

  return result;
}

/*
    these few expressions are what really
    is the driving force behind what the language
    is to me. if we consider the expressive power
    of infix expressions plus function application.
    we have arrived at some percentage of the
    expressive power of both imperitive and functional
    languages. this core is extended with subtyping, polymorphism,
    modules/abstract types, and concurrency. but
    the basic framework of specifying a program
    flows from primitive operations, and
    the ability to compose those operations together.
    the ability to describe new operations by way
    of some composition of known primitives is the
    focus of the kernel of the language. the basic
    unit of abstraction is the function.
    operations are the basic unit of work.
    expressions and function application are the
    basic units of composition.
    types and the type system is what maintains
    the alignment between what expressions can validly
    be stated by the grammar and what expressions can be
    validly expressed by the runtime.

    a + b c - d
    -->> (+ a (- (b c) d))

    b c d e * f g h i
    -->> (* (b d c e) (f g h i))

    a b + c d * e f - g h
    -->> (+ (a b) (- (* (c d) (e f)) (g h))

    (a binop b) (c binop d binop e)
    -->>(binop a b) (binop c (binop d e))
*/


Ast* parse_literal(Parser* p, Scanner* s);
Ast* parse_lambda(Parser* p, Scanner* s);
Ast* parse_type(Parser* p, Scanner* s);
Ast* parse_entity(Parser* p, Scanner* s);
Ast* parse_call(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc);
Ast* parse_binop(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc);
Ast* parse_infix_expr(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc, int minPrec);
Ast* parse_unop(Parser* p, Scanner* s);
Ast* parse_parens(Parser* p, Scanner* s);
Ast* parse_primary(Parser* p, Scanner* s);

Ast* parse_term(Parser* p, Scanner* s)
{
  Ast* term      = NULL;
  Token ct       = curtok(p);
  StrLoc* lhsloc = curloc(p);
/*
  a term is either:
  a variable
  a literal
  an application
  an affix expression
  where
  a literal can be a function literal,
    or the literal value nil. (this is also
    where we find numeric literals, character and
    string literals, and so on.)
  an application is any valid variable/literal
    next to another valid variable/literal
  and an expression is composed of and valid
  variable/literal, followed by a known binop,
  followed by another valid variable/literal.
  we can distinguish between an application
  and a binop only after parsing the first valid
  term. however, since application is always higher
  precedence than operation we choose to always parse terms
  into an application sequence starting from when parse_primary
  is called to the point in the tokens in which we recognize
  a binop, or an end-of-term token, such as RPAREN,
  or REQARROW, or END. then, when that call to parse_primary
  returns it can be collected into a call expression by the
  caller. which is only ever because parse_primary itself called
  parse_primary, specifically because it predicted another
  primary term after it parsed a primary term. this recursion
  needs to bottom out when we see a binop or another end-of-term token

  if primary terms subsume recognizing function application,
  recognizing literals, and recognizing variables.
  then we can describe binary operations
  in terms of a primary term followed by a binop,
  followed by another primary term.
  this also allows  call expressions to be built
  up recursively by way of predict_primary vs. predict_end.
  if we predict another primary term then we construct a
  call node from the previously parsed term, and the node
  created when we parse the predicted primary expression.
  if we predict the end of the expression then the result
  term is what was already parsed and we can simply return.
  whatever 'end' token we saw will be handled by some caller.
  as an example:
  if we imagine parsing some call expression of length (n)
  t0 t1 t2 ... tn
  t0 is parsed by parse_primary, then the function
  predicts another primary so we prepare to construct the
  first call node by calling parse_primary again to construct
  the rhs of the node. then we parse t1, and again,
  predict another primary term, so we prepare to construct
  the second call node by calling parse_primary again,
  and so on until we parse tn, and predict the end of the expression.
  then we place tn-1 and tn into the lhs and rhs of the bottom-most
  call node and return the call node to the node above,
  and then that call node has it's rhs ready, and so on until we build
  up the entire tree.

  then, each of the literals can be parsed by their own function
  and it is simply a matter of directing the creation of the correct
  entity node, and connecting these entities together by means of
  the connective tissue nodes, operations and applications.
*/

  if (predicts_primary(ct)) {
    term = parse_primary(p, s);
  }

  // when we get here, we have either parsed
  // a primary entity, or not.
  // if we have then term will point to the
  // AST node describing the term.
  // from this point curtok could be anything
  // if it is a binop, then we know we need to
  // construct some affix expression.
  // if it is anything else, we can do nothing
  // because we do not want to consume tokens
  // needlessly. the calling context should
  // take care of the end of an expression.
  // and if we could have seen another primary
  // term, that should have been parsed
  // already into a call expression. so the
  // only tokens that could appear after
  // a validly parsed primary term are
  // END, REQARROW, and RPAREN.

  if (term != NULL) {
    ct = curtok(p);
    if (predicts_binop(ct)) {
      term = parse_binop(p, s, term, lhsloc);
    }
  }

  return term;
}

Ast* parse_primary(Parser* p, Scanner* s)
{
  /*
  ct must be one of:
    NIL, NIL_TYPE, ID, LPAREN, BSLASH
  by definition of predicts_primary
  */
  Ast*    term = NULL;
  Token   ctok = curtok(p);
  char*   ctxt = curtxt(p);
  StrLoc* cloc = curloc(p);
  switch(ctok) {
    case ID: {

      break;
    }

    case NIL: {

      break;
    }

    case NIL_TYPE: {

      break;
    }

    case BSLASH: {

      break;
    }

    case LPAREN: {

      break;
    }
  }

  if (term != NULL) {
    ctok = curtok(p);
    ctxt = curtxt(p);
    cloc = curloc(p);
    if (predicts_primary(ctok)) {
      // construct a call node
    }
    // else we can just return the term we parsed
  } else {
    printf("primary term failed to parse.");
  }
  return term;
}

/*
Ast* parse_term(Parser* p, Scanner* s)
{
*/
  /*
      constant := nil
                | lambda

      lambda := \ id (: type)? => term

      type   := Nil
              | type '->' type

      entity := id
              | constant
              | type

      unop-expr := unop term

      parens    := '(' term ')'

      call      := term term

      primary   := entity
                 | unop-expr
                 | parens

      binop-expr := term binop term

      term := primary
            | binop-expr
            | call
            // | module
            // | abstract-type
  */


/*

  Ast* term = NULL;
  Token ct = curtok(p);
  StrLoc* termlhsloc = curloc(p), *termrhsloc = NULL;
  */
  /*
    if parse_term was called, we expect to find at least
    one primary expression in the series of tokens after.
    all valid expressions in the language are some form of
    operation upon entites. so we expect expressions to be
    composed of names or literal entities with the
    connective tissue of operations linking entities into
    full expressions.

    the head of any expression is called a primary expression.
    predicts_primary is the set of known tokens that can precede some
    valid expression. any literal value, any id, any unary operation
    or a parenthesis validly predicts a primary expression.
  */
  /*
  if (predicts_primary(ct)) {
    term = parse_primary(p, s);
    termrhsloc = curloc(p);
  }
*/
  /*
    if we have a valid term parsed then we
    still need to try and parse possible
    binop, call terms, or this could be the end
    of the expression.
    if the token after the
    valid term is a binop, that then predicts an infix expression
    and we need to parse an operator precedence expression,
    if the term predicts
    another primary term then we need to parse a call expr.
    because two primary terms sitting lexically adjacent is
    what constitutes a function application.
    if, immediately after some primary expression we encounter
    something predicting the end of an expression we can
    simply return what we have parsed, and leave the token for
    the calling context to handle. in the case of END the calling
    context should be, eventually, the root caller. in the case of
    RPAREN, RBRACE, and REQARROW the calling context expects to
    consume each of these tokens.
  */
  /*
  if (term != NULL) {
    switch (ct) {
      case END:
        p->end = true;
        return term;
      // in each of the cases we want to leave
      // the token for the calling function to consume.
      case RPAREN:
      case RBRACE:
      case REQARROW:
        return term;
      // we avoid doing anything given any other token.
      default:;
    }


    StrLoc LhsLoc;
    LhsLoc.first_line   = termlhsloc->first_line;
    LhsLoc.first_column = termlhsloc->first_column;
    LhsLoc.last_line    = termrhsloc->last_line;
    LhsLoc.last_column  = termrhsloc->last_column;
    ct = curtok(p);
    char* ctxt = curtxt(p);
    */
    /*
      in my previous implementation, it was always obvious
      that unless term has some clear prefix token, the
      parser needed to assume that the user was entering some
      affix expression. this has changed with the new
      syntax of the language, we now have two base conditions
      to choose from which constitute a continuing expression;
      affix expressions and applications. this is untenable
      until I realized that both base conditions range over
      disjoint sets of tokens, and the first primary term
      is always parsed before we can see the binop token,
      and when we are looking for a binop, and we see a call
      expression we can silently gather the full call term
      into a single expression.

      thinking about this some more, this seems wrong,
      if we imagine the call graph of the parser while it
      tracks along the term
      a b + c d * e f - g h
      -->> (+ (a b) (- (* (c d) (e f)) (g h))

      we gather the first call expression into the first primary term
      by way of parse_call, when parse_call calls parse_term to
      construct it's rhs after picking up 'b' into the first rhs
      we instead encounter the binop, which calls parse affix expression
      passing in the last primary which was b, this incorrectly
      associates b with the affix expression rather than the call
      expression, because this parser always greedily assumes
      more terms from this statrting point. instead of wanting
      to consume more tokens in both cases, I want to make application
      bind tighter than anything, and thereby make any application
      be parsed as a single term for any binop expression to operate
      on, the other way around doesn't really work in a sensible way.
      instead the first and last terms of an application would by default
      bind to the binop expressions before and after the call term.
      ex:
        a b + c d
      -->> (a ((+ b c) d))
      which again, is not what I want the expression to parse as.
      properly parsed a b + c d should be
      (+ (a b) (c d))
    */
    /*
    if (predicts_binop(p, ctxt)) {
      term = parse_binop(p, s, term, &LhsLoc);
    }
    else if (predicts_primary(ct)) {
      term = parse_call(p, s, term, &LhsLoc);
    } else {
      return term;
      // a term by itself
    }
  }
  else {
    // error: malformed term.
  }

  return term;
}
*/

Ast* parse_type(Parser* p, Scanner* s)
{
  /*
  type := Nil
  */
  Token ct  = curtok(p);
  Ast* type = NULL;
  StrLoc* lhsloc = NULL;

  if (ct == NIL_TYPE) {
    lhsloc = curloc(p);
    type = CreateAstEntityTypeNil(lhsloc);
    nexttok(p, s);
  }

  return type;
}

Ast* parse_literal(Parser* p, Scanner* s)
{
  /*
  literal := nil
            | lambda
  */
  Token ct = curtok(p);
  Ast* literal = NULL;

  if (ct == NIL) {
    literal = CreateAstEntityLiteralNil(curloc(p));
    // eat the nil
    nexttok(p, s);
  }
  else if (ct == BSLASH) {
    literal = parse_lambda(p, s);
  }
  else {
    // error: not a valid constant term
  }

  return literal;
}

Ast* parse_lambda(Parser* p, Scanner* s)
{
  /*
  lambda := \ id (: type)? => term
  */
  Token ct = curtok(p);
  Ast* type = NULL, *body = NULL, *lambda = NULL;
  StrLoc* begin_loc = NULL, *end_loc = NULL;
  StrLoc lambda_loc;
  char* arg_name = NULL;

  if (ct == BSLASH) {
    begin_loc = curloc(p);

    nexttok(p, s); // eat '\\'
    ct = curtok(p);

    if (ct == ID) {
      arg_name = curtxt(p); // save the text associated with the ID token

      nexttok(p, s); // eat ID
      ct = curtok(p);

      if (ct == COLON) {
        nexttok(p, s); // eat ':'
                                 // the colon predicts a type expression,
        type = parse_term(p, s); // so we parse it here.
      } else {
        type = CreateAstEntityTypePoly();
      }

      if (ct == REQARROW) {
        nexttok(p, s); // eat '=>' which predicts the body of the function.

        if (ct == LBRACE) { // an explicit scope can be denoted with enclosing '{''}'
          nexttok(p, s);

          body = parse_term(p, s);

          if (ct == RBRACE) {
            nexttok(p, s);
          } else {
            // error: missing rbrace after term
          }
        } else {
          body = parse_term(p, s);
        }

        if (body == NULL) {
          error_abort("unexpected NULL from parsed lambda body! aborting", __FILE__, __LINE__);
        }

        end_loc = curloc(p);

        lambda_loc.first_line   = begin_loc->first_line;
        lambda_loc.first_column = begin_loc->first_column;
        lambda_loc.last_line    = end_loc->first_line;
        lambda_loc.last_column  = end_loc->first_column;

        lambda = CreateAstEntityLiteralProc(strdup(arg_name), type, body, &lambda_loc);
      } else {
        // error: expecting an REQARROW before the body of the lambda
      }
    }
  } else {
    // error: only BSLASH predicts a lambda
  }

  return lambda;
}

Ast* parse_entity(Parser* p, Scanner* s)
{
  /*
  entity := type
          | literal
  */
  Token ct = curtok(p);
  Ast* entity = NULL;


  if (predicts_type(ct)) {
    entity = parse_type(p, s);
  }
  if (predicts_literal(ct)) {
    entity = parse_literal(p, s);
  }

  else {
    // error: malformed entity
  }

  return entity;
}

Ast* parse_call(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc)
{

  // a b c -->> a (b c)
  // a b c d e f g h -->> (a (b (c (d (e (f (g h)))))))
  Ast* rhs = NULL, *call = NULL;
  rhs = parse_term(p, s);
  StrLoc* rhsloc = curloc(p);

  if (rhs) {
    StrLoc callloc;
    callloc.first_line   = lhsloc->first_line;
    callloc.first_column = lhsloc->first_column;
    callloc.last_line    = rhsloc->first_line;
    callloc.last_column  = rhsloc->first_column;
    call = CreateAstCall(lhs, rhs, &callloc);
  } else {
    ; // error: malformed rhs.
  }

  return call;
}

Ast* parse_affix_expr(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc, int minPrecdnce)
{
  /*
  parse_expression_1(lhs, min_precedence)
  lookahead := peek next token
  while lookahead is a binary operator whose precedence is >= min_precedence
        op := lookahead
        advance to next token
        rhs := parse_primary ()
        lookahead := peek next token
        while lookahead is a binary operator whose precedence is greater
                 than op's, or a right-associative operator
                 whose precedence is equal to op's
            rhs := parse_expression_1 (rhs, lookahead's precedence)
            lookahead := peek next token
        lhs := the result of applying op with operands lhs and rhs
    return lhs
  */

  // peek at the operator that triggered this call of parse_affix_expr;
  Token lookahead   = curtok(p);
  char* loptxt      = curtxt(p);
  int   lopPrec     = -1;

  // collapse all lhs expressions which rotate a new top of the list
  // into a single lhs.
  while (predicts_binop(p, loptxt) && (lopPrec = LookupOpPrec(p->pTable, loptxt)) >= minPrecdnce) {
    Token optok = lookahead; // save the current top op of the tree
    char* optxt = loptxt;

    nexttok(p, s);
    StrLoc* llocp = curloc(p);
    StrLoc rhsloc;

    rhsloc.first_line   = llocp->first_line;
    rhsloc.first_column = llocp->first_column;
    // rhs := parse_primary()
    Ast* rhs   = parse_primary(p, s);
    llocp = curloc(p);
    rhsloc.last_line   = llocp->last_line;
    rhsloc.last_column = llocp->last_column;

    // collapse all rhs expressions which rotate a new top of the list into
    // a single rhs, (this means operators with strictly higher precedence,
    // and right associative operators with equal precedence.) in both
    // of these cases the rhs needs to be parsed as if it was a brand new lhs.
    /*
       op
    lhs  rhs

        op
    lhs     op
        lhs'   rhs

    */
    lookahead  = curtok(p);
    loptxt     = curtxt(p);
    while (predicts_binop(p, loptxt) &&  \
        ((LookupOpPrec(p->pTable, loptxt) > lopPrec) || \
        ((LookupOpPrec(p->pTable, loptxt) == lopPrec) && (LookupOpAssoc(p->pTable, loptxt) == A_RIGHT))))
    {
      rhs = parse_affix_expr(p, s, rhs, &rhsloc, lopPrec);
      lookahead  = curtok(p);
      loptxt     = curtxt(p);
    }

    // create a new node of the tree by attaching the previous
    // good lhs and the result of collapsing the rhs.
    // in the case that we encountered some number of equal precedence
    // operations upon entities, we push each operation into the lhs
    // tree via this call and the while loop.
    /*
       op
    lhs  rhs

        op
     op    rhs'
  lhs  rhs
    */
    StrLoc binoploc;
    binoploc.first_line   = lhsloc->first_line;
    binoploc.first_column = lhsloc->first_column;
    binoploc.last_line    = rhsloc.last_line;
    binoploc.last_column  = rhsloc.last_column;
    lhs = CreateAstBinop(optxt, lhs, rhs, &binoploc);
  }
  return lhs;
}

Ast* parse_binop(Parser* p, Scanner* s, Ast* firstlhs, StrLoc* lhsloc)
{
    return parse_affix_expr(p, s, firstlhs, lhsloc, 0);
}

Ast* parse_unop(Parser* p, Scanner* s)
{
  /*
  unop-expr := unop term
  */
  Token ct = curtok(p);
  char* ctxt = curtxt(p);
  Ast* result = NULL;

  if (predicts_unop(p, ctxt)) {
    /* nothing predicts_unop */
  }

  return result;
}

Ast* parse_parens(Parser* p, Scanner* s)
{
  /*
  parens := '(' term ')'
  */
  Token ct = curtok(p);
  Ast* result = NULL;
  if (ct == LPAREN) {
    nexttok(p, s);

    result = parse_term(p, s);
    ct = curtok(p);
    if (ct == RPAREN) {
      nexttok(p, s);

    } else {
      // error: missing rparen after term
    }
  } else {
    // error: missing lparen?
  }
  return result;
}

Ast* parse_primary(Parser* p, Scanner* s)
{
  /*
  primary   := id
             | id := term
             | entity
             | unop-expr
             | parens
             | primary primary
  */
  Token ct = curtok(p);
  char* ctxt = curtxt(p);
  Ast* result = NULL;

  if (ct == ID) {
    char*   id = curtxt(p);
    StrLoc* idloc = curloc(p);
    StrLoc* rhsloc = NULL;
    StrLoc  bindloc;
    Ast*    term = NULL;
    bool is_bind = false;

    nexttok(p, s); // eat ID
    ct = curtok(p);
    ctxt = curtxt(p);
    if (ct == COLONEQUALS) {
      is_bind = true;
      nexttok(p, s); // eat :=
      term = parse_term(p, s);
      rhsloc = curloc(p);
      bindloc.first_line   = idloc->first_line;
      bindloc.first_column = idloc->first_column;
      bindloc.last_line    = rhsloc->last_line;
      bindloc.last_column  = rhsloc->last_column;
    }

    if (is_bind) {
      result = CreateAstBind(id, term, &bindloc);
    } else {
      result = CreateAstId(id, idloc);
    }
  }
  if (predicts_entity(ct)) {
    result = parse_entity(p, s);
  }
  else if (predicts_unop(p, ctxt)) {
    result = parse_unop(p, s);
  }
  else if (ct == LPAREN) {
    result = parse_parens(p, s);
  }
  else {
    // error: malformed primary
  }

  if (result != NULL) {
    ct = curtok(p);
    if (predicts_primary(ct)) {
      Ast* rhs = parse_primary(p, s);
    } else if (predicts_end(ct)) {
      ;
    } else {
      // error: malformed primary
    }
  }

  return result;
}

bool speculate(Parser* parser, Scanner* scanner, Token token)
{
  if (token == curtok(parser)) {
    nexttok(parser, scanner);
    return true;
  } else
    return false;
}

bool speculate_constant(Parser* p, Scanner* s);
bool speculate_lambda(Parser* p, Scanner* s);
bool speculate_entity(Parser* p, Scanner* s);
bool speculate_binop(Parser* p, Scanner* s);
bool speculate_unop(Parser* p, Scanner* s);

bool speculate_term(Parser* p, Scanner* s)
{
  /*
  term :=
          nil
        | Nil
        | id
        | \ id (: type)? => term
        | term term
        | term binop term
        | unop term
        | '(' term ')'

        entity     -> id, nil, Nil, (\ id (: type)? => term)

        call       -> term term

        binop-expr -> term binop term

        unop-expr  -> unop term

        parens     -> '(' term ')'

  */

  bool result = true;
  // it = the token sequence being parsed
  if (speculate(p, s, ID)) {

  }
  else if (speculate_entity(p, s)) {   // is it an entity?

  }

  else if (speculate_unop(p, s)) {  // is it a unop expression?
    if (speculate_term(p, s));
    else {
      /* error: term after the unop is malformed */
      result = false;
    }
  }

  else if (speculate(p, s, LPAREN)) {
    if (speculate_term(p, s)) {
      if (speculate(p, s, RPAREN));
      else {
        /*  error: no postfix RPAREN after a well formed term */
        result = false;
      }
    }
    else {
      /* error: the parenthized term is malformed */
      result = false;
    }
  }

  else {
    // error: not an entity, a unop term or a LPAREN
    result = false;
  }

  if (result) {
    if (speculate_binop(p, s)) {  // is it an expression?
      if (speculate_term(p, s)); // is the rhs parsable?
      else {
        /* error: rhs of binop expr is malformed */
        result = false;
      }
    }
    else if (speculate_term(p, s)) { // is it a call?
        /*
          try and parse any number of terms in sequence

          pretty sure that the recursion here takes care of
          the 'any number' part of the above statement

          does this recur forever?
          it would if we didn't check for the well-formedness
          of the term above. then the terminating condition of
          END, RPAREN, RBRACE, wouldn't terminate and we would always
          call speculate_term above.
        */
    }
    else { // it's an entity by itself.
      if (predicts_end(curtok(p)))
        return result;
    }
  }

  return result;
}

bool speculate_constant(Parser* p, Scanner* s)
{
  bool result = true;
  if      (speculate(p, s, NIL));
  else if (speculate(p, s, NIL_TYPE));
  else result = false;
  return result;
}


bool speculate_lambda(Parser* p, Scanner* s)
{
  bool result = true;
  if (speculate(p, s, BSLASH)) {
    if (speculate(p, s, ID)) {
      if (speculate(p, s, COLON)) {
        if (speculate_term(p, s));
        else {
          // error: type annotation not well formed
          result = false;
        }
      }

      if (speculate(p, s, REQARROW)) {
        if (speculate(p, s, LBRACE)) {
          if (speculate_term(p, s)) {
            if (speculate(p, s, RBRACE));
            else
              // error: no trailing RBRACE
              result = false;
          }
          else
            // error: no well formed term after the LBRACE
            result = false;
        }
        else if (speculate_term(p, s));
        else {
          // error: lambda body not well formed
          result = false;
        }
      }
      else {
        // error: expected '=>' after ID or type annotation.
        result = false;
      }
    } else {
      // error: expected ID after '\'
      result = false;
    }
  } else {
    // error: expected '\' to start lambda term
    result = false;
  }
  return result;
}

bool speculate_entity(Parser* p, Scanner* s)
{
  bool result = true;
  if      (speculate_constant(p, s));
  else if (speculate_lambda(p, s));
  else result = false;
  return result;
}

bool speculate_binop(Parser* p, Scanner* s)
{
  bool result = true;
  if (speculate(p, s, RARROW));
  else result = false;
  return result;
}

bool speculate_unop(Parser* p, Scanner* s)
{
  bool result = true;
  result = false;
  return result;
}


































/* -------------------------------------------------------------------------- */
