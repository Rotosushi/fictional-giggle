
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
    | '\' id (':' term)? => term
    | term term
    | id := term
    | unop term
    | term binop term
    | '(' term ')'
    //| int_literal
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
  for (int i = 0; i < p->bufsz; ++i)
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

void resetParser(Parser* p)
{
  if (p->markstack != NULL) {
    free(p->markstack);
    p->markstack = NULL;
  }
  if (p->tokbuf != NULL) {
    free(p->tokbuf);
    p->tokbuf = NULL;
  }
  if (p->locbuf != NULL) {
    free(p->locbuf);
    p->locbuf = NULL;
  }
  if (p->texbuf != NULL) {
    for (int i = 1; i < p->bufsz; ++i)
      free(p->texbuf[i]);
    free(p->texbuf);
    p->texbuf = NULL;
  }

  p->idx = 0;
  p->mkstsz = 0;
  p->bufsz = 0;
  p->end = false;
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

    p->tokbuf = (Token*)realloc(p->tokbuf,  sizeof(Token) * (p->bufsz + n));
    p->texbuf = (char**)realloc(p->texbuf,  sizeof(char**) * (p->bufsz + n));
    p->locbuf = (StrLoc*)realloc(p->locbuf, sizeof(StrLoc*) * (p->bufsz + n));

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
    it is safe to reset the parser
    if we have parsed all available tokens
    and the parser is not speculating.
    otherwise we may still need to pick up
    tokens from the lexer to build up a full
    expression. or we may be speculating and
    need to reparse the tokbuf.
  */
  if (p->idx == p->bufsz && !speculating(p)) {
    resetParser(p);
  }

  fillTokens(p, s, 1);
}

bool predicts_unop(Parser* p, char* op)
{
  if (isMember(op, p->unopSet)) return true;
  else return false;
}

/*
  observant readers will note that binops
  are always dealt with using string comparison
  and not token comparison. this is because
  declaring new operators is something I consider
  essential to the language. This feature completes
  the cycle when we are considering the programmers
  ability to fold away logic. we can already define
  new functions, and abstract over our own expressions.
  but what if we want to declare new 'units of work.'
  and build expressions up over those new units?
  well, the basic unit of work is the operator.
  hence, operator declarations. and then naturally
  operator overloading, for when the unit of work
  is something that can be valid for more than one type.
  and again, we can hide redeclarations from the programmer
  via polymorphic declarations. (which are again more akin
  to c++ templates than HM parametric polymorphism)
*/
bool predicts_binop(Parser* p, char* op)
{
  if (isMember(op, p->binopSet)) return true;
  else return false;
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
  bool good_term = false;

  resetParser(parser);

  fillTokens(parser, scanner, 1);

  mark(parser);
  good_term = speculate_term(parser, scanner);
  release(parser);

  if (good_term)
    result = parse_term(parser, scanner);
  else {
    printf("bad term.\n");
  }

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
    basic units of composition. by restricting
    definitions to be decidable at compile time
    i think we can avoid introducing runtime typing,
    or runtime function definitions, simply to keep
    the semantics of the language as close to C
    as we reasonably want to get.
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



Ast* parse_lambda(Parser* p, Scanner* s);
Ast* parse_binop(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc);
Ast* parse_infix_expr(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc, int minPrec);
Ast* parse_primary(Parser* p, Scanner* s);

Ast* parse_term(Parser* p, Scanner* s)
{
  Ast* term      = NULL;
  Token ctok     = curtok(p);
  char* ctxt     = curtxt(p);
  StrLoc* lhsloc = curloc(p);

  if (predicts_primary(ctok)) {
    term = parse_primary(p, s);
  }

  if (term != NULL) {
    ctok = curtok(p);
    if (predicts_binop(p, ctxt)) {
      term = parse_binop(p, s, term, lhsloc);
    }
  }

  return term;
}

Ast* parse_primary(Parser* p, Scanner* s)
{
  /*
  this function assumes the responsiblity
  of consuming single token primary terms.

  multiple token primary terms are consumed
  by their parsing function.

  this separation ensures we only eat tokens
  once we know what their use will be.

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
      char* id = strdup(ctxt);
      nexttok(p, s); // eat ID

      if (ctok == COLONEQUALS) { // this is a bind term
        nexttok(p, s); // eat ':='

        Ast* bound_term  = parse_term(p, s);
        StrLoc* rightloc = curloc(p);
        StrLoc bindloc;
        bindloc.first_line   = cloc->first_line;
        bindloc.first_column = cloc->first_column;
        bindloc.last_line    = rightloc->last_line;
        bindloc.last_column  = rightloc->last_column;
        if (bound_term != NULL) {
          term = CreateAstBind(id, bound_term, &bindloc);
        } else {
          // error: couldn't parse bound term
        }

      } else { // this is just an ID by itself.
        term = CreateAstId(id, cloc);
      }
      break;
    }

    case NIL: {
      term = CreateAstEntityLiteralNil(cloc);
      nexttok(p, s); // eat 'nil'
      break;
    }

    case NIL_TYPE: {
      term = CreateAstEntityTypeNil(cloc);
      nexttok(p, s); // eat 'Nil'
      break;
    }

    case BSLASH: {
      term = parse_lambda(p, s);
      break;
    }

    case LPAREN: {
      nexttok(p, s); // eat '('

      term = parse_term(p, s);
      ctok = curtok(p);
      if (ctok == RPAREN) {
        nexttok(p, s);

      } else {
        // error: missing rparen after term
      }
      break;
    }
    default: {
      // error: unknown token.
      break;
    }
  }

  if (term != NULL) {
    ctok = curtok(p);
    if (predicts_primary(ctok)) {
      /*
        recursively fold any number of
        lexically adjacent primary terms
        into a call tree.
        ensuring that:
        a b c d -->> a (b (c d))
      */
      Ast* rhs = parse_primary(p, s);
      StrLoc* prvloc = cloc;
      StrLoc call_loc;
      cloc = curloc(p);

      call_loc.first_line   = prvloc->first_line;
      call_loc.first_column = prvloc->first_column;
      call_loc.last_line    = cloc->last_line;
      call_loc.last_column  = cloc->last_column;

      term = CreateAstCall(term, rhs, &call_loc);
    }
    // else we can just return the term we parsed
    // a single term by itself is a valid parse.
    // this is both the base case of recursion for
    // call expressions, and the regular case of
    // the code simply being a single term. in
    // both situations we simply want to silently
    // return whatever we parsed.
  } else {
    printf("primary term failed to parse.\n");
  }
  return term;
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

    nexttok(p, s); // eat '\'
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
    // the only time we call this function is
    // when we have already predicted a BSLASH tho...
  }

  return lambda;
}

Ast* parse_affix_expr(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc, int minPrecdnce)
{
  /*
  this is an implementation of an operator precedence parser.
  the pseudocode for which:

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
  // loptok, loptxt, lopPrec -->> (l)ookahead (op)erator ...
  Token loptok  = curtok(p);
  char* loptxt  = curtxt(p);
  int   lopPrec = -1;

  while (predicts_binop(p, loptxt) && (lopPrec = LookupOpPrec(p->pTable, loptxt)) >= minPrecdnce) {
    Token optok = loptok;
    char* optxt = loptxt;

    nexttok(p, s); // eat the previous op
    StrLoc* llocp = curloc(p);
    StrLoc rhsloc;

    rhsloc.first_line   = llocp->first_line;
    rhsloc.first_column = llocp->first_column;
    // rhs := parse_primary()
    Ast* rhs = parse_primary(p, s); // parse the primary term
    llocp = curloc(p);
    rhsloc.last_line   = llocp->last_line;
    rhsloc.last_column = llocp->last_column;

    // collapse all rhs expressions which rotate a new top of the tree into
    // a single rhs, (this means operators with strictly higher precedence,
    // and right associative operators with equal precedence.) in both
    // of these cases the rhs needs to be parsed as if it was a brand new lhs.
    /*
       op
    lhs  rhs

        op
    lhs     op
         rhs   rhs'

    */
    loptok = curtok(p);
    loptxt = curtxt(p);
    while (predicts_binop(p, loptxt) && ( \
        (LookupOpPrec(p->pTable, loptxt) > lopPrec) || \
        ((LookupOpPrec(p->pTable, loptxt) == lopPrec) && (LookupOpAssoc(p->pTable, loptxt) == A_RIGHT))))
    {
      rhs = parse_affix_expr(p, s, rhs, &rhsloc, lopPrec);
      loptok  = curtok(p);
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

  /*
    this algorithm parses as many tokens as it can until it
    is forced to construct the deepest tree node, deepest being,
    starting from the left of the expression, the first highest
    precedence operator in the equation, and that rhs primary term.
    it essentially
    walks the tokens, using function calls as an implicit stack to
    store the intermediate data and then works it's way upwards
    always being as greedy as it can so the resulting tree enforces
    the right ordering according to precedence.
    (parentheses have
    the highest precedence in the sense that they always count
    as a single primary term, so they, in reality, completely subvert
    this algorithm. subvert in the sense that parentheses don't have
    an explicit 'precedence', they are simply parsed as their own term.)
  */

  return lhs;
}

Ast* parse_binop(Parser* p, Scanner* s, Ast* firstlhs, StrLoc* lhsloc)
{
  /*
  observant readers may note that in the original implementation
  of parse_binop(), due to classes, parse_binop had
  no arguments, and instead called parse_primary itself in a similar
  call to the "real" parse_binop expression.
  i.e.
  Ast* parse_binop() {
    return _parse_binop(parse_primary(), 0);
  }
  we may also note how that call to parse_primary didn't 'dissappear',
  it was in fact factored out into the callers code.
  in this way we can predict between call expressions and
  binop expressions in an LL manner. because the common
  problem; "we can't know which is which without first
  parsing a term." is solved by the fact that the first
  term of both is identical (any primary term),
   and the tokens which predict
  one or the other after the first term are disjoint.
  (primary terms vs binop terms)
  additionally, call expressions are parsed in their
  own function taking advantage of the uniformity
  of terms present in a call expression. meaning that
  complex expressions comprising both calls, binary operators
  and no parenthesis will be parsed such that all calls
  happen before any binary operation, and from there we
  rely on operator precedence.
  */
    return parse_affix_expr(p, s, firstlhs, lhsloc, 0);
}

bool speculate(Parser* parser, Scanner* scanner, Token token)
{
  if (token == curtok(parser)) {
    nexttok(parser, scanner);
    return true;
  } else
    return false;
}

bool speculate_lambda(Parser* p, Scanner* s);

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

  if (speculate(p, s, NIL));
  else if (speculate(p, s, NIL_TYPE));
  else if (speculate(p, s, ID)) {
    if (speculate(p, s, COLONEQUALS)) {
      result = speculate_term(p, s);
    }
  }
  else if (speculate(p, s, LPAREN)) {
    if (speculate_term(p, s)) {
      if (speculate(p, s, RPAREN));
      else result = false;
    }
    else result = false;
  }
  else if (speculate_lambda(p, s));
  else result = false;

  if (result) {
    Token ctok = curtok(p);
    char* ctxt = curtxt(p);

    if (predicts_primary(ctok)) {
      result = speculate_term(p, s);
    }
    else if (predicts_binop(p, ctxt)) {
      /*
      a binary expression, modulo precedence,
      is essentially:
       primary-term (binop primary-term)*
      */
      nexttok(p, s); // eat binop
      result = speculate_term(p, s); // parse the rhs term of the binop
    }
  }

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
        result = speculate_term(p, s);
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
































/* -------------------------------------------------------------------------- */
