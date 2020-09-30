
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

extern bool traced;

Parser* createParser()
{
  Parser* result    = (Parser*)malloc(sizeof(Parser));
  result->markstack = NULL;
  result->tokbuf    = NULL;
  result->txtbuf    = NULL;
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
    free(p->txtbuf[i]);
  free(p->txtbuf);
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
  if (p->txtbuf != NULL) {
    for (int i = 1; i < p->bufsz; ++i)
      free(p->txtbuf[i]);
    free(p->txtbuf);
    p->txtbuf = NULL;
  }

  p->idx = 0;
  p->mkstsz = 0;
  p->bufsz = 0;
  p->end = false;
}

void printParsedTokens(Parser* p)
{
  for (int i = 0; i < p->bufsz; i++) {
    printf("[%s]", tokenToString(p->tokbuf[i]));
  }
  printf("\n");
  /*
  for (int i = 0; i < p->bufsz; i++) {
    printf("[%s]", p->txtbuf[i]);
  }
  printf("\n");
  */
}

char* tokenToString(Token t)
{
  char* result = NULL;
  switch(t) {
    case ERR:
      result = "ERR";
      break;
    case END:
      result = "END";
      break;
    case NEWLN:
      result = "NEWLN";
      break;
    case MORE:
      result = "MORE";
      break;
    case OPERATOR:
      result = "OP";
      break;
    case NIL:
      result = "NIL";
      break;
    case NIL_TYPE:
      result = "NIL_TYPE";
      break;
    case INT:
      result = "INT";
      break;
    case INT_TYPE:
      result = "INT_TYPE";
      break;
    case TRUE:
      result = "TRUE";
      break;
    case FALSE:
      result = "FALSE";
      break;
    case BOOL_TYPE:
      result = "BOOL_TYPE";
      break;
    case IF:
      result = "IF";
      break;
    case THEN:
      result = "THEN";
      break;
    case ELSE:
      result = "ELSE";
      break;
    case ID:
      result = "ID";
      break;
    case COLON:
      result = "COLON";
      break;
    case COLONEQUALS:
      result = "COLONEQUALS";
      break;
    case BSLASH:
      result = "BSLASH";
      break;
    case REQARROW:
      result = "REQARROW";
      break;
    case SEMICOLON:
      result = "SEMICOLON";
      break;
    case LPAREN:
      result = "LPAREN";
      break;
    case RPAREN:
      result = "RPAREN";
      break;
    case LBRACE:
      result = "LBRACE";
      break;
    case RBRACE:
      result = "RBRACE";
      break;
  }
  return result;
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
  if (p->txtbuf == NULL)
    return NULL;

  return p->txtbuf[p->idx];
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
    p->txtbuf = (char**)realloc(p->txtbuf,  sizeof(char**) * (p->bufsz + n));
    p->locbuf = (StrLoc*)realloc(p->locbuf, sizeof(StrLoc) * (p->bufsz + n));

    for (int j = 0; j < n; ++j) {
       /*
          grab an actual new token from the internal buffer
       */
       Token t = ERR;

       t = yylex(p, s);

       p->tokbuf[p->bufsz + j] = t;
       p->txtbuf[p->bufsz + j] = yytext(s);
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
  new functions, and thusly abstract over our own expressions.
  but what if we want to declare new 'units of work.'
  and build expressions up over those new units? (to
  further abstract over.)
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
    case INT:    case INT_TYPE: case BOOL_TYPE:
    case TRUE:   case FALSE:    case IF:
    case NIL:    case NIL_TYPE: case ID:
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
    case RPAREN: case END: case NEWLN:
    case RBRACE: case REQARROW: case THEN:
    case ELSE: case IF:
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

  if (good_term && traced) {
    printf("speculatively parsed: ");
    printParsedTokens(parser);
  }


  if (good_term)
    result = parse_term(parser, scanner);
  else {
    printf("bad term.\n");
  }


  if (traced && result != NULL) {
    char* s = AstToString(result);
    printf("parsed Ast: [%s]\n", s);
    free(s);
  }

  return result;
}

/*

*/


Ast* parse_if(Parser* p, Scanner* s);
Ast* parse_lambda(Parser* p, Scanner* s);
Ast* parse_binop(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc);
Ast* parse_infix_expr(Parser* p, Scanner* s, Ast* lhs, StrLoc* lhsloc, int minPrec);
Ast* parse_primary(Parser* p, Scanner* s);

Ast* parse_term(Parser* p, Scanner* s)
{
  Ast*       lhs = NULL, *rhs = NULL;
  Token     ctok = curtok(p);
  char*     ctxt = curtxt(p);
  StrLoc* rhsloc = NULL;
  StrLoc* lhsloc = curloc(p);

  if (predicts_primary(ctok)) {
    lhs = parse_primary(p, s);
  }

  if (lhs != NULL) {
    ctok = curtok(p);
    ctxt = curtxt(p);
    if (predicts_primary(ctok)) {
      /*
        fold any number of
        lexically adjacent primary terms
        into a call tree.
        ensuring that:
        a b c d ... -->> ((a b) c) d ...
      */
      do {
        rhsloc = curloc(p);
        rhs = parse_primary(p, s);
        StrLoc call_loc;

        // update ctok after the call to parse_primary
        ctok = curtok(p)
        ;

        call_loc.first_line   = lhsloc->first_line;
        call_loc.first_column = lhsloc->first_column;
        call_loc.last_line    = rhsloc->last_line;
        call_loc.last_column  = rhsloc->last_column;

        lhs = CreateAstCall(lhs, rhs, &call_loc);
      } while (predicts_primary(ctok));
    }
    else if (predicts_binop(p, ctxt)) {
      lhs = parse_binop(p, s, lhs, lhsloc);
    }
    else if (predicts_end(ctok)) {

    }
  }

  if (ctok == ERR) {

  }

  return lhs;
}

Ast* parse_primary(Parser* p, Scanner* s)
{
  /*
  this function assumes the responsiblity
  of consuming single token primary terms.

  multiple token primary terms are consumed
  by their parsing function. but this function
  selects which primary term to parse based on
  it's prefix token. this needs to be the case
  for every primary term. to say that again
  in other words: all primary terms must be LL(1)
  distinguishable.

  this separation ensures we only eat tokens
  once we know what their use will be.

  and it lets the connective tissue be defined
  in terms of primary terms, and full expressions
  be recursively parsed/defined simply by calling
  parse_term again.

  ctok must be one of:
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
      ctok = curtok(p);
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
          printf("couldn't parse bound term.");
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

    case INT: {
      term = CreateAstEntityLiteralInt(atoi(ctxt), cloc);
      nexttok(p, s); // eat INT
      break;
    }

    case INT_TYPE: {
      term = CreateAstEntityTypeInt(cloc);
      nexttok(p, s); // eat "Int"
      break;
    }

    case TRUE: {
      term = CreateAstEntityLiteralBool(true, cloc);
      nexttok(p, s); // eat "true"
      break;
    }

    case FALSE: {
      term = CreateAstEntityLiteralBool(false, cloc);
      nexttok(p, s); // eat "false"
      break;
    }

    case BOOL_TYPE: {
      term = CreateAstEntityTypeBool(cloc);
      nexttok(p, s); // eat "Bool"
      break;
    }

    case IF: {
      term = parse_if(p, s);
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
        nexttok(p, s); // eat ')'

      } else {
        printf("expecting rparen after term.\n");
        // error: missing rparen after term
      }
      break;
    }

    case OPERATOR: {
      if (predicts_unop(p, ctxt)) {
        nexttok(p, s); // eat OPERATOR
        Ast* rhs = parse_primary(p, s); // parse the rhs primary
        StrLoc* rhsloc = curloc(p), unoploc;
        unoploc.first_line   = cloc->first_line;
        unoploc.first_column = cloc->first_column;
        unoploc.last_line    = rhsloc->last_line;
        unoploc.last_column  = rhsloc->last_column;
        term = CreateAstUnop(ctxt, rhs, &unoploc);
      } else {
        printf("operator in primary position [%s] is not a unary operator.\n", ctxt);
      }
      break;
    }

    case END:
      break;
    default: {
      printf("unexpected token.\n");
      // error: unknown token.
      break;
    }
  }

  return term;
}

Ast* parse_if(Parser* p, Scanner* s)
{
  Token ct = curtok(p);
  Ast *cond = NULL, *first = NULL, *second = NULL, *result = NULL;
  StrLoc* begin_loc = curloc(p), *end_loc = NULL;
  StrLoc cond_loc;

  if (ct == IF) {
    nexttok(p, s); // eat IF

    cond = parse_term(p, s);

    if (cond == NULL) {
      printf ("couldn't parse conditional.\n");
      return NULL;
    }

    ct = curtok(p);
    if (ct == THEN) {
      nexttok(p, s); // eat THEN

      first = parse_term(p, s);

      if (first == NULL) {
        printf("couldn't parse first term.\n");
        return NULL;
      }

      ct = curtok(p);
      if (ct == ELSE) {
        nexttok(p, s); // eat ELSE

        second = parse_term(p, s);

        if (second == NULL) {
            printf("couldn't parse second term.\n");
            return NULL;
        }

        end_loc = curloc(p);
        cond_loc.first_line   = begin_loc->first_line;
        cond_loc.first_column = begin_loc->first_column;
        cond_loc.last_line    = end_loc->last_line;
        cond_loc.last_column  = end_loc->last_column;
        result = CreateAstCond(cond, first, second, &cond_loc);
      }
    }
    else {
      // error, no then
    }
  }
  else {
    // error, no if.
  }


  return result;
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
        ct = curtok(p);
      } else {
        type = CreateAstEntityTypePoly();
      }

      if (ct == REQARROW) {
        nexttok(p, s); // eat '=>' which predicts the body of the function.

        body = parse_term(p, s);

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
      rhs    = parse_affix_expr(p, s, rhs, &rhsloc, lopPrec);
      loptok = curtok(p);
      loptxt = curtxt(p);
    }

    // create a new node of the tree by attaching the previous
    // good lhs and the result of collapsing the rhs.
    // in the case that we encountered some number of equal precedence
    // operations upon entities, we push each operation into the lhs
    // tree via this call and the while loop.

    StrLoc binoploc;
    binoploc.first_line   = lhsloc->first_line;
    binoploc.first_column = lhsloc->first_column;
    binoploc.last_line    = rhsloc.last_line;
    binoploc.last_column  = rhsloc.last_column;
    lhs = CreateAstBinop(strdup(optxt), lhs, rhs, &binoploc);
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
    the right ordering according to precedence. greedy meaning when it walks
    out one level of the tree, the next step is to see if we need to walk
    deeper, then, only when we can no longer walk deeper can we safely
    construct the binop node.
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
  they may also note how that call to parse_primary didn't 'dissappear',
  it was in fact factored out into the callers code.
  in this way we can predict between call expressions and
  binop expressions in an LL manner. because the common
  problem; "we can't know which is which without first
  parsing a term." is solved by the fact that the first
  term of both is identical (any primary term),
   and the tokens which predict
  one or the other after the first term are disjoint.
  (primary terms vs binop terms)
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
bool speculate_primary(Parser* p, Scanner* s);

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

  /*
  i think some of these recursive calls can be
  replaced by:
    1) a wrapping while (1) loop
    and then:
    2) a continue;
  because the speculation doesn't need to track the
  intermediate data so closely to construct the AST.
  then, on a error or success we can use a break;
  however in order to track
  an arbitrary number of nested parentheses
  we may still need the recursion in that case.
  because the stack-like nature of a recursive
  function naturally stores all of it's intermediate
  results, which gives us nested parenthesized expressions
  'for free' as it were.
  this is the same case for speculate_lambda, which itself
  needs to contain one or two terms. and the bind expression,
  and later, ifs, whiles and ;s.
  (i am more confident about converting the speculative
  parser into tail-recursive form, than the 'real' parser,
  because of the lack of data tracking.)
  */

  bool result = true;
  if (predicts_primary(curtok(p))) {
    result = speculate_primary(p, s);
  }

  if (result) {
    if (predicts_primary(curtok(p))) {
      do {
        result = speculate_primary(p, s);
        if (!result) break;
      } while (predicts_primary(curtok(p)));
    }
    else if (predicts_binop(p, curtxt(p))) {
      do {
        nexttok(p, s); // eat the binop token
        result = speculate_primary(p, s);
        if (!result) break;
      } while (predicts_binop(p, curtxt(p)));
    }
  }

  return result;
}

bool speculate_primary(Parser* p, Scanner* s)
{
  bool result = true;
  if (speculate(p, s, NIL));
  else if (speculate(p, s, NIL_TYPE));
  else if (speculate(p, s, INT));
  else if (speculate(p, s, INT_TYPE));
  else if (speculate(p, s, TRUE));
  else if (speculate(p, s, FALSE));
  else if (speculate(p, s, BOOL_TYPE));
  else if (speculate(p, s, ID)) {
    if (speculate(p, s, COLONEQUALS)) {
      result = speculate_term(p, s);
    }
  }
  else if (speculate(p, s, IF)) {
    if (speculate_term(p, s)) {
      if (speculate(p, s, THEN)) {
        if (speculate_term(p, s)) {
          if (speculate(p, s, ELSE)) {
            result = speculate_term(p, s);
          }
          else
            result = false;
        }
        else
          result = false;
      }
      else
        result = false;
    }
    else
      result = false;
  }
  else if (speculate(p, s, LPAREN)) {
    if (speculate_term(p, s)) {
      result = speculate(p, s, RPAREN);
    }
    else result = false;
  }
  else if (speculate(p, s, OPERATOR)) {
    result = speculate_primary(p, s);
  }
  else if (speculate_lambda(p, s))
  ;

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
