
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "lexer.h"
#include "parser.h"
#include "error.h"


Scanner* createScanner(FILE* in)
{
    Scanner* scnr = (Scanner*)malloc(sizeof(Scanner));
    memset (scnr->buf, 0, SCANNER_BUF_SZ + 1);
    scnr->yych = 0;
    scnr->end = scnr->cursor = scnr->token = scnr->marker = scnr->buf + SCANNER_BUF_SZ;
    scnr->yyaccept = 0;
    scnr->state = -1;
    if (in) {
      if (in == stdin)
        scnr->is_stdin = true;
      else
        scnr->is_stdin = false;

      scnr->yyin = in;
    }
    else {
      scnr->yyin = stdin;
      scnr->is_stdin = true;
    }
    return scnr;
}

void destroyScanner(Scanner* scanner)
{
  free(scanner);
}

void yysetbuffer(Scanner* scnr, char* text, int len)
{
    if (len > SCANNER_BUF_SZ)
        error_abort("cannot buffer input text, aborting", __FILE__, __LINE__);

    memset (scnr->buf, 0, SCANNER_BUF_SZ);
    memcpy (scnr->buf, text, len);

    scnr->cursor = scnr->marker = scnr->mrkctx = scnr->token = scnr->buf;
    /*
        I removed a (+ 1) from the end of this line,
        thinking that it may solve the issue where the
        lexer inserts the ERR token when lexing the
        NEWLINE at the end of the buffer (filled with
        text from the user via getline.)
    */
    scnr->end = scnr->buf + len;
}

int yyfill(Scanner* scanner)
{
  /*
    technically the new scanner never calls YYFILL by definition,
    so this function is always useless. but it is necessary if
    we ever want to write a stream frontend for the compiler.
    this function will be used to request more tokens from the
    user of an interpreter, or if the compiler needs more input
    from a file being compiled. but all of that will need to be
    decided later, for the various frontends. given that the
    extensions mentioned above are in fact usefull, we will leave
    this function alone until we need it.

    buf->[c0, c1, c2, .., ctoken-1, ctoken, .., cend-1, cend, .., cbuf+SCANNER_BUF_SZ]

    ->token points to the character just past the last token processed.
    (in other words, at the beginning of the next token to process.)
    the difference between the start of the array (->buf) and
    the end of the last token (->token) is the space currently
    being occupied by processed characters in the array.
    (it is the 'processed' text.)

    available is the slots in the buffer left which can be filled
    with more characters. it is the total slots in the array available
    minus the slots taken up by the unprocessed slots sitting
    between the end of the last token (->token) and the end of
    the buffered input (->end) (the 'unprocessed' characters as it were)

    so the goal of this function is to shift the unprocessed
    input left in the buffer to the bottom of the array, and
    fill the now available space with more input from the input FILE.
  */
  size_t charsRead = 0;
  size_t processed = scanner->token - scanner->buf;
  size_t available = SCANNER_BUF_SZ - (scanner->end - scanner->token);

  if (available < 1) return -1;

  memmove(scanner->buf, scanner->token, SCANNER_BUF_SZ - processed);
  scanner->end    -= processed;
  scanner->cursor -= processed;
  scanner->token  -= processed;

  if (scanner->is_stdin)
    printf(":> ");
  charsRead = getline(&(scanner->end), &available, scanner->yyin);

  if (charsRead < 0) {
    /*
    getline returns -1 after errors and eof;
    so distinguish between real errors and
    the known case of end of input.
     */
    if (!feof(scanner->yyin)) {
      perror("yylex:getline");
      exit(1);
    }
  }
  else {
    scanner->end += charsRead;
    scanner->end[0] = 0;
  }

  return charsRead;
}

char* yytext(Scanner* scanner)
{
    return strndup(scanner->token, scanner->cursor - scanner->token);
}

StrLoc* yylloc(Scanner* scanner)
{
    return (StrLoc*)(&(scanner->yylloc));
}


void update_location(StrLoc* llocp, char* token, int length)
{
    llocp->first_line   = llocp->last_line;
    llocp->first_column = llocp->last_column;

    for (int i = 0; i < length; i++) {
        if (token[i] == '\n') {
            llocp->last_line++;
            llocp->last_column = 1;
        }
        else {
            llocp->last_column++;
        }
    }
}



/*
typedef struct Scanner {
	char  buf[SCANNER_BUF_SZ + 1];
    char* end;
    char* cursor;
	char* marker;
    char* token;
	unsigned int yyaccept;
	int state;
	FILE* yyin;
	char  yych;
	bool  is_stdin;
	struct {
		int first_line;
		int first_column;
		int last_line;
		int last_column;
	} yylloc;
} Scanner;

    needs of re2c:

    yych holds current input character.
    YYCTYPE is the type of yych

    YYCURSOR is of type YYCTYPE*, and is used to search for matches

    YYLIMIT is of type YYCTYPE*, and is used to delimit the end of the buffer.

    YYMARKER is of type YYCTYPE* and is used to backup the cursor after a successful
                match, in certain circumstances.


from the re2c manual
the default scanner behavior in terms of the generic interface.
these are the basic scanner primitives which drive the algorithm:
#define  YYPEEK ()         *YYCURSOR
#define  YYSKIP ()         ++YYCURSOR
#define  YYBACKUP ()       YYMARKER = YYCURSOR
#define  YYRESTORE ()      YYCURSOR = YYMARKER
this is the check for the end of the input:
#define  YYLESSTHAN (n)    YYLIMIT - YYCURSOR < n
these primitives are only used to support specific scanner features
in re2c, specifically, s-tags and m-tags, which from the docs are
used to implement "staDFA delayed store actions". which I do not know
what that is, but I am not using them so eh.
the context (CTX) primitives are used to support the specific
regular expression R \ S which is an R followed by an S but the S
is not consumed. (which is why it takes another pointer to support.)
(notice how the scanner data structure doesn't even define this context
 ptr, because it doesn't need to.)
#define  YYBACKUPCTX ()    YYCTXMARKER = YYCURSOR
#define  YYRESTORECTX ()   YYCURSOR = YYCTXMARKER
#define  YYRESTORERAG (t)  YYCURSOR = t
#define  YYSTAGP (t)       t = YYCURSOR
#define  YYSTAGPD (t)      t = YYCURSOR - 1
#define  YYSTAGN (t)       t = NULL
*/


/*!re2c
    alpha      = [a-zA-Z];
    digit      = [0-9];
    alnum      = [alpha|digit];
    identifier = [a-zA-Z][-a-zA-Z0-9]*;
*/

/*
    we want to support reading from an input source
    that may not give us all of the input in a single go.
    because that makes extending the compiler to handle
    very large files, multiple input sources, and reading
    input from an unreliable source like a socket, possible.
    (making the parser with this semantics from the beginning
    should work to say the interpreter semantics neatly,
    (as the user may enter half an expression on one call to
    getline and the other half on the second call.)
    and allow extension to be easier. simply because there will
    be less to change to fit with the extensions (each a situation
    in which the full input doesn't fit in the memory buffer
    foor parsing.).)

    we could tie the scanner and lexer together if we imagine the existance
    of the ';' symbol with the meaning of [term ';' term] in the grammar.

    this gives us an explicit lexical symbol with the meaning 'end-of-term'.

    we then allow empty expressions; by allowing an ending
    token to appear immediately as a primary
    leading to said empty expression. we could
    have a user insert a semicolon to 'end' an expression in
    the language. and silently we would parse this as
    (user input) ';' empty

    we could also parse an empty buffer as an empty term.
    and that can stop the parse.

    but we need to be very careful. I don't want every
    lone term to parse forever.

    - enter nothing, parser returns an empty term
        - typechecker types it as nothing,
        - evaluator evaluates it to nothing,
    - enter something valid, keep trying to
        fill the buffer until we reach an end token.

    to recap:
    terms like "(\x=>x)" will cause the parser to return for
    more input unless ended with a ';' token. (essentially
    the parser greedily wants to assume you are going to
    postfix an application or binop after a valid term,
    as that is it's required behavior when parsing some long
    expression, we as programmers want to have as much
    control over what get's parsed as possible, so the explicit
    'period' character (homomorphically (what i would call metaphorically
    if it were between two entities in reality being described by language.))
     ";" makes the most sense.)
    and "" will cause the parser to end immediately.
    this allows a user to enter a single full term by
    ending it with a ';'. (and if they enter nothing
    the parser will end, and the program will execute
    default no-op behavior. which is also technically what it
    does when evaluating the rhs of the semicolon.)
    we won't immediately try and construct any node, as the
    yyfill call is implicitly associated with reaching the end
    of input from the lexer. whereby it returns MORE.
    i think this means that the trigger for yymore needs to
    be lifted out of the scanner by way of making it reentrant,
    and the parser needs to call yyfill itself when it encounters
    the MORE token.
    then we need to care about a yymore that doesn't get any more,
    and wether that situation is an error or not.
    which depends on where we are in a parse. (im not sure what
    exactly to base this boolean judgement on. some flag allocated
    for this purpose? some particular arrangement of the already
    allocated memory?)
*/



/*
    we use the Generic re2c Interface to allow us to
    store the lexer variables in our own structure,
    because the lexer will eventually be reentrant.
    and if the lexer is specified as reentrant, it
    expects the user to store them like this,
    (but this current use case is not documented at all.
        at least explicitly.)
*/
#define YYPEEK()       *scanner->cursor
#define YYSKIP()       ++(scanner->cursor)
#define YYBACKUP()     scanner->marker = scanner->cursor
#define YYRESTORE()    scanner->cursor = scanner->marker
#define YYBACKUPCTX()  scanner->mrkctx = scanner->cursor
#define YYRESTORECTX() scanner->cursor = scanner->mrkctx
#define YYLESSTHAN(n)  (scanner->end - scanner->cursor) < n

int yylex(Parser* parser, Scanner* scanner)
{
loop:
  scanner->token = scanner->cursor;
  /*!re2c
      re2c:define:YYCTYPE  = char;
      re2c:sentinel = -1;
      re2c:yyfill:enable = 0;
      re2c:eof = 0;

      *          { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return ERR; }
      $          { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return END; }
      [ \t]      { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); goto loop; }
      [\n]       { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return NEWLN; }
      "nil"      { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return NIL; }
      "Nil"      { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return NIL_TYPE; }
      [0-9]+     { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return INT; }
      "Int"      { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return INT_TYPE; }
      "true"     { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return TRUE; }
      "false"    { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return FALSE; }
      "Bool"     { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return BOOL_TYPE; }
      "if"       { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return IF; }
      "then"     { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return THEN; }
      "else"     { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return ELSE; }
      ":"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return COLON; }
      ":="       { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return COLONEQUALS; }
      "\\"       { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return BSLASH; }
      "=>"       { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return REQARROW; }
      /*
        notice how each of these binary operators returns the same token to the lexer.
        eventually we want to allow programmers to specify their own
        operators, which means we will want to lex arbitrary strings
        of special characters (probably [~`,.!@#$%^&*_-+=><?/]),
        this use case will subsume each of the
        underlying cases. but for now, we keep things simple.
      */
      "->"       { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return OPERATOR; }
      "+"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return OPERATOR; }
      "-"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return OPERATOR; }
      "*"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return OPERATOR; }
      "/"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return OPERATOR; }
      "%"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return OPERATOR; }
      ";"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return SEMICOLON; }
      "("        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return LPAREN; }
      ")"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return RPAREN; }
      "{"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return LBRACE; }
      "}"        { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return RBRACE; }
      identifier { update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return ID; }
  */
}








































/* ---------------------------------------------------------------- */
