/* Generated by re2c 1.3 on Wed Jun 24 14:42:42 2020 */
#line 1 "lexer.re"

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
    scnr->end = scnr->buf + len + 1;
}

int yyfill(Scanner* scanner)
{
  /*
    buf->[c0, c1, c2, .., ctoken-1, ctoken, .., cend-1, cend, .., cbuf+SCANNER_BUF_SZ]

    ->token points to the character just past the last token processed.
    (in other words, at the beginning of the next token to process.)
    the difference between the start of the array (->buf) and
    the end of the last token (->token) is the space currently
    being occupied by processed characters in the array.
    (its the 'processed' text.)

    available is the slots in the buffer left which can be fill
    with more characters. it is the total slots in the array available
    minus the slots taken up by the unprocessed slots sitting
    between the end of the last token (->token) and the end of
    the buffered input (->end) (the 'unprocessed' characters as it were)

    so the goal of this function is to shift the unprocessed
    input left in the buffer to the bottom of the array, and
    fill the available space with more input from the input FILE.
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


#define  YYPEEK ()         *YYCURSOR
#define  YYSKIP ()         ++YYCURSOR
#define  YYBACKUP ()       YYMARKER = YYCURSOR
#define  YYBACKUPCTX ()    YYCTXMARKER = YYCURSOR
#define  YYRESTORE ()      YYCURSOR = YYMARKER
#define  YYRESTORECTX ()   YYCURSOR = YYCTXMARKER
#define  YYRESTORERAG (t)  YYCURSOR = t
#define  YYLESSTHAN (n)    YYLIMIT - YYCURSOR < n
#define  YYSTAGP (t)       t = YYCURSOR
#define  YYSTAGPD (t)      t = YYCURSOR - 1
#define  YYSTAGN (t)       t = NULL


*/

#line 190 "lexer.re"


#define YYPEEK()       *scanner->cursor
#define YYSKIP()       ++(scanner->cursor)
#define YYBACKUP()     scanner->marker = scanner->cursor
#define YYRESTORE()    scanner->cursor = scanner->marker
#define YYBACKUPCTX()  scanner->mrkctx = scanner->cursor
#define YYRESTORECTX() scanner->cursor = scanner->mrkctx
#define YYLESSTHAN(n)  (scanner->end - scanner->cursor) < n

int yylex(Parser* parser, Scanner* scanner)
{

  scanner->token = scanner->cursor;
loop:
  
#line 205 "lexer.c"
{
	char yych;
	yych = YYPEEK ();
	switch (yych) {
	case '\t':
	case '\n':
	case '\r':
	case ' ':	goto yy4;
	case '(':	goto yy6;
	case ')':	goto yy8;
	case '-':	goto yy10;
	case ':':	goto yy11;
	case ';':	goto yy13;
	case '=':	goto yy15;
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':	goto yy16;
	case 'N':	goto yy19;
	case '\\':	goto yy20;
	case 'n':	goto yy22;
	case '{':	goto yy23;
	case '}':	goto yy25;
	default:	goto yy2;
	}
yy2:
	YYSKIP ();
yy3:
#line 211 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return ERR; }
#line 282 "lexer.c"
yy4:
	YYSKIP ();
#line 210 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); goto loop; }
#line 287 "lexer.c"
yy6:
	YYSKIP ();
#line 220 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return LPAREN; }
#line 292 "lexer.c"
yy8:
	YYSKIP ();
#line 221 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return RPAREN; }
#line 297 "lexer.c"
yy10:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case '>':	goto yy27;
	default:	goto yy3;
	}
yy11:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case '=':	goto yy29;
	default:	goto yy12;
	}
yy12:
#line 214 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return COLON; }
#line 315 "lexer.c"
yy13:
	YYSKIP ();
#line 219 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return SEMICOLON; }
#line 320 "lexer.c"
yy15:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case '>':	goto yy31;
	default:	goto yy3;
	}
yy16:
	YYSKIP ();
	yych = YYPEEK ();
yy17:
	switch (yych) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':	goto yy16;
	default:	goto yy18;
	}
yy18:
#line 224 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return ID; }
#line 400 "lexer.c"
yy19:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'i':	goto yy33;
	default:	goto yy17;
	}
yy20:
	YYSKIP ();
#line 216 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return BSLASH; }
#line 412 "lexer.c"
yy22:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'i':	goto yy34;
	default:	goto yy17;
	}
yy23:
	YYSKIP ();
#line 222 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return LBRACE; }
#line 424 "lexer.c"
yy25:
	YYSKIP ();
#line 223 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return RBRACE; }
#line 429 "lexer.c"
yy27:
	YYSKIP ();
#line 217 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return BINOP; }
#line 434 "lexer.c"
yy29:
	YYSKIP ();
#line 215 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return COLONEQUALS; }
#line 439 "lexer.c"
yy31:
	YYSKIP ();
#line 218 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return REQARROW; }
#line 444 "lexer.c"
yy33:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'l':	goto yy35;
	default:	goto yy17;
	}
yy34:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 'l':	goto yy37;
	default:	goto yy17;
	}
yy35:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':	goto yy16;
	default:	goto yy36;
	}
yy36:
#line 213 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return NIL_TYPE; }
#line 530 "lexer.c"
yy37:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':	goto yy16;
	default:	goto yy38;
	}
yy38:
#line 212 "lexer.re"
	{ update_location((StrLoc*)&scanner->yylloc, scanner->token, scanner->cursor - scanner->token); return NIL; }
#line 602 "lexer.c"
}
#line 225 "lexer.re"

}








































/* ---------------------------------------------------------------- */
