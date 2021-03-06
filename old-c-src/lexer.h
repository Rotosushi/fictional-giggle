#ifndef LEXER_H
#define LEXER_H
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "parser.h"

struct Parser;
struct StrLoc;

#define SCANNER_BUF_SZ 4096

/*
	this is a buffered scanner,
	buf, is the buffer
	end is the end of the filled slots in the buffer.
	cursor is used to gather slots into single tokens.
	token is the begining of the current token being processed.
	yyaccept, yych, and state are used internally by re2c for
	keeping track of it's state to allow for reentrancy.

*/
typedef struct Scanner {
	char  buf[SCANNER_BUF_SZ + 1];
  char* end;
  char* cursor;
	char* marker;
	char* mrkctx;
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


Scanner* createScanner(FILE* in);
void     destroyScanner(Scanner* scanner);
void     yysetbuffer(Scanner* scanner, char* text, int len);
int      yyfill(Scanner* scanner);
char*    yytext(Scanner* scanner);
struct StrLoc*  yylloc(Scanner* scanner);
int      yylex(struct Parser* parser, Scanner* scanner);

#endif
