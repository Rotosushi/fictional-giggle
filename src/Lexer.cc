/* Generated by re2c 1.3 on Thu Jul 23 14:03:08 2020 */
#line 1 "Lexer.re"
#include <string>
using std::string;

#include "Ast.hh"
#include "Parser.hh"

string dstr;

void Lexer::set_buffer(const string& str)
{
    buf = str;
    end = buf.end();
    cursor = marker = token = buf.begin();
    loc.first_line   = 0;
    loc.first_column = 0;
    loc.last_line    = 0;
    loc.last_column  = 0;
}

void Lexer::reset()
{
    buf.clear();
    end = buf.end();
    cursor = marker = token = buf.begin();
    loc.first_line   = 0;
    loc.first_column = 0;
    loc.last_line    = 0;
    loc.last_column  = 0;
}

void Lexer::update_location()
{
    int length = cursor - token;
    loc.first_line   = loc.last_line;
    loc.first_column = loc.last_column;

    for (int i = 0; i < length; i++)
    {
        if (token[i] == '\n') {
            loc.last_line++;
            loc.last_column = 1;
        } else {
            loc.last_column++;
        }
    }
}

string* Lexer::yytxt()
{
    /*
        this was once
        strndup(token, cursor - token);
        where token and cursor have type char*
        (and are pointing into the buffer of characters
        to lex.)

        so here we emulate said behavior by copying
        the series of characters from token until cursor
        we utilize the string constructor defined
        with two string iterators to achieve the same
        result.
    */
    return new string(token, cursor);
}

Location& Lexer::yyloc()
{
    return loc;
}

#line 75 "Lexer.re"


#define YYPEEK()      (*cursor)
#define YYSKIP()      (++cursor)
#define YYBACKUP()    (marker = cursor)
#define YYRESTORE()   (cursor = marker)
#define YYLESSTHAN(n) (n > (end - cursor))

Token Lexer::yylex()
{
    while(1) {
        token = cursor;
        
#line 88 "Lexer.cc"
{
	char yych;
	yych = YYPEEK ();
	switch (yych) {
	case '\t':
	case '\n':
	case ' ':	goto yy4;
	case '!':
	case '$':
	case '%':
	case '&':
	case '*':
	case '+':
	case '-':
	case '/':
	case '<':
	case '>':
	case '@':
	case '^':
	case '|':
	case '~':	goto yy6;
	case '(':	goto yy9;
	case ')':	goto yy11;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':	goto yy13;
	case ':':	goto yy16;
	case '=':	goto yy18;
	case 'A':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
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
	case '_':
	case 'a':
	case 'b':
	case 'c':
	case 'g':
	case 'h':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 'u':
	case 'v':
	case 'x':
	case 'y':
	case 'z':	goto yy19;
	case 'B':	goto yy22;
	case 'I':	goto yy23;
	case 'N':	goto yy24;
	case '\\':	goto yy25;
	case 'd':	goto yy27;
	case 'e':	goto yy28;
	case 'f':	goto yy29;
	case 'i':	goto yy30;
	case 'n':	goto yy31;
	case 't':	goto yy32;
	case 'w':	goto yy33;
	default:
		if (YYLESSTHAN (1)) {
			goto yyeof;
		}
		goto yy2;
	}
yy2:
	YYSKIP ();
yy3:
#line 92 "Lexer.re"
	{ update_location(); return Token::Error;       }
#line 189 "Lexer.cc"
yy4:
	YYSKIP ();
#line 94 "Lexer.re"
	{ update_location(); continue;                  }
#line 194 "Lexer.cc"
yy6:
	YYSKIP ();
	yych = YYPEEK ();
yy7:
	switch (yych) {
	case '!':
	case '$':
	case '%':
	case '&':
	case '*':
	case '+':
	case '-':
	case '/':
	case ':':
	case '<':
	case '=':
	case '>':
	case '@':
	case '^':
	case '|':
	case '~':	goto yy6;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy8;
	}
yy8:
#line 114 "Lexer.re"
	{ update_location(); return Token::Operator;    }
#line 224 "Lexer.cc"
yy9:
	YYSKIP ();
#line 107 "Lexer.re"
	{ update_location(); return Token::LParen;      }
#line 229 "Lexer.cc"
yy11:
	YYSKIP ();
#line 108 "Lexer.re"
	{ update_location(); return Token::RParen;      }
#line 234 "Lexer.cc"
yy13:
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
	case '9':	goto yy13;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy15;
	}
yy15:
#line 113 "Lexer.re"
	{ update_location(); return Token::Int;         }
#line 257 "Lexer.cc"
yy16:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case '!':
	case '$':
	case '%':
	case '&':
	case '*':
	case '+':
	case '-':
	case '/':
	case ':':
	case '<':
	case '>':
	case '@':
	case '^':
	case '|':
	case '~':	goto yy6;
	case '=':	goto yy34;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy17;
	}
yy17:
#line 109 "Lexer.re"
	{ update_location(); return Token::Colon;       }
#line 286 "Lexer.cc"
yy18:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy8;
	case '>':	goto yy36;
	default:	goto yy7;
	}
yy19:
	YYSKIP ();
	yych = YYPEEK ();
yy20:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	}
yy21:
#line 112 "Lexer.re"
	{ update_location(); return Token::Id;          }
#line 374 "Lexer.cc"
yy22:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'o':	goto yy38;
	default:	goto yy20;
	}
yy23:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'n':	goto yy39;
	default:	goto yy20;
	}
yy24:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'i':	goto yy40;
	default:	goto yy20;
	}
yy25:
	YYSKIP ();
#line 106 "Lexer.re"
	{ update_location(); return Token::Backslash;   }
#line 412 "Lexer.cc"
yy27:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'o':	goto yy41;
	default:	goto yy20;
	}
yy28:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'l':	goto yy43;
	default:	goto yy20;
	}
yy29:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'a':	goto yy44;
	default:	goto yy20;
	}
yy30:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'f':	goto yy45;
	default:	goto yy20;
	}
yy31:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'i':	goto yy47;
	default:	goto yy20;
	}
yy32:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'h':	goto yy48;
	case 'r':	goto yy49;
	default:	goto yy20;
	}
yy33:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'h':	goto yy50;
	default:	goto yy20;
	}
yy34:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case '!':
	case '$':
	case '%':
	case '&':
	case '*':
	case '+':
	case '-':
	case '/':
	case ':':
	case '<':
	case '=':
	case '>':
	case '@':
	case '^':
	case '|':
	case '~':	goto yy6;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy35;
	}
yy35:
#line 110 "Lexer.re"
	{ update_location(); return Token::ColonEquals; }
#line 519 "Lexer.cc"
yy36:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case '!':
	case '$':
	case '%':
	case '&':
	case '*':
	case '+':
	case '-':
	case '/':
	case ':':
	case '<':
	case '=':
	case '>':
	case '@':
	case '^':
	case '|':
	case '~':	goto yy6;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy37;
	}
yy37:
#line 111 "Lexer.re"
	{ update_location(); return Token::EqRarrow;    }
#line 548 "Lexer.cc"
yy38:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'o':	goto yy51;
	default:	goto yy20;
	}
yy39:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 't':	goto yy52;
	default:	goto yy20;
	}
yy40:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'l':	goto yy54;
	default:	goto yy20;
	}
yy41:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy42;
	}
yy42:
#line 102 "Lexer.re"
	{ update_location(); return Token::Do;          }
#line 657 "Lexer.cc"
yy43:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 's':	goto yy56;
	default:	goto yy20;
	}
yy44:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'l':	goto yy57;
	default:	goto yy20;
	}
yy45:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy46;
	}
yy46:
#line 103 "Lexer.re"
	{ update_location(); return Token::If;          }
#line 755 "Lexer.cc"
yy47:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'l':	goto yy58;
	default:	goto yy20;
	}
yy48:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'e':	goto yy60;
	default:	goto yy20;
	}
yy49:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'u':	goto yy61;
	default:	goto yy20;
	}
yy50:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'i':	goto yy62;
	default:	goto yy20;
	}
yy51:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'l':	goto yy63;
	default:	goto yy20;
	}
yy52:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy53;
	}
yy53:
#line 97 "Lexer.re"
	{ update_location(); return Token::TypeInt;     }
#line 886 "Lexer.cc"
yy54:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy55;
	}
yy55:
#line 96 "Lexer.re"
	{ update_location(); return Token::TypeNil;     }
#line 962 "Lexer.cc"
yy56:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'e':	goto yy65;
	default:	goto yy20;
	}
yy57:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 's':	goto yy67;
	default:	goto yy20;
	}
yy58:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy59;
	}
yy59:
#line 95 "Lexer.re"
	{ update_location(); return Token::Nil;         }
#line 1060 "Lexer.cc"
yy60:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'n':	goto yy68;
	default:	goto yy20;
	}
yy61:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'e':	goto yy70;
	default:	goto yy20;
	}
yy62:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'l':	goto yy72;
	default:	goto yy20;
	}
yy63:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy64;
	}
yy64:
#line 100 "Lexer.re"
	{ update_location(); return Token::TypeBool;    }
#line 1169 "Lexer.cc"
yy65:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy66;
	}
yy66:
#line 105 "Lexer.re"
	{ update_location(); return Token::Else;        }
#line 1245 "Lexer.cc"
yy67:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'e':	goto yy73;
	default:	goto yy20;
	}
yy68:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy69;
	}
yy69:
#line 104 "Lexer.re"
	{ update_location(); return Token::Then;        }
#line 1332 "Lexer.cc"
yy70:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy71;
	}
yy71:
#line 98 "Lexer.re"
	{ update_location(); return Token::True;        }
#line 1408 "Lexer.cc"
yy72:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy21;
	case 'e':	goto yy75;
	default:	goto yy20;
	}
yy73:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy74;
	}
yy74:
#line 99 "Lexer.re"
	{ update_location(); return Token::False;       }
#line 1495 "Lexer.cc"
yy75:
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
	case '_':
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
	case 'z':	goto yy19;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy76;
	}
yy76:
#line 101 "Lexer.re"
	{ update_location(); return Token::While;       }
#line 1571 "Lexer.cc"
yyeof:
#line 93 "Lexer.re"
	{ update_location(); return Token::End;         }
#line 1575 "Lexer.cc"
}
#line 115 "Lexer.re"

    }
}
