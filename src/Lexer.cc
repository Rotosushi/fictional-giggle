/* Generated by re2c 1.3 on Wed Jul 22 13:52:52 2020 */
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
	case ' ':	goto yy4;
	case '\n':	goto yy6;
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
	case '~':	goto yy8;
	case '(':	goto yy11;
	case ')':	goto yy13;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':	goto yy15;
	case ':':	goto yy18;
	case '=':	goto yy20;
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
	case 'd':
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
	case 'w':
	case 'x':
	case 'y':
	case 'z':	goto yy21;
	case 'B':	goto yy24;
	case 'I':	goto yy25;
	case 'N':	goto yy26;
	case '\\':	goto yy27;
	case 'e':	goto yy29;
	case 'f':	goto yy30;
	case 'i':	goto yy31;
	case 'n':	goto yy32;
	case 't':	goto yy33;
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
#line 95 "Lexer.re"
	{ update_location(); return Token::NewLn;       }
#line 199 "Lexer.cc"
yy8:
	YYSKIP ();
	yych = YYPEEK ();
yy9:
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
	case '~':	goto yy8;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy10;
	}
yy10:
#line 113 "Lexer.re"
	{ update_location(); return Token::Operator;    }
#line 229 "Lexer.cc"
yy11:
	YYSKIP ();
#line 106 "Lexer.re"
	{ update_location(); return Token::LParen;      }
#line 234 "Lexer.cc"
yy13:
	YYSKIP ();
#line 107 "Lexer.re"
	{ update_location(); return Token::RParen;      }
#line 239 "Lexer.cc"
yy15:
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
	case '9':	goto yy15;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy17;
	}
yy17:
#line 112 "Lexer.re"
	{ update_location(); return Token::Int;         }
#line 262 "Lexer.cc"
yy18:
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
	case '~':	goto yy8;
	case '=':	goto yy34;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy19;
	}
yy19:
#line 108 "Lexer.re"
	{ update_location(); return Token::Colon;       }
#line 291 "Lexer.cc"
yy20:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy10;
	case '>':	goto yy36;
	default:	goto yy9;
	}
yy21:
	YYSKIP ();
	yych = YYPEEK ();
yy22:
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
	case 'z':	goto yy21;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	}
yy23:
#line 111 "Lexer.re"
	{ update_location(); return Token::Id;          }
#line 379 "Lexer.cc"
yy24:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'o':	goto yy38;
	default:	goto yy22;
	}
yy25:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'n':	goto yy39;
	default:	goto yy22;
	}
yy26:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'i':	goto yy40;
	default:	goto yy22;
	}
yy27:
	YYSKIP ();
#line 105 "Lexer.re"
	{ update_location(); return Token::Backslash;   }
#line 417 "Lexer.cc"
yy29:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'l':	goto yy41;
	default:	goto yy22;
	}
yy30:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'a':	goto yy42;
	default:	goto yy22;
	}
yy31:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'f':	goto yy43;
	default:	goto yy22;
	}
yy32:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'i':	goto yy45;
	default:	goto yy22;
	}
yy33:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'h':	goto yy46;
	case 'r':	goto yy47;
	default:	goto yy22;
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
	case '~':	goto yy8;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy35;
	}
yy35:
#line 109 "Lexer.re"
	{ update_location(); return Token::ColonEquals; }
#line 502 "Lexer.cc"
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
	case '~':	goto yy8;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy37;
	}
yy37:
#line 110 "Lexer.re"
	{ update_location(); return Token::EqRarrow;    }
#line 531 "Lexer.cc"
yy38:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'o':	goto yy48;
	default:	goto yy22;
	}
yy39:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 't':	goto yy49;
	default:	goto yy22;
	}
yy40:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'l':	goto yy51;
	default:	goto yy22;
	}
yy41:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 's':	goto yy53;
	default:	goto yy22;
	}
yy42:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'l':	goto yy54;
	default:	goto yy22;
	}
yy43:
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
	case 'z':	goto yy21;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy44;
	}
yy44:
#line 102 "Lexer.re"
	{ update_location(); return Token::If;          }
#line 662 "Lexer.cc"
yy45:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'l':	goto yy55;
	default:	goto yy22;
	}
yy46:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'e':	goto yy57;
	default:	goto yy22;
	}
yy47:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'u':	goto yy58;
	default:	goto yy22;
	}
yy48:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'l':	goto yy59;
	default:	goto yy22;
	}
yy49:
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
	case 'z':	goto yy21;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy50;
	}
yy50:
#line 98 "Lexer.re"
	{ update_location(); return Token::TypeInt;     }
#line 782 "Lexer.cc"
yy51:
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
	case 'z':	goto yy21;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy52;
	}
yy52:
#line 97 "Lexer.re"
	{ update_location(); return Token::TypeNil;     }
#line 858 "Lexer.cc"
yy53:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'e':	goto yy61;
	default:	goto yy22;
	}
yy54:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 's':	goto yy63;
	default:	goto yy22;
	}
yy55:
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
	case 'z':	goto yy21;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy56;
	}
yy56:
#line 96 "Lexer.re"
	{ update_location(); return Token::Nil;         }
#line 956 "Lexer.cc"
yy57:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'n':	goto yy64;
	default:	goto yy22;
	}
yy58:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'e':	goto yy66;
	default:	goto yy22;
	}
yy59:
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
	case 'z':	goto yy21;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy60;
	}
yy60:
#line 101 "Lexer.re"
	{ update_location(); return Token::TypeBool;    }
#line 1054 "Lexer.cc"
yy61:
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
	case 'z':	goto yy21;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy62;
	}
yy62:
#line 104 "Lexer.re"
	{ update_location(); return Token::Else;        }
#line 1130 "Lexer.cc"
yy63:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy23;
	case 'e':	goto yy68;
	default:	goto yy22;
	}
yy64:
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
	case 'z':	goto yy21;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy65;
	}
yy65:
#line 103 "Lexer.re"
	{ update_location(); return Token::Then;        }
#line 1217 "Lexer.cc"
yy66:
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
	case 'z':	goto yy21;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy67;
	}
yy67:
#line 99 "Lexer.re"
	{ update_location(); return Token::True;        }
#line 1293 "Lexer.cc"
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
	case 'z':	goto yy21;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy69;
	}
yy69:
#line 100 "Lexer.re"
	{ update_location(); return Token::False;       }
#line 1369 "Lexer.cc"
yyeof:
#line 93 "Lexer.re"
	{ update_location(); return Token::End;         }
#line 1373 "Lexer.cc"
}
#line 114 "Lexer.re"

    }
}
