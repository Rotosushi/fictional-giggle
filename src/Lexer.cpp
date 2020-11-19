/* Generated by re2c 1.3 on Wed Nov 18 16:41:32 2020 */
#line 1 "Lexer.re"
#include <string>
using std::string;

#include "Ast.hpp"
#include "Parser.hpp"

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

string Lexer::yytxt()
{
    /*
        this was once
        strndup(token, cursor - token);
        where token and cursor have type char*
        (and are pointing into the buffer of characters
        to lex.)

        so here we emulate said behavior of copying
        the series of characters from token until cursor
        by utilizing the string constructor defined
        with two string iterators to achieve the same
        result.
    */
    return string(token, cursor);
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
        
#line 88 "Lexer.cpp"
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
	case '/':
	case '>':
	case '@':
	case '^':
	case '|':
	case '~':	goto yy6;
	case '(':	goto yy9;
	case ')':	goto yy11;
	case ',':	goto yy13;
	case '-':	goto yy15;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':	goto yy16;
	case ':':	goto yy19;
	case ';':	goto yy21;
	case '<':	goto yy23;
	case '=':	goto yy24;
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
	case 'Q':
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
	case 'z':	goto yy25;
	case 'B':	goto yy28;
	case 'I':	goto yy29;
	case 'N':	goto yy30;
	case 'P':	goto yy31;
	case 'R':	goto yy32;
	case '\\':	goto yy33;
	case 'd':	goto yy35;
	case 'e':	goto yy36;
	case 'f':	goto yy37;
	case 'i':	goto yy38;
	case 'n':	goto yy39;
	case 't':	goto yy40;
	case 'w':	goto yy41;
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
#line 191 "Lexer.cpp"
yy4:
	YYSKIP ();
#line 94 "Lexer.re"
	{ update_location(); continue;                  }
#line 196 "Lexer.cpp"
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
#line 120 "Lexer.re"
	{ update_location(); return Token::Operator;    }
#line 226 "Lexer.cpp"
yy9:
	YYSKIP ();
#line 109 "Lexer.re"
	{ update_location(); return Token::LParen;      }
#line 231 "Lexer.cpp"
yy11:
	YYSKIP ();
#line 110 "Lexer.re"
	{ update_location(); return Token::RParen;      }
#line 236 "Lexer.cpp"
yy13:
	YYSKIP ();
#line 111 "Lexer.re"
	{ update_location(); return Token::Comma;       }
#line 241 "Lexer.cpp"
yy15:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy8;
	case '>':	goto yy42;
	default:	goto yy7;
	}
yy16:
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
	case '9':	goto yy16;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy18;
	}
yy18:
#line 119 "Lexer.re"
	{ update_location(); return Token::Int;         }
#line 275 "Lexer.cpp"
yy19:
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
	case '=':	goto yy44;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy20;
	}
yy20:
#line 112 "Lexer.re"
	{ update_location(); return Token::Colon;       }
#line 304 "Lexer.cpp"
yy21:
	YYSKIP ();
#line 113 "Lexer.re"
	{ update_location(); return Token::Semicolon;   }
#line 309 "Lexer.cpp"
yy23:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy8;
	case '-':	goto yy46;
	default:	goto yy7;
	}
yy24:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy8;
	case '>':	goto yy48;
	default:	goto yy7;
	}
yy25:
	YYSKIP ();
	yych = YYPEEK ();
yy26:
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	}
yy27:
#line 118 "Lexer.re"
	{ update_location(); return Token::Id;          }
#line 408 "Lexer.cpp"
yy28:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'o':	goto yy50;
	default:	goto yy26;
	}
yy29:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'n':	goto yy51;
	default:	goto yy26;
	}
yy30:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'i':	goto yy52;
	default:	goto yy26;
	}
yy31:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'o':	goto yy53;
	default:	goto yy26;
	}
yy32:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'e':	goto yy54;
	default:	goto yy26;
	}
yy33:
	YYSKIP ();
#line 108 "Lexer.re"
	{ update_location(); return Token::Backslash;   }
#line 468 "Lexer.cpp"
yy35:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'o':	goto yy55;
	default:	goto yy26;
	}
yy36:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'l':	goto yy57;
	default:	goto yy26;
	}
yy37:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'a':	goto yy58;
	default:	goto yy26;
	}
yy38:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'f':	goto yy59;
	default:	goto yy26;
	}
yy39:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'i':	goto yy61;
	default:	goto yy26;
	}
yy40:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'h':	goto yy62;
	case 'r':	goto yy63;
	default:	goto yy26;
	}
yy41:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'h':	goto yy64;
	default:	goto yy26;
	}
yy42:
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
		goto yy43;
	}
yy43:
#line 116 "Lexer.re"
	{ update_location(); return Token::Rarrow;      }
#line 575 "Lexer.cpp"
yy44:
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
		goto yy45;
	}
yy45:
#line 114 "Lexer.re"
	{ update_location(); return Token::ColonEquals; }
#line 604 "Lexer.cpp"
yy46:
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
		goto yy47;
	}
yy47:
#line 115 "Lexer.re"
	{ update_location(); return Token::Larrow;      }
#line 633 "Lexer.cpp"
yy48:
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
		goto yy49;
	}
yy49:
#line 117 "Lexer.re"
	{ update_location(); return Token::EqRarrow;    }
#line 662 "Lexer.cpp"
yy50:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'o':	goto yy65;
	default:	goto yy26;
	}
yy51:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 't':	goto yy66;
	default:	goto yy26;
	}
yy52:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'l':	goto yy68;
	default:	goto yy26;
	}
yy53:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'l':	goto yy70;
	default:	goto yy26;
	}
yy54:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'f':	goto yy71;
	default:	goto yy26;
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy56;
	}
yy56:
#line 104 "Lexer.re"
	{ update_location(); return Token::Do;          }
#line 793 "Lexer.cpp"
yy57:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 's':	goto yy73;
	default:	goto yy26;
	}
yy58:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'l':	goto yy74;
	default:	goto yy26;
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy60;
	}
yy60:
#line 105 "Lexer.re"
	{ update_location(); return Token::If;          }
#line 891 "Lexer.cpp"
yy61:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'l':	goto yy75;
	default:	goto yy26;
	}
yy62:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'e':	goto yy77;
	default:	goto yy26;
	}
yy63:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'u':	goto yy78;
	default:	goto yy26;
	}
yy64:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'i':	goto yy79;
	default:	goto yy26;
	}
yy65:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'l':	goto yy80;
	default:	goto yy26;
	}
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy67;
	}
yy67:
#line 99 "Lexer.re"
	{ update_location(); return Token::TypeInt;     }
#line 1022 "Lexer.cpp"
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy69;
	}
yy69:
#line 98 "Lexer.re"
	{ update_location(); return Token::TypeNil;     }
#line 1098 "Lexer.cpp"
yy70:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'y':	goto yy82;
	default:	goto yy26;
	}
yy71:
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy72;
	}
yy72:
#line 96 "Lexer.re"
	{ update_location(); return Token::TypeRef;     }
#line 1185 "Lexer.cpp"
yy73:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'e':	goto yy84;
	default:	goto yy26;
	}
yy74:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 's':	goto yy86;
	default:	goto yy26;
	}
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy76;
	}
yy76:
#line 97 "Lexer.re"
	{ update_location(); return Token::Nil;         }
#line 1283 "Lexer.cpp"
yy77:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'n':	goto yy87;
	default:	goto yy26;
	}
yy78:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'e':	goto yy89;
	default:	goto yy26;
	}
yy79:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'l':	goto yy91;
	default:	goto yy26;
	}
yy80:
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy81;
	}
yy81:
#line 102 "Lexer.re"
	{ update_location(); return Token::TypeBool;    }
#line 1392 "Lexer.cpp"
yy82:
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy83;
	}
yy83:
#line 95 "Lexer.re"
	{ update_location(); return Token::TypePoly;    }
#line 1468 "Lexer.cpp"
yy84:
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy85;
	}
yy85:
#line 107 "Lexer.re"
	{ update_location(); return Token::Else;        }
#line 1544 "Lexer.cpp"
yy86:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'e':	goto yy92;
	default:	goto yy26;
	}
yy87:
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy88;
	}
yy88:
#line 106 "Lexer.re"
	{ update_location(); return Token::Then;        }
#line 1631 "Lexer.cpp"
yy89:
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy90;
	}
yy90:
#line 100 "Lexer.re"
	{ update_location(); return Token::True;        }
#line 1707 "Lexer.cpp"
yy91:
	YYSKIP ();
	yych = YYPEEK ();
	switch (yych) {
	case 0x00:
		if (YYLESSTHAN (1)) {
		}
		goto yy27;
	case 'e':	goto yy94;
	default:	goto yy26;
	}
yy92:
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy93;
	}
yy93:
#line 101 "Lexer.re"
	{ update_location(); return Token::False;       }
#line 1794 "Lexer.cpp"
yy94:
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
	case 'z':	goto yy25;
	default:
		if (YYLESSTHAN (1)) {
		}
		goto yy95;
	}
yy95:
#line 103 "Lexer.re"
	{ update_location(); return Token::While;       }
#line 1870 "Lexer.cpp"
yyeof:
#line 93 "Lexer.re"
	{ update_location(); return Token::End;         }
#line 1874 "Lexer.cpp"
}
#line 121 "Lexer.re"

    }
}
