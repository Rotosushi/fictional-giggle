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

/*!re2c
    identifier = [a-zA-Z_][a-zA-Z0-9_]*;
    integer    = [0-9]+;
    operator   = [+\-*/%<>:=&@!~|$\^]+;
*/

#define YYPEEK()      (*cursor)
#define YYSKIP()      (++cursor)
#define YYBACKUP()    (marker = cursor)
#define YYRESTORE()   (cursor = marker)
#define YYLESSTHAN(n) (n > (end - cursor))

Token Lexer::yylex()
{
    while(1) {
        token = cursor;
        /*!re2c
            re2c:define:YYCTYPE = char;
            re2c:yyfill:enable = 0;
            re2c:eof = 0;

            *          { update_location(); return Token::Error;       }
            $          { update_location(); return Token::End;         }
            [ \t]      { update_location(); continue;                  }
            [\n]       { update_location(); return Token::NewLn;       }
            "nil"      { update_location(); return Token::Nil;         }
            "Nil"      { update_location(); return Token::TypeNil;     }
            "Int"      { update_location(); return Token::TypeInt;     }
            "true"     { update_location(); return Token::True;        }
            "false"    { update_location(); return Token::False;       }
            "Bool"     { update_location(); return Token::TypeBool;    }
            "if"       { update_location(); return Token::If;          }
            "then"     { update_location(); return Token::Then;        }
            "else"     { update_location(); return Token::Else;        }
            "\\"       { update_location(); return Token::Backslash;   }
            "("        { update_location(); return Token::LParen;      }
            ")"        { update_location(); return Token::RParen;      }
            ":"        { update_location(); return Token::Colon;       }
            ":="       { update_location(); return Token::ColonEquals; }
            "=>"       { update_location(); return Token::EqRarrow;    }
            identifier { update_location(); return Token::Id;          }
            integer    { update_location(); return Token::Int;         }
            operator   { update_location(); return Token::Operator;    }
        */
    }
}
