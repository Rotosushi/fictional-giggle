#include <string>
using std::string;

#include "token.h"
#include "error.h"

string token_to_string(_token t)
{
	switch (t) {
	case T_ERR: return "ERR";
	case T_EOF: return "EOF";

	case T_EQ: return "=";
	case T_COLON: return ":";
	case T_COLON_EQ: return ":=";
	case T_COLON_COLON: return "::";

	case T_ADD:  return "+";
	case T_SUB:  return "-";
	case T_MULT: return "*";
	case T_DIV:  return "/";
	case T_MOD:  return "%";

	case T_BIT_NOT: return "!!";
	case T_BIT_AND: return "&&";
	case T_BIT_XOR: return "^^";
	case T_BIT_OR:  return "||";
	case T_BIT_LSHIFT: return "<<";
	case T_BIT_RSHIFT: return ">>";

	case T_LOG_NOT: return "!";
	case T_LOG_AND: return "&";
	case T_LOG_XOR: return "^";
	case T_LOG_OR:  return "|";
	case T_LOG_EQUALS: return "==";
	case T_LOG_NOT_EQUALS: return "!=";
	case T_LOG_LESS: return "<";
	case T_LOG_GREATER: return ">";
	case T_LOG_LESS_EQUALS: return "<=";
	case T_LOG_GREATER_EQUALS: return ">=";

	case T_PERIOD: return ".";
	case T_COMMA:  return ",";
	case T_SEMICOLON: return ";";
	case T_LBRACE: return "{";
	case T_RBRACE: return "}";
	case T_LBRACKET: return "[";
	case T_RBRACKET: return "]";
	case T_LPAREN: return "(";
	case T_RPAREN: return ")";
	case T_ARROW:  return "->";
	case T_IF: return "if";
	case T_ELSE:  return "else";
	case T_DO: return "do";
	case T_WHILE: return "while";
	case T_FN: return "fn";
	case T_RECORD: return "record";

	case T_INT:  return "int";
	case T_FLOAT: return "float";
	case T_TEXT: return "text";
	case T_BOOL: return "bool";
	default: throw _parser_error("internal error: bad token in token-to-string:", t);
	}
}
