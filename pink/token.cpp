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

	case T_NOT: return "!";
	case T_AND: return "&";
	case T_XOR: return "^";
	case T_OR:  return "|";
	case T_EQUALS: return "==";
	case T_NOT_EQUALS: return "!=";
	case T_LESS: return "<";
	case T_GREATER: return ">";
	case T_LESS_EQUALS: return "<=";
	case T_GREATER_EQUALS: return ">=";

	case T_COMMA:  return ",";
	case T_SEMICOLON: return ";";
	case T_LBRACE: return "{";
	case T_RBRACE: return "}";
	case T_LPAREN: return "(";
	case T_RPAREN: return ")";
	case T_ARROW:  return "->";
	case T_IF: return "if";
	case T_ELSE:  return "else";
	case T_WHILE: return "while";
	case T_FN: return "fn";

	case T_INT:  return "int";
	case T_REAL: return "float";
	case T_TEXT: return "text";
	case T_BOOL: return "bool";
	default: throw _parser_error("internal error: bad token in token-to-string:", t);
	}
}
