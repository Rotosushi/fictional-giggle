#include "ast.h"
#include "error.h"

string ast_type_to_string(_ast_type at)
{
	{
		switch (at) {
		case AST_ERR: return "error";
		case AST_VAR: return "var";
		case AST_VARDECL: return "vardecl";
		case AST_ARG: return "arg";
		case AST_FN: return "fndecl";
		case AST_FCALL: return "fcall";
		case AST_IF: return "if";
		case AST_WHILE: return "while";
		case AST_BINOP: return "binop";
		case AST_UNOP: return "unop";
		case AST_RETURN: return "return";
		case AST_SCOPE: return "scope";
		case AST_MODULE: return "module";
		case AST_INT: return "int";
		case AST_REAL: return "float";
		case AST_TEXT: return "text";
		case AST_BOOL: return "bool";
		default: return "error";
		}
	}
}


