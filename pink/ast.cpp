#include "ast.h"

string ast_type_to_string(_ast_type at)
{
	{
		switch (at) {
		case AST_ERR: return "error";
		case AST_TYPE_SPECIFIER: return "type_specifier";
		case AST_VAR: return "var";
		case AST_VARDECL: return "vardecl";
		case AST_ARG: return "arg";
		case AST_LAMBDA: return "lambda";
		case AST_FNDECL: return "fndecl";
		case AST_FCALL: return "fcall";
		case AST_NAMED_MEMBER: return "named_member";
		case AST_POSITIONAL_MEMBER: return "positional_member";
		case AST_IF: return "if";
		case AST_WHILE: return "while";
		case AST_DOWHILE: return "dowhile";
		case AST_BINOP: return "binop";
		case AST_UNOP: return "unop";
		case AST_RETURN: return "return";
		case AST_MEMBER: return "member";
		case AST_RECORD: return "struct";
		case AST_UNION: return "union";
		case AST_TUPLE: return "tuple";
		case AST_SCOPE: return "scope";
		case AST_MODULE: return "module";
		case AST_ARRAY: return "array";
		case AST_INT: return "int";
		case AST_FLOAT: return "float";
		case AST_TEXT: return "text";
		case AST_BOOL: return "bool";
		default: return "error";
		}
	}
}
