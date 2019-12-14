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
		case AST_FLOAT: return "float";
		case AST_TEXT: return "text";
		case AST_BOOL: return "bool";
		default: return "error";
		}
	}
}

_var* build_var(string id, _type type, _ast* type_expression)
{
	auto var = new _var;
	var->id = id;
	var->type = type;
	var->type_expression = type_expression;
	return var;
}

_vardecl* create_vardecl(_var lhs, _token op, _ast* init)
{
	auto vardecl = new _vardecl;
	vardecl->lhs.id = lhs.id;
	vardecl->lhs.type = lhs.type;
	vardecl->lhs.type_expression = lhs.type_expression;
	vardecl->op = op;
	vardecl->init = init;
	return vardecl;
}

_scope* create_scope(vector<_vardecl> variables, vector<_ast*> statements)
{
	auto scope = new _scope;

}

_arg* create_arg(string id, _type type, _ast* type_expression)
{
	return nullptr;
}

_fn* create_fn(string id, vector<_arg> argument_list, _var return_value, _scope body)
{
	return nullptr;
}

_if* create_if(_ast* cond, _ast* then, _ast* els)
{
	return nullptr;
}

_while* create_while(_ast* cond, _ast* then, _ast* els)
{
	return nullptr;
}

_binop* create_binop(_type type, _token op, _ast* lhs, _ast* rhs)
{
	return nullptr;
}

_unop* create_unop(_type type, _token op, _ast* rhs)
{
	return nullptr;
}

_return* create_return(_type type, _ast* expr)
{
	return nullptr;
}

_fcall* create_fcall(string id, vector<_arg> argument_list, _var return_value)
{
	return nullptr;
}

void init_module_with_kernel(_module& m)
{
	
}


