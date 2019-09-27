#include "pink_semantic_analysis.h"
/*
bool _semantic_analyzer::typecheck(_ast* expr, _scope& module_scope)
{
	if (_typecheck(expr, module_scope) != AST_ERR)
		return true;
	else
		return false;
}

bool _semantic_analyzer::typecheck(_ast * expr, _scope& module_scope, _scope& current_scope)
{
	if (_typecheck(expr, module_scope, current_scope) != AST_ERR)
		return true;
	else
		return false;
}

_ast_type _semantic_analyzer::_typecheck(_ast* expr, _scope& module_scope)
{
	
	expr could be a single function call, 
		array derefrence, literal, or a complex tree mixing
		ops and primary expressions.

	this function needs to handle all of these possible cases

	the structure of the tree is such that a
	recursive approach is ergonomic.

	due to operator precedence, primary expressions
	are the leaves of the tree, meaning that typechecking
	can be built out of only keeping track of leaf node
	data. interior nodes will see their childrens type
	as the result type of the entire tree below the child.
	then the parent node need only worry about the types
	of it's immediate children.
	
	
	switch (expr->ast_type) {
	case AST_BINOP:
		// if there is a multilayer tree of binops,
		// this case will walk the binop tree and
		// postorder typecheck.
		auto lhs_type = _typecheck(((_binop*)expr)->lhs, module_scope);
		auto rhs_type = _typecheck(((_binop*)expr)->rhs, module_scope);
		auto result_type = typecheck_binop(((_binop*)expr)->op.type, lhs_type, rhs_type);
		return result_type;
	case AST_UNOP:
		auto rhs_type = _typecheck(((_unop*)expr)->rhs, module_scope);
		auto result_type = typecheck_unop(((_unop*)expr)->op.type, rhs_type);
		return result_type;
	case AST_VAR:
		// ensure we are refrencing declared variables;
		if (module_scope.resolve(((_var*)expr)->id)) {
			// variables hold their postops,
			// the result type of a variable is the
			// type of that variable after the application
			// of all postops (left-to-right)
			auto result_type = typecheck_postops((_var*)expr, module_scope);
			return result_type;
		}
		else return AST_ERR;
	case AST_INT:
		return AST_INT;
		break;
	case AST_FLOAT:
		return AST_FLOAT;
		break;
	case AST_STRING:
		return AST_STRING;
		break;
	case AST_BOOL:
		return AST_BOOL;
		break;
	default:
		return AST_ERR;
	}
}

_ast_type _semantic_analyzer::_typecheck(_ast* expr, _scope& module_scope, _scope& current_scope)
{
	return _ast_type();
}

_ast_type _semantic_analyzer::typecheck_binop(_tok op, _ast_type lhs_type, _ast_type rhs_type)
{
	return _ast_type();
}

_ast_type _semantic_analyzer::typecheck_unop(_tok op, _ast_type rhs_type)
{
	return _ast_type();
}

_ast_type _semantic_analyzer::typecheck_postops(_var* var, _scope& module_scope)
{
	// there are no postops, so the type of the var
	// is it's defined type.
	if (var->postops.size() == 0) {
		auto result = module_scope.resolve(var->id);
		if (result != nullptr) {
			return tok_to_ast_type(result->);
		}
	}
	else {

	}
}

bool _semantic_analyzer::do_semantic_analysis(_declaration& decl, _scope& module_scope)
{
	
}

bool _semantic_analyzer::do_semantic_analysis(_declaration& decl, _scope& module_scope, _scope& current_scope)
{
	return false;
}

void _semantic_analyzer::resolve_declaration(_declaration& decl, _scope& module_scope)
{
	switch (_typecheck(decl.rhs, module_scope)) {
	case AST_INT:
		decl.var.type = _INT;
	case AST_FLOAT:
		decl.var.type = _FLOAT;
	case AST_STRING:
		decl.var.type = _STRING;
	case AST_BOOL:
		decl.var.type = _BOOL;
	case AST_VAR:
		decl.var.type = _VAR;
	}
}

void _semantic_analyzer::resolve_declaration(_declaration& decl, _scope& module_scope, _scope& current_scope)
{
	switch (_typecheck(decl.rhs, module_scope, current_scope)) {
	case AST_INT:
		decl.var.type = _INT;
	case AST_FLOAT:
		decl.var.type = _FLOAT;
	case AST_STRING:
		decl.var.type = _STRING;
	case AST_BOOL:
		decl.var.type = _BOOL;
	case AST_VAR:
		decl.var.type = _VAR;
	}
}

_ast_type _semantic_analyzer::tok_to_ast_type(_tok tok)
{
	switch (tok) {
	case T_INT: case T_INT_LITERAL:
		return AST_INT;
	case T_FLOAT: case T_FLOAT_LITERAL:
		return AST_FLOAT;
	case T_STRING: case T_STRING_LITERAL:
		return AST_STRING;
	case T_BOOL: case T_TRUE: case T_FALSE:
		return AST_BOOL;
	case T_ID:
		return AST_VAR;
	}
}
*/