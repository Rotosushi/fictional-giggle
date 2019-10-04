

#include "semantic_analysis.h"
#include "error.h"

bool _semantic_analyzer::typecheck_module(_module& m)
{
	bool success = true;
	_ast* dec;

	curr_module = m;
	// first we need to resolve the type of the declarations
	for (auto var : m.body.variables) {
		auto vartype = var.second.lhs.type;
		auto op = var.second.op;

		if (op == T_COLON) { // only a <type-specifier>
			/*
			<type-specifier> := <lambda-header>
				  | <type-primitive>
				  | <identifier>
			*/
			switch (vartype) {
			case _DEDUCE:
				// deduce type here
				dec = m.resolve_type(var.second.lhs.tname);
				break;
			case _LAMBDA:
				// typecheck the lambda 
				typecheck_lambda(*((_lambda*)var.second.rhs));
				break;
			case _INT: case _FLOAT: case _TEXT: case _BOOL:
				// the variable has a type
				break;
			default:
				throw _error("internal error: variable has incorrect type tag, should't happen with expected parsed variables");
			}

		}
		else if (op == T_COLON_COLON) { // only an <initializer>
			/*
			<initializer>  := <lambda-definition>
				| <expr>
			*/
			if (vartype == _LAMBDA) {
				typecheck_lambda(*((_lambda*)var.second.rhs));
			}
			else if (vartype == _DEDUCE) {
				auto expr_type = typecheck_expression(var.second.rhs);
				if (expr_type == _DEDUCE) {
					// TODO: this is the result of the design fighting with itself
					// we need to have a consistent way of looking up usertypes 
					// and resolving them.
					// var.second.lhs.type = m.resolve_type(var.second.lhs.tname);
				}
				else if (is_intrinsic_type(expr_type)) {
					var.second.lhs.type = expr_type;
				}
				else {

				}
			}
			else throw _error("internal error: variable has incorrect type tag, shouldn't happen with expected parsed variables");

		}
		else if (op == T_COLON_EQ) {
			
		}
		else {

		}
	}

	return success;
}

bool _semantic_analyzer::is_intrinsic_type(_type t)
{
	switch (t) {
	case _INT: case _FLOAT: case _TEXT: case _BOOL:
	case _LAMBDA:
		return true;
	default:
		return false;
	}
}

bool _semantic_analyzer::typecheck_declaration(_vardecl& var)
{
	return false;
}

bool _semantic_analyzer::typecheck_function(_fndecl& fn)
{
	return false;
}

_type _semantic_analyzer::typecheck_expression(_ast* expr)
{
	
}

bool _semantic_analyzer::typecheck_lambda(_lambda& l)
{
	return false;
}

bool _semantic_analyzer::typecheck_conditional(_if& conditional)
{
	return false;
}

bool _semantic_analyzer::typecheck_iteration(_while& loop)
{
	return false;
}

bool _semantic_analyzer::typecheck_iteration(_dowhile& loop)
{
	return false;
}
