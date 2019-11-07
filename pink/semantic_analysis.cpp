
#include "semantic_analysis.h"
#include "error.h"

bool _semantic_analyzer::analyze_module(_module & m)
{
	bool success = true;
	curr_module = m;
	
	// for now import and export lists do nothing
	// so we do nothing with them.

	// are there multiple functions with the name
	// of <root>?
	if (m.function_table.at[m.root].size() > 1) {
		success = false;
		printf("root of module cannot be overloaded.");

	}
	// does the root function exist
	if (m.function_table[m.root][0].id != m.root) {
		success = false;
		printf("root (%s) of module undeclared", m.root.c_str());
	}

	// resolve the type of all global variables
	for (auto pair : m.variable_table) {
		pair.second.lhs.type = typeof(pair.second);
	}

	// resolve the types in all the functions declared
	// in this module
	for (auto pair : m.function_table) {
		for (auto fn : pair.second) {
			resolve_type(fn);
		}
	}


}

_scope& _semantic_analyzer::curr_scope()
{
	return scopes.top();
}

void _semantic_analyzer::push_scope(_scope& scope)
{
	scopes.push(scope);
}

void _semantic_analyzer::pop_scope()
{
	scopes.pop();
}

_vardecl* _semantic_analyzer::lookup_vardecl(string id)
{
	try {
		// lookup the symbol in the current scope
		auto&& vardecl = curr_scope().variable_table.at(id);
		return &vardecl;
	}
	catch (...) {
		try {
			// lookup the symbol in global scope
			auto&& vardecl = curr_module.variable_table.at(id);
			return &vardecl;
		}
		catch (...) {
			return nullptr;
		}
	}
}

_fn* _semantic_analyzer::lookup_fn(string id, vector<_arg> args)
{
	auto match = [](vector<_arg> a, vector<_arg> b) {
		for (int i = 0; i < a.size(); i++)
			if (a[i].id != b[i].id) return false;
			else if (a[i].type != b[i].type) return false;
		return true;
	};

	try {
		// apparently, we support function overloading now...
		auto&& overload_set = curr_module.function_table.at(id);
		for (auto&& fn : overload_set)
			if (match(fn.argument_list, args))
				return &fn;

		return nullptr;
	}
	catch (...) {
		return nullptr;
	}
}

void _semantic_analyzer::resolve_type(_vardecl& dec)
{
	switch (dec.op) {
	case T_COLON: // <id> ':' <type> ';'
		// the type information is present in the decl already
		return;
	case T_COLON_COLON: // <id> '::' <expr> ';'
	{
		// the type information is deduced from the expression
		dec.lhs.type = typeof(dec.init);
		return;
	}
	case T_COLON_EQ: // <id> ':' (<type>)? '=' <expr> ';'
	{
		// if there wasn't a type annotation,
		// the type is marked as _DEDUCE
		// so the type is deduced from the
		// rhs expression.
		if (dec.lhs.type = _DEDUCE)
			dec.lhs.type = typeof(dec.init);
		else 
		return;
	}
	default: throw;
	}
}

void _semantic_analyzer::resolve_type(_var& var)
{
	switch (var.type) {
	case _DEDUCE:
	{
		var.type = typeof(var.type_expression);
	} 
	case _INT: case _REAL:
	case _TEXT: case _BOOL: case _NIL:
		return;
	default: throw;
	}
}

void _semantic_analyzer::resolve_type(_ast* expr)
{
}

void _semantic_analyzer::resolve_type(_fn& fn)
{
	// resolve the types of the variables in this function
	for (auto pair : fn.body.variable_table) {
		resolve_type(pair.second);
	}
}

void _semantic_analyzer::resolve_type(_fcall& fcall)
{
	// TODO: do we need this function?
}

void _semantic_analyzer::resolve_type(_binop& binop)
{
	auto lhs_type = typeof(binop.lhs);
	auto rhs_type = typeof(binop.rhs);

	// is <op> defined for the lhs_type and rhs_type
	// what is the action associated with this operation
	// function named '+' that we overload?
	auto operator_func = lookup_fn(curr_module, binop.op, binop.lhs, binop.rhs);


	switch (binop.op) {
	case T_EQ:
		// var = int | real | text | bool
		// lhs of '=' must be a variable
		if (binop.lhs->ast_type != AST_VAR)
			throw _semantic_error(__FILE__, __LINE__, "left hand side of '=' must be a variable");
		if (resolve_type(*(_var*)binop.lhs).op == T_COLON_COLON)
			throw _semantic_error(__FILE__, __LINE__, "left hand side of assignment cannot be constant");
		
	case T_ADD:
		// int + int -> int
		// real + real -> real
		// string + string -> string
		// bool + bool -X>
		// can type1 be coerced (cast) to type2?
		// coercible (type1, type2)
	case T_SUB:

	case T_MULT:

	case T_DIV:

	case T_MOD:

	case T_AND:

	case T_OR:

	case T_XOR:

	case T_EQUALS:

	case T_NOT_EQUALS:

	case T_LESS:

	case T_GREATER:

	case T_LESS_EQUALS:

	case T_GREATER_EQUALS:

	case T_BIT_AND:

	case T_BIT_OR:

	case T_BIT_XOR:

	case T_BIT_LSHIFT:
		
	case T_BIT_RSHIFT:
	}
	
}

void _semantic_analyzer::resolve_type(_unop& unop)
{
	// TODO: insert return statement here
}

_type _semantic_analyzer::typeof(_var& var)
{
	return var.type;
}

_type _semantic_analyzer::typeof(_vardecl& vardecl)
{
	if (vardecl.lhs.type == _DEDUCE) {
		return typeof(vardecl.init);
	}
	else {
		return typeof(vardecl.lhs);
	}
}

_type _semantic_analyzer::typeof(_fn& fn)
{
	return typeof(fn.return_value);
}

_type _semantic_analyzer::typeof(_fcall& fn)
{
	// first find the function we are calling
	auto f = resolve_type(fn);
	// then lookup its type
	auto type = typeof(f);
	return type;
}

_type _semantic_analyzer::typeof(_expr& expr)
{
	return typeof(expr.expr);
}

_type _semantic_analyzer::typeof(_ast* expr)
{
	switch (expr->ast_type) {
	case AST_BINOP:
		_binop* binop = ((_binop*)expr);
		return typeof(*binop);
	case AST_UNOP:
		_unop* unop = ((_unop*)expr);
		return typeof(*unop);
	case AST_VAR:
		_var* var = ((_var*)expr);
		return typeof(*var);
	case AST_FCALL:
		_fcall* fcall = ((_fcall*)expr);
		return typeof(*fcall);
	}
}

_type _semantic_analyzer::typeof(_binop& binop)
{
	if (binop.type == _DEDUCE) {
		auto lhs_type = typeof(binop.lhs);
		auto rhs_type = typeof(binop.rhs);

		return typeof(resolve_type(binop))
	}
	else {
		return binop.type;
	}
}

_type _semantic_analyzer::typeof(_unop& unop)
{
	return _type();
}

_type _semantic_analyzer::typeof(_return& ret)
{
	return _type();
}


