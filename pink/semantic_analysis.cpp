
#include <stdexcept>
#include <optional>
using std::optional;
using std::nullopt;

#include "semantic_analysis.h"
#include "error.h"

void _semantic_analyzer::analyze(_module& mdl)
{
	init_module_with_kernel(mdl);

	stack<_scope> scope_stack;

	infer_types(mdl, scope_stack);
	typecheck(mdl, scope_stack);
}

void _semantic_analyzer::init_module_with_kernel(_module& m)
{
	// binops
	// int
	_type int_type;
	int_type.name = "int";
	int_type.expr = nullptr;

	_arg int_arg_lhs, int_arg_rhs;
	int_arg_lhs.id = "x";
	int_arg_lhs.type = int_type;
	int_arg_rhs.id = "y";
	int_arg_rhs.type = int_type;

	// +, -, *, /, %, ==, !=, <, >, <=, >=, !!, &&, ||, ^^, <<, >>
	// we start with integer arithmetic
	_fn fn;
	fn.id = "+";
	fn.return_type = int_type;
	fn.argument_list = { int_arg_lhs, int_arg_rhs };
	m.functions[fn.id].push_back(fn);

	fn.id = "-";
	m.functions[fn.id].push_back(fn);

	fn.id = "*";
	m.functions[fn.id].push_back(fn);

	fn.id = "/";
	m.functions[fn.id].push_back(fn);

	fn.id = "%";
	m.functions[fn.id].push_back(fn);

	// then integer bitwise operations 

	fn.id = "&&";
	m.functions[fn.id].push_back(fn);

	fn.id = "||";
	m.functions[fn.id].push_back(fn);

	fn.id = "^^";
	m.functions[fn.id].push_back(fn);

	fn.id = "<<";
	m.functions[fn.id].push_back(fn);

	fn.id = ">>";
	m.functions[fn.id].push_back(fn);

	// integer boolean comparisons
	_type bool_type;
	bool_type.name = "bool";
	bool_type.expr = nullptr;

	fn.id = "==";
	fn.return_type = bool_type;
	m.functions[fn.id].push_back(fn);

	fn.id = "!=";
	m.functions[fn.id].push_back(fn);

	fn.id = "<";
	m.functions[fn.id].push_back(fn);

	fn.id = ">";
	m.functions[fn.id].push_back(fn);

	fn.id = "<=";
	m.functions[fn.id].push_back(fn);

	fn.id = ">=";
	m.functions[fn.id].push_back(fn);

	// real type primitives
	// +, -, *, /, %, ==, !=, <, >, <=, >=
	_type real_type;
	real_type.name = "real";
	real_type.expr = nullptr;

	_arg real_arg_lhs, real_arg_rhs;
	real_arg_lhs.id = "x";
	real_arg_lhs.type = real_type;
	real_arg_rhs.id = "y";
	real_arg_rhs.type = real_type;
	// starting with real boolean comparisons
	fn.id = "==";
	fn.argument_list = { real_arg_lhs, real_arg_rhs };
	m.functions[fn.id].push_back(fn);

	fn.id = "!=";
	m.functions[fn.id].push_back(fn);

	fn.id = "<";
	m.functions[fn.id].push_back(fn);

	fn.id = ">";
	m.functions[fn.id].push_back(fn);

	fn.id = "<=";
	m.functions[fn.id].push_back(fn);

	fn.id = ">=";
	m.functions[fn.id].push_back(fn);

	// defining real arithmetic
	fn.id = "+";
	fn.return_type = real_type;
	m.functions[fn.id].push_back(fn);

	fn.id = "-";
	m.functions[fn.id].push_back(fn);

	fn.id = "*";
	m.functions[fn.id].push_back(fn);

	fn.id = "/";
	m.functions[fn.id].push_back(fn);

	fn.id = "%";
	m.functions[fn.id].push_back(fn);

	// text
	_type text_type;
	text_type.name = "text";
	text_type.expr = nullptr;

	_arg text_arg_lhs, text_arg_rhs;
	text_arg_lhs.id = "x";
	text_arg_lhs.type = text_type;
	text_arg_rhs.id = "y";
	text_arg_rhs.type = text_type;
	// +, ==, !=, <, >, <=, >=
	// text concatenation
	fn.id = "+";
	fn.argument_list = { text_arg_lhs, text_arg_rhs };
	fn.return_type = text_type;
	m.functions[fn.id].push_back(fn);

	// string comparison,  <, ==, >
	// with the < clause being
	// the first char that does not
	// match is less on the lhs 
	// > : the first char that does
	// not match is greater on the lhs
	// and <=, >=, match true if the
	// strings are equal to or if the
	// first character that does not 
	// match is less or greater respectively.
	fn.id = "==";
	fn.return_type = bool_type;
	m.functions[fn.id].push_back(fn);

	fn.id = "!=";
	m.functions[fn.id].push_back(fn);

	fn.id = "<";
	m.functions[fn.id].push_back(fn);

	fn.id = ">";
	m.functions[fn.id].push_back(fn);

	fn.id = "<=";
	m.functions[fn.id].push_back(fn);

	fn.id = ">=";
	m.functions[fn.id].push_back(fn);

	// bool
	_arg bool_arg_lhs, bool_arg_rhs;
	bool_arg_lhs.id = "x";
	bool_arg_lhs.type = bool_type;
	bool_arg_rhs.id = "y";
	bool_arg_rhs.type = bool_type;
	// ==, !=, !, &, |, ^

	fn.id = "==";
	fn.argument_list = { bool_arg_lhs, bool_arg_rhs };
	m.functions[fn.id].push_back(fn);

	fn.id = "!=";
	m.functions[fn.id].push_back(fn);

	fn.id = "!";
	m.functions[fn.id].push_back(fn);

	fn.id = "&";
	m.functions[fn.id].push_back(fn);

	fn.id = "|";
	m.functions[fn.id].push_back(fn);

	fn.id = "^";
	m.functions[fn.id].push_back(fn);

	fn.id = "&&";
	m.functions[fn.id].push_back(fn);

	fn.id = "||";
	m.functions[fn.id].push_back(fn);

	fn.id = "^^";
	m.functions[fn.id].push_back(fn);

	// unops


}

bool _semantic_analyzer::structurally_equivalent(_type lt, _type rt)
{
	// note: the reason this works is that
	// there are only four types in the language,
	// and all of them are primitive.
	// _int, _real, _text, _bool
	// the real definition will need to be recursive
	// and walk the adt structure
	if (lt.expr == nullptr || rt.expr == nullptr) throw _semantic_error(__FILE__, __LINE__, "Semantic Error: Structural Equality is not defined for nullptr types");
	return lt.expr->ast_type == rt.expr->ast_type;
}

_type _semantic_analyzer::resolve_type(_var& var, stack<_scope>& scope_stack)
{
	stack<_scope> buffer;

	while (scope_stack.size() > 0) {
		// get the current scope from the top of the stack
		auto&& current_scope = scope_stack.top();
		// save the current scope to the top of an internal buffer
		buffer.push(current_scope);
		// remove the top of the stack so if we need to iterate again
		// we look at the next scope out.
		scope_stack.pop();

		// just do a simple linear search of each scope
		// from the current scope outwards.
		for (auto&& bucket : current_scope.local_symbols)
			for (auto&& pair : bucket) {
				auto&& symbol = pair.second;

				if (symbol.lhs.id == var.id)
					// we found the symbol, so we need to
					// reset the state of the scope_stack
					var = symbol.lhs;
					while (buffer.size() > 0) {
						scope_stack.push(buffer.top());
						buffer.pop();
					}
					// return the found type
					return symbol.lhs.type;
			}

	}

	
}

void _semantic_analyzer::infer_types(_module& mdl, stack<_scope>& scope_stack)
{
	// at some point in the future I am sure this program will be
	// defined using design patterns to better support
	// a more dynamic semantics, but this is version one,
	// so getting something working is the first
	// priority. then, we can use an analysis of this design
	// to better design version 2. we look at where was
	// resonant in the design, and where was dissonant
	// (what worked, and what didn't) and we look at where
	// we want to extend.

	// infer the type of every global symbol
	for (auto&& bucket : mdl.module_scope.local_symbols)
		for (auto&& pair : bucket) {
			infer_type(pair.second, scope_stack, mdl.functions);
		}

	// infer the return type of every function
	for (auto&& overload_set : mdl.functions)
		for (auto&& fn : overload_set.second) {
			infer_type(fn, scope_stack, mdl.functions);
		}
}

optional<_type> _semantic_analyzer::infer_return_type_from_fn(_fn& fn, stack<_scope>& scope_stack, function_table& functions)
{
	optional<_type> return_type;

	bool found_return = false;

	auto&& local_scope = fn.body;

	for (auto&& stmt : local_scope.statements) {
		switch (stmt->ast_type) {
		case AST_RETURN: {
			auto&& inferred_type = typeof(stmt, scope_stack, functions);
			return_type = inferred_type;
			found_return = true;
			break;
		}
		case AST_IF: {
			auto&& cond = (_if*)stmt;
			auto&& inferred_type = infer_return_type_from_if(*cond, scope_stack, functions);
			return_type = inferred_type;
			found_return = true;
			break;
		}
		case AST_WHILE: {
			auto&& loop = (_while*)stmt;
			auto&& inferred_type = infer_return_type_from_while(*loop, scope_stack, functions);
			return_type = inferred_type;
			found_return = true;
			break;
		}
		case AST_SCOPE: {
			auto&& local_scope = (_scope*)stmt;
			scope_stack.push(*local_scope);
			auto&& inferred_type = infer_return_type_from_scope(scope_stack, functions);
			scope_stack.pop();
			return_type = inferred_type;
			found_return = true;
			break;
		}
		}
	}

	// if we looked through the entire body of the function, and didn't find a return expr
	// we are going to assume that the return value of the function is the result type
	// of the last expression in the body of the function
	if (!found_return) {
		auto&& inferred_type = infer_return_type_from_last_stmt(local_scope.statements.back(), scope_stack, functions);
		return_type = inferred_type;
	}

	return return_type;
}

optional<_type> _semantic_analyzer::infer_return_type_from_scope(stack<_scope>& scope_stack, function_table& functions) {
	optional<_type> return_type;
	
	// search for a return stmt in the local scope.
	auto&& local_scope = scope_stack.top();

	for (auto&& stmt : local_scope.statements) {
		switch (stmt->ast_type) {
		case AST_RETURN: {
			auto&& inferred_type = typeof(stmt, scope_stack, functions);
			return_type = inferred_type;
			break;
		}
		case AST_IF: {
			auto&& cond = (_if*)stmt;
			auto&& inferred_type = infer_return_type_from_if(*cond, scope_stack, functions);
			return_type = inferred_type;
			break;
		}
		case AST_WHILE: {
			auto&& loop = (_while*)stmt;
			auto&& inferred_type = infer_return_type_from_while(*loop, scope_stack, functions);
			return_type = inferred_type;
			break;
		}
		case AST_SCOPE: {
			auto&& local_scope = (_scope*)stmt;
			scope_stack.push(*local_scope);
			auto&& inferred_type = infer_return_type_from_scope(scope_stack, functions);
			scope_stack.pop();
			return_type = inferred_type;
			break;
		}
		}
	}
	return return_type;
}
optional<_type> _semantic_analyzer::infer_return_type_from_if(_if& cond, stack<_scope>& scope_stack, function_table& functions)
{
	// there may be a return stmt in either body of the if conditional.
	// so we need to search both for a possible return stmt. once we find our first
	// return we want to return it's type immediately. we need to be sure
	// to search the else body of the conditional if we do not find a return stmt.
	optional<_type> inferred_type;
	switch (cond.then->ast_type) {
	case AST_RETURN: {
		inferred_type = typeof(cond.then, scope_stack, functions);
		break;
	}
	case AST_IF: {
		auto&& nested_cond = (_if*)cond.then;
		inferred_type = infer_return_type_from_if(*nested_cond, scope_stack, functions);
		break;
	}
	case AST_WHILE: {
		auto&& loop = (_while*)cond.then;
		inferred_type = infer_return_type_from_while(*loop, scope_stack, functions);
		break;
	}
	case AST_SCOPE: {
		auto&& local_scope = (_scope*)cond.then;
		scope_stack.push(*local_scope);
		inferred_type = infer_return_type_from_scope(scope_stack, functions);
		scope_stack.pop();
		break;
	}
	}

	// if we didn't find a return in the then body
	// we now search the else body
	if (!inferred_type) {
		switch (cond.els->ast_type) {
		case AST_RETURN: {
			inferred_type = typeof(cond.els, scope_stack, functions);
			break;
		}
		case AST_IF: {
			auto&& nested_cond = (_if*)cond.els;
			inferred_type = infer_return_type_from_if(*nested_cond, scope_stack, functions);
			break;
		}
		case AST_WHILE: {
			auto&& loop = (_while*)cond.els;
			inferred_type = infer_return_type_from_while(*loop, scope_stack, functions);
			break;
		}
		case AST_SCOPE: {
			auto&& local_scope = (_scope*)cond.els;
			scope_stack.push(*local_scope);
			inferred_type = infer_return_type_from_scope(scope_stack, functions);
			scope_stack.pop();
			break;
		}
		}
	}
	return inferred_type;
}
optional<_type> _semantic_analyzer::infer_return_type_from_while(_while& loop, stack<_scope>& scope_stack, function_table& functions)
{
	optional<_type> inferred_type;
	// we need to search the body of the while loop for a potential
	// return stmt
	switch (loop.body->ast_type) {
	case AST_RETURN: {
		inferred_type = typeof(loop.body, scope_stack, functions);
		break;
	}
	case AST_IF: {
		auto&& cond = (_if*)loop.body;
		inferred_type = infer_return_type_from_if(*cond, scope_stack, functions);
		break;
	}
	case AST_WHILE: {
		auto&& nested_loop = (_while*)loop.body;
		inferred_type = infer_return_type_from_while(*nested_loop, scope_stack, functions);
		break;
	}
	case AST_SCOPE: {
		auto&& local_scope = (_scope*)loop.body;
		scope_stack.push(*local_scope);
		inferred_type = infer_return_type_from_scope(scope_stack, functions);
		scope_stack.pop();
		break;
	}
	}
	return inferred_type;
}

_type _semantic_analyzer::infer_return_type_from_last_stmt(_ast* stmt, stack<_scope>& scope_stack, function_table& functions)
{
	_type inferred_type;

	/*
		if the last statement in the body of the function is
		an affix expression, then we can use typeof to infer it's type.

		if the last statement in the body is a scope, then we infer from
		the last statement in that scope. 

		if the last statement in the body is a while, then we infer from the
		body of the while. (the actual return would be placed in such a way 
		as to return after the exit condition of the loop is met, and to
		return the named or temporary value of the last statement, of which
		the type is inferred from.)

		if the last statement is an if, then we infer from the 'then' case of
		the conditional. (this adds an additional typecheck
		case for if conditionals which appear as return expressions,
		the resulting type of the then and else cases must be equivalent.
		and the value of the acutal path taken will need to be the result value.)
	*/

	switch (stmt->ast_type) {
	case AST_IF: {
		auto&& cond = (_if*)stmt;
		inferred_type = infer_return_type_from_last_stmt(cond->then, scope_stack, functions);
	}
	case AST_WHILE: {
		auto&& loop = (_while*)stmt;
		inferred_type = infer_return_type_from_last_stmt(loop->body, scope_stack, functions);
	}
	case AST_SCOPE: {
		auto&& scope = (_scope*)stmt;
		scope_stack.push(*scope);
		inferred_type = infer_return_type_from_last_stmt(scope->statements.back(), scope_stack, functions);
		scope_stack.pop();
	}
	default: {
		inferred_type = typeof(stmt, scope_stack, functions);
	}
	}
	return inferred_type;
}

void _semantic_analyzer::infer_type(_fn& fn, stack<_scope>& scope_stack, function_table& functions)
{
	// inferring the type of a function
	// only means inferring the return type.
	// as that is the only typename that is allowed to
	// be elided by the user for now. 
	// maybe eliding the typename of arguments is useful?
	// but really, if you are going to go that far, why not
	// just get rid of the need for any explicit type denotation like
	// haskell?
	
	/*
		there are two states that a fn can be in here,
		1: the user explicitly typed in the typename
			meaning we don't need to infer.
		2: the user elided the return type 
			so we infer the type from:
			1: the return type of the first return expression
				we find in the body of the function, this requires
				walking the parse tree
			2: if we don't find a return statement
				then the type of the last executable statement in 
				the body of the function is assumed to be the
				return type. and the destination value of the last
				executable statement to be the return value.
	*/
	if (empty_type(fn.return_type)) {
			auto&& local_scope = fn.body;
			scope_stack.push(local_scope);
			auto&& inferred_type = infer_return_type_from_scope(scope_stack, functions);
			scope_stack.pop();

			// if we couldn't infer a return type, then the
			// inferred_type will be nullopt.
			if (inferred_type)
				fn.return_type = inferred_type.value;
			else throw _semantic_error(__FILE__, __LINE__, "cannot infer return type of function: " + fn_to_string(fn.id, fn.argument_list));
	}
}

void _semantic_analyzer::infer_type(_vardecl& decl, stack<_scope>& scope_stack, function_table& functions)
{
	/*
		there are two states that a decl can be in here,
		1: the user typed in the type directly, meaning 
			we don't need to infer.
		2: the user elided the typename and is inferring the type
			via the initialization expression.
	*/
	if (empty_type(decl.lhs.type)) {
		auto inferred_type = typeof(decl.init, scope_stack, functions);
		decl.lhs.type = inferred_type;
	}
}

_type _semantic_analyzer::typeof(_ast* expr, stack<_scope>& scope_stack, function_table& functions)
{
	switch (expr->ast_type) {
	case AST_VAR: {
		auto&& var = (_var*)expr;
		auto type = resolve_type(*var, scope_stack);
		return type;
	}
	case AST_BINOP: {
		auto&& binop = (_binop*)expr;

		auto fn = lookup_binop(binop, scope_stack, functions);

		auto type = fn.return_type;
		return type;
	}
	case AST_UNOP: {
		auto&& unop = (_unop*)expr;

		auto fn = lookup_unop(unop, scope_stack, functions);

		auto type = fn.return_type;
		return type;
	}
	case AST_FCALL: {
		auto&& fcall = (_fcall*)expr;
		auto&& fn = lookup_fn(fcall->id, fcall->argument_list, functions);
		auto type = fn.return_type;
		return type;
	}
	case AST_INT:  { return int_type; }
	case AST_REAL: { return real_type; }
	case AST_BOOL: { return bool_type; }
	case AST_TEXT: { return text_type; }
	}
}

_fn& _semantic_analyzer::lookup_fn(string id, vector<_arg> args, function_table& functions)
{
	try {
		auto&& overload_set = functions.at(id);
		for (auto&& fn : overload_set) {
			if (fn.id == id) {
				bool success = true;
				for (int i = 0; i < fn.argument_list.size(); ++i) {
					if (args[i] != fn.argument_list[i])
						success = false;
				}
				if (success) return fn;
			}
		}
		// if we reach this point in the execution we did not find a function matching the argument list
		throw _semantic_error(__FILE__, __LINE__, "function definition not found, no functions found with matching arguments: ", id + fn_to_string(id, args));
	}
	catch (std::out_of_range) {
		// if we reach this point in the execution we did not find a function matching the name
		throw _semantic_error(__FILE__, __LINE__, "function definition not found, no functions found with matching name: ", id + fn_to_string(id, args));
	}
}

_fn& _semantic_analyzer::lookup_binop(_binop* binop, stack<_scope> scope_stack, function_table& functions)
{
	string fn_id = token_to_string(binop->op);
	_arg lhs_arg, rhs_arg;
	lhs_arg.type = typeof(binop->lhs, scope_stack, functions);
	rhs_arg.type = typeof(binop->rhs, scope_stack, functions);
	vector<_arg> fn_args = { lhs_arg, rhs_arg };

	auto&& fn = lookup_fn(fn_id, fn_args, functions);
	return fn;
}

_fn& _semantic_analyzer::lookup_unop(_unop* unop, stack<_scope> scope_stack, function_table& functions)
{
	string fn_id = token_to_string(unop->op);
	_arg arg;
	arg.type = typeof(unop->rhs, scope_stack, functions);
	vector<_arg> fn_args = { arg };

	auto&& fn = lookup_fn(fn_id, fn_args, functions);
	return fn;
}

string _semantic_analyzer::fn_to_string(string id, vector<_arg> args)
{
	string fn_string;
	fn_string = id + " ( ";
	if (args.size() == 0);
	else if (args.size() == 1) {
		fn_string += args[0].id + ":" + args[0].type.name;
	}
	else for (int i = 0; i < args.size(); ++i) {
		auto&& arg = args[i];

		if (i == args.size() - 1)
			fn_string += arg.id + ": " + arg.type.name;
		else 
			fn_string += arg.id + ": " + arg.type.name + ", ";
	}
	fn_string += " ) ";
	return fn_string;
}

bool _semantic_analyzer::name_equivalent(_type lt, _type rt)
{
	return lt.name == rt.name;
}

bool _semantic_analyzer::empty_type(_type& t)
{
	return t.name == "" && t.expr == nullptr;
}


void _semantic_analyzer::typecheck(_module& mdl, stack<_scope> scope_stack)
{
	// make sure each variable declaration makes sense
	for (auto&& bucket : mdl.module_scope.local_symbols) 
		for (auto&& pair : bucket) {
			auto&& decl = pair.second;
			typecheck(decl, scope_stack, mdl.functions);
		}

	// make sure each statement in the function makes sense
	for (auto&& pair : mdl.functions)
		for (auto&& fn : pair.second)
			typecheck(fn, scope_stack, mdl.functions);

}

void _semantic_analyzer::typecheck(_fn& fn, stack<_scope> scope_stack, function_table& functions)
{
	// what do we need to ensure about functions?
	// 1: every individual statement needs to be typechecked
	// 2: every return statement must have the same type as the
	//		functions return type.
	scope_stack.push(fn.body);
	auto&& local_scope = scope_stack.top();

	for (auto&& stmt : local_scope.statements) {
		typecheck_statement(stmt, fn, scope_stack, functions);
	}

	scope_stack.pop();
}

void _semantic_analyzer::typecheck_statement(_ast* stmt, _fn& fn, stack<_scope> scope_stack, function_table& functions)
{
	switch (stmt->ast_type) {
	case AST_RETURN: {
		auto&& return_stmt = (_return*)stmt;
		typecheck(*return_stmt, fn, scope_stack, functions);
		break;
	}
	case AST_IF: {
		auto&& if_stmt = (_if*)stmt;
		typecheck(*if_stmt, fn, scope_stack, functions);
		break;
	}
	case AST_WHILE: {
		auto&& while_stmt = (_while*)stmt;
		typecheck(*while_stmt, fn, scope_stack, functions);
		break;
	}
	case AST_SCOPE: {
		auto&& scope_stmt = (_scope*)stmt;
		typecheck(*scope_stmt, fn, scope_stack, functions);
		break;
	}
	default: { // the only other option is an infix expr
		typecheck_expression(stmt, fn, scope_stack, functions);
		break;
	}
	}
}

void _semantic_analyzer::typecheck(_scope& scope, _fn& fn, stack<_scope> scope_stack, function_table& functions)
{
	scope_stack.push(scope);
	auto&& local_scope = scope_stack.top();

	// we typecheck a scope, by typechecking it's body.

	for (auto&& stmt : local_scope.statements) {
		typecheck_statement(stmt, fn, scope_stack, functions);
	}

	scope_stack.pop();
}

void _semantic_analyzer::typecheck(_vardecl& decl, stack<_scope> scope_stack, function_table& functions)
{
	// what does the type system need to say about module declarations?
	// the stated type of the declaration must match the type of it's init expression
	// if the type of the declaration was inferred from the init expr, 
	// then yes this is a 'redundant' check. but this is v1.
	auto&& init_type = typeof(decl.init, scope_stack, functions);
	if (!name_equivalent(decl.lhs.type, init_type))
		throw _semantic_error(__FILE__, __LINE__, "decl type: " + decl.lhs.type.name + " does not match init type: " + init_type.name);
}


void _semantic_analyzer::typecheck(_vardecl& decl, _fn& fn, stack<_scope> scope_stack, function_table& functions)
{
	// what does the type system need to say about local declarations?
	// the stated type of the local decl must match the type of it's init expression.
	auto&& init_type = typeof(decl.init, scope_stack, functions);
	if (!name_equivalent(decl.lhs.type, init_type))
		throw _semantic_error(__FILE__, __LINE__, "decl type: " + decl.lhs.type.name + " does not match init type: " + init_type.name);
}

void _semantic_analyzer::typecheck(_if& if_cond, _fn& fn, stack<_scope> scope_stack, function_table& functions)
{
	// what does the type system need to say about if conditionals?
	// the expression in the condition must have result type boolean
	// if the if conditional is being used as the return expression,
	//	then the return type of both branches must be equivalent.
	//  (since we allow the else branch to be elided, the result
	//	 type only needs to be equivalent if the else branch exists.)
	// this leads us into a discussion of control flow analysis.
	// whereby we check to see that a function returns some known
	// and valid value at the conclusion of each of it's control paths.
	// but that seems like a different task than typechecking an if conditional.
	// so perhaps we need to develop another set of functions to implement
	// that behavior, and typechecking only consists of ensuring that
	// the condition is boolean?
	auto&& cond_type = typeof(if_cond.cond, scope_stack, functions);
	if (!name_equivalent(cond_type, bool_type))
		throw _semantic_error(__FILE__, __LINE__, "condition of if statement must be boolean, instead type was: " + cond_type.name);

	// then the bodies of the if statement need to be typechecked
	typecheck_statement(if_cond.then, fn, scope_stack, functions);
	typecheck_statement(if_cond.els, fn, scope_stack, functions);
}

void _semantic_analyzer::typecheck(_while& while_loop, _fn& fn, stack<_scope> scope_stack, function_table& functions)
{
	// what does the type system need to say about while loops?
	// the expression in the condition must have the result type boolean.
	// if the while loop appears as the return expression (by being explicitly stated
	// via the return statement, or by implication as the last statement in the body of
	// a function.) then the last statement in the while loops body is assumed to be the
	// type and return value of the entire function.
	auto&& cond_type = typeof(while_loop.cond, scope_stack, functions);
	if (!name_equivalent(cond_type, bool_type))
		throw _semantic_error(__FILE__, __LINE__, "condition of while statement must be boolean, instead type was: " + cond_type.name);

	// then the body of the while loop needs to be typechecked
	typecheck_statement(while_loop.body, fn, scope_stack, functions);
}

void _semantic_analyzer::typecheck(_return& ret, _fn& fn, stack<_scope> scope_stack, function_table& functions)
{
	// what does the type system need to say about return expressions?
	// the type of the return expression must match the return type of
	// the function. (at this point the return type of the function has
	// been inferred)
	auto&& ret_expr_type = typeof(ret.expr, scope_stack, functions);
	if (!name_equivalent(ret_expr_type, fn.return_type))
		throw _semantic_error(__FILE__, __LINE__, "return type: " + ret_expr_type.name + " does not match expected return type: " + fn.return_type.name);
}

void _semantic_analyzer::typecheck_expression(_ast* expr, _fn& fn, stack<_scope> scope_stack, function_table& functions)
{
	// what does the type system need to say about affix expressions?
	// the sequence of types flowing through the expression must make sense
	// this implies that this function is going to need to walk the structure
	// of the expression to fully typecheck the expression.
	/*
		binop: lhs and rhs type + op, must uniquely identify a visible function.
		unop: rhs + op, must uniquely identify a visible function.
		postop: the function name and argument list must uniquely identify a visible function.
		var: the var name must identify a visible symbol
		literal: nothing needs to be checked; here is where we would calculate
					the result of primitive oprations on literal types, but this is v1.
	*/
	if (expr == nullptr) {
		// empty expressions are valid right?
		// they are semantically useful without
		// adding any unnessecary ambiguity right?
	}
	else {
		_typecheck_expression(expr, fn, scope_stack, functions);
	}
}

_type _semantic_analyzer::_typecheck_expression(_ast* expr, _fn& fn, stack<_scope> scope_stack, function_table& functions)
{
	// what does the type system need to say about affix expressions?
	// the sequence of types flowing through the expression must make sense
	// this implies that this function is going to need to walk the structure
	// of the expression to fully typecheck the expression.
	/*
		binop: lhs and rhs type + op, must uniquely identify a visible function.
		unop: rhs + op, must uniquely identify a visible function.
		postop: the function name and argument list must uniquely identify a visible function.
		var: the var name must identify a visible symbol
		literal: nothing needs to be checked; here is where we would calculate
					the result of primitive oprations on literal types, but this is v1.
	*/
	switch (expr->ast_type) {
	case AST_BINOP: {
		auto binop = (_binop*)expr;
		// just because we found it, doesn't mean
		// we found every function which composes it, 
		// so this code isn't handling the recursive typechecking
		// yet. 1/31/2020
		auto fn = lookup_binop(binop, scope_stack, functions);
	}
	case AST_UNOP: {

	}
	case AST_FCALL: {

	}
	case AST_VAR: {

	}
	case AST_INT: {

	}
	case AST_REAL: {

	}
	case AST_TEXT: {

	}
	case AST_BOOL: {

	}
	}
}


	// to typecheck the body we must consider the idea of scopes.
	// the body of a function consists of a scope. wherein we
	// may see new declarations, and new statements.
	// there is a special kind of declaration which exists in
	// functions, and that is the scope. a scope is a new
	// local namespace where variables can be defined.
	// scopes are a semantic tool for programmers.
	// mainly they extend the body of if and while constructs
	// to any arbitrary size, instead of being limited to a
	// single expression. scopes control the lifetime
	// of variables and the state they represent declared locally.
	// so a variable declared in a new scope will be
	// destroyed at the end of it's enclosing scope.
	// variables can shadow outer declarations, changing the
	// names meaning locally.
	// this allows programmers to reuse names 
	// which can increase legibility.
	// scopes also serve to describe the control paths of
	// a function together with the if and while constructs.

	// Note:
	// the function itself exists in a module scope, which is
	// defined by the file the function definition is found in.
	/*
		the reasoning behind this is that modules are a way
		of thinking about, and organizing your program
		in a coarse grained way. something which is essential
		if you want to build programs compositionally. (a good thing)
		modules act as a way to encapsulate some particular semantics
		which can be understood as existing independently in your program.
		like a data-structure, or an interface, or a type, etc...

		all this to preface, files are the most natural
		coarse grained abstraction which is flexible enough, and
		simple enough to represent an encapsulation of semantics. (to me)
		it also already has the metaphoric overloading of being it's
		own local namespace and local storage area.

		one could say, hey why not make this "encapsulation of
		semantics" mean a folder of files like in java, so the module
		itself can be composed of multiple files which can help programers
		to compositionally write larger modules?

		and my response to that is the folder abstraction already
		creates an association between multiple files/modules, so why
		not make a new abstraction? what about something akin to a top file
		in hardware programming languages?  

		so, the main difference between an executable and a library
		is that an exe has a predefined starting location (it's root of execution)
		the libraries use case is not to be executed on it's own, it just 
		provides some predefined behavior to be called by other programs,
		(either compiled in, or dynamically linked)

		if there was some structure, lets call it a root, which defined the
		"starting point of execution" of an executable;
		then any file/module included in the project can never define another root.
		a library would be a collection of modules where no module can define a root.

		lets say we have then two abstractions in the language to define files in the language.
		just for the sake of argument:

		module <module-name>? := { <module-statements>* }
		
		library <library-name>? := { <library-statements>* }

		module can choose to define a root. maybe a single function named main will count,
		otherwise there will also be a statement which defines a new root.

		library must never define a root. and is simply used to talk about a
		collection of modules as a single semantic unit. which allows programmers
		to talk about their programs at an even coarser semantic level.
		in fact, if we let libraries be composed of other libraries, then
		this allows for arbitrary coarseness, specified entirely by the programmer.

		and there will be one abstraction to include both libraries and modules;
		whatever include mechanism is eventually decided upon. at most this means
		creating two versions and disambiguating the correct one for the user.

		so, this is really starting to bleed into the actual codegen portion of
		the program for me, because what we are really concerned about at this point
		is what we mean when we say, the static definition of the program vs.
		the dynamic definition of the program.

		the static definitin of the program is superficially the portion of the
		code which is compiled into the physical executable which can be stored
		on disk. (we could imagine as users creating copies of this object file,
		but that is outside of the scope of the compiler)

		the dynamic definition of the program is superficially the portion of the
		code which exists in the memory of the computer as the computer is executing
		the program. (we could imagine as the OS some portions of the executable being
		written to disk during execution due to the size of the executable, but that is
		invisible to the host program, and as such it is outside of the scope of the
		compiler)

		so, the static definition needs to contain at least enough information for the
		loader to take the static definition to the dynamic definition.

		and libraries need to contain enough information to be either a) compiled into
		a static definition, or b) loaded into a dynamic definition.

		so, the global scope of any program is not something which exists that the compiler 
		creates and not the user, the global scope of a program is simply the top level
		module scope, which is also the module scope which contains the root of the program.

		if this top level of compilation is a library, then we expect there to be no root
		and the resulting file will need to contain enough information to be reconstituted
		by the compiler.

		so, libraries cannot be the top level file of an executable, and must be
		the top level file of a library. well, then maybe the distinction between
		does the compiler create an executable or a library rests on wether the
		user's top compilation file is a module or a library.

		well what about static vs dynamic libraries? 
		well, maybe that is a library statement, so is programmer specified.

		library my-lib := {
		#{ module or library includes, etc. #}
		linkage = static;
		or
		linkage = dynamic;
		#{ ... #}
		}

		well, what about static vs dynamic memory, you ask. does every module get it's
		own allocation space? is there one global store of memory which is subdivided?
		those two ideas are not mutually exclusive, but the same thing from two perspectives.

		so we have one 'global' space, that is, the top level scope. then we have the first
		execution local scope, that is, the local scope of the root function, and from there
		it is programmer defined.

		nothing executes before the first statement of the root function.

		if the program needs to initialize anything the programmer must specify that somehow
		after the root of execution.

		how do we initialize our data then? 
		after the beginning of the execution of your program.
		the work of initialization will need to be done wether
		we have code that can run before the root or not, and 
		it clarifies the notion of the root of execution; so
		we require the programmer to specify all static data.
		this also simplifies the notion of what the compiler will
		try to put into the static memory of the program. (statically
		known strings and constants that are reflected over)

		if we are talking about dynamic memory, then we are talking
		about memory allocated from the heap, and the pointer
		which will refrence that data must be known statically.
		of course we can dynamically create an object with a pointer,
		but that is besides the point. 

	*/

	// the file/module scope itself exists in a larger global
	// scope where modules can be composed together. this is done
	// by the linker when we resolve the static refrences accross modules,
	// and then done again by the loader when resolving the dynamic
	// refrences accross modules. the global namespace should be used to describe
	// variables, and the physical resources that the program has access to. 
	// the file/module boundary is to facilitate building larger projects
	// in a compositional/modular way. each file/module defines it's own
	// local namespace, within this namespace we cannot see symbols defined
	// in other namespaces unless we import a name they explicitly export.
	// we also allow local names to shadow the global names in the same way
	// that functions local scopes do. the other option is to preface imported
	// names with their module name, and access module local names by 
	// reffering to them with their fully qualified names (like c++ does)
	// this however implies that the global namespace contains -all- of the
	// names in the program, instead of just what is exported explicitly.
	// I feel that this subtly defeats the purpose of namespaces. 
	// modules have a variety of usecases though
	/*
		say we had a module which defines a data-structure, that module
		is probably expecting to be used by the programmer via,
		the programmer using names defined as the type of the data
		structure, and then using the predefined semantics to build
		up their own semantics. meaning the module acts more as a template
		for the data-structure, whereby many of these data structure
		objects could be created and destroyed throughout the life of
		the program.
		if the module was instead defining something like an interface
		to some hardware (a driver), then at first glance it wouldn't make sense for
		programmers to create a bunch of these interfaces, because there is
		a finite number of actual hardware pieces to interface with.
		this difference can be managed by the writers of the modules themselves
		by choosing which names to expose and which to keep hidden.

	*/

	// the global scope is the conceptual boundary between the program and the outside world.
	// when we consider I/O, we consider I/O accross this boundary.
	// (if you derefrence some pointer to write to a hardware module
	//  that action crosses the global scope of your program to interface
	//  with the external world.)
	// when we consider program interoperation we consider the programs
	// interoperating accross their respective boundaries.
	// if we consider threads, or forks, or the static definitions of the program
	// we consider their operation within this boundary. 
	// this is the internal world of the language.
	
