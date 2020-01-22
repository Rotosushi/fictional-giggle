
#include <stdexcept>


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
	// we and to extend.

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

_type _semantic_analyzer::infer_return_type_from_scope(stack<_scope>& scope_stack, function_table& functions) {
	_type return_type;
	// search for a return stmt in the local scope.
	auto&& local_scope = scope_stack.top();

	for (auto&& stmt : local_scope.statements) {
		// if we find a return stmt
		if (stmt->ast_type = AST_RETURN) {
			return_type = typeof(stmt, scope_stack, functions);
			break;
		}
		else if (stmt->ast_type == AST_SCOPE) {
			auto&& local_scope = (_scope*)stmt;
			scope_stack.push(*local_scope);
			auto&& inferred_type = infer_return_type_from_scope(scope_stack, functions);
			scope_stack.pop();
			return_type = inferred_type;
			break;
		}
	}

	return return_type;
};

void _semantic_analyzer::infer_type(_fn& fn, stack<_scope>& scope_stack, function_table& functions)
{
	// for now, inferring the type of a function
	// only means inferring the return type.
	// as that is the only typename that is allowed to
	// be elided by the user for now. 
	// maybe eliding the typename of arguments is useful?
	// but really, if you are going to go that far, why not
	// just get rid of the need for any explicit typing like
	// haskell?
	
	/*
		there are two states that a fn can be in here,
		1: the user explicitly typed in the typename
			meaning we don't need to infer.
		2: the user elided the return type 
			so we infer the type from:
			1: the return type of the first return expression
				we find in the body of the function.
			2: if we don't find a return statement
				(this i am undecided about)
				we can follow in c's footsteps here 
				where the return type is assumed none.
				or we can go the more functional route and treat the last
				statement in the function as the return expression and
				infer the type from it.
	*/
	if (empty_type(fn.return_type)) {
		for (auto&& stmt : fn.body.statements) {
			if (stmt->ast_type == AST_RETURN) {
				auto&& inferred_type = typeof(stmt, scope_stack, functions);

				fn.return_type = inferred_type;
				break;
			}
			else if (stmt->ast_type == AST_SCOPE) {
				auto&& local_scope = (_scope*)stmt;
				scope_stack.push(*local_scope);
				auto&& inferred_type = infer_return_type_from_scope(scope_stack, functions);
				scope_stack.pop();

				fn.return_type = inferred_type;
				break;
			}
		}
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

		string fn_id = token_to_string(binop->op);
		_arg lhs_arg, rhs_arg;
		lhs_arg.type = typeof(binop->lhs, scope_stack, functions);
		rhs_arg.type = typeof(binop->rhs, scope_stack, functions);
		vector<_arg> fn_args = { lhs_arg, rhs_arg };

		auto fn = lookup_fn(fn_id, fn_args, functions);

		auto type = fn.return_type;
		return type;
	}
	case AST_UNOP: {
		auto&& unop = (_unop*)expr;

		string fn_id = token_to_string(unop->op);
		_arg arg;
		arg.type = typeof(unop->rhs, scope_stack, functions);
		vector<_arg> fn_args = { arg };

		auto fn = lookup_fn(fn_id, fn_args, functions);

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
	}
	catch (std::out_of_range) {
		throw _semantic_error(__FILE__, __LINE__, "function definition not found: ", id + fn_to_string(id, args));
	}
}

string _semantic_analyzer::fn_to_string(string id, vector<_arg> args)
{
	string fn_string;
	fn_string = id + " ( ";
	if (args.size() == 0) {}
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
	// make sure functions that return a value have
	// an appropriate return type on each unique control
	// path in the function.
	// if there is no return expression, the function returns None
	for (auto&& pair : mdl.functions)
		for (auto&& fn : pair.second)
			typecheck(fn, scope_stack, mdl.functions);



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
	// the file/module scope itself exists in a larger global
	// scope where modules can be composed together. this is done
	// by the linker when we resolve the static refrences accross modules,
	// and then done again by the loader when resolving the dynamic
	// refrences accross modules. the global namespace should be used to describe
	// variables, and resources that the program has access to. 
	// the file/module boundary is to facilitate building larger projects
	// in a compositional/modular way. each file/module defines it's own
	// local namespace, within this namespace we cannot see symbols defined
	// in other namespaces unless we import a name they explicitly export.
	// we also allow local names to shadow the global names in the same way
	// that functions local scopes do.

	// the global scope is the conceptual boundary between the program and the outside world.
	// when we consider I/O, we consider I/O accross this boundary.
	// (if you derefrence some pointer to write to a hardware module
	//  that action crosses the global scope of your program to interface
	//  with the external world.)
	// when we consider program interoperation we consider the programs
	// interoperating accross their respective boundaries.
	// if we consider threads, or forks, or the static definitions of the program
	// we consider their operation within this boundary. this is the internal world,
	// of the language.
	
