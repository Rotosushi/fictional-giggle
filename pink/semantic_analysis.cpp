
#include <stdexcept>


#include "semantic_analysis.h"
#include "error.h"

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

void _semantic_analyzer::infer_types(_module& mdl)
{
	// at some point in the future i am sure this program will be
	// defined using design patterns to better support
	// a more dynamic semantics, but this is version one,
	// so getting something working is the first
	// priority.

	// local scope and global scope are the same at the top level
	// so i guess this wastes clock cycles checking the dummy
	// functions symbol table for 
	// make sure each symbol at a global level has a defined type
	for (auto&& pair : mdl.global_symbols) 
		infer_global_type(pair.second, mdl.global_symbols, mdl.functions);

	// make sure every symbol in the local scopes of every function has a defined type
	for (auto&& pair : mdl.functions)
		for (auto&& fn : pair.second) // pair.second == overload_set
			for (auto&& pair : fn.body.local_symbols)
				infer_type(pair.second, fn, mdl.global_symbols, mdl.functions);
}



_type _semantic_analyzer::typeof(_ast* expr, _fn& local_function, symbol_table& global_symbols, function_table& functions)
{
	switch (expr->ast_type) {
	case AST_VAR: { 
		auto var = (_var*)expr;
		// this seems like the wrong place to put this, but i don't have
		// a location earlier than this that makes sense right now,
		// this statement is necessary because we could be encountering a declaration
		// as the first statement in a function. and the parser does not
		// know what the type of 'var x := 2 + y;' is. it only looks at the lexical structure.
		if (empty_type(var->type)) {
			_vardecl* decl;
			
			// this ordering should remain constant accross all typechecking,
			// local names bind tighter than global names, (a.k.a. name shadowing)
			try {
				// is this variable refrencing a local symbol?
				decl = &local_function.body.local_symbols.at(var->id);
				var->type = decl->lhs.type;
			}
			catch (std::out_of_range) {
				try {
					// is this variable refrencing a global symbol?
					decl = &global_symbols.at(var->id);
					var->type = decl->lhs.type;
				}
				catch (std::out_of_range) {
					throw _semantic_error(__FILE__, __LINE__, "Variable used but not defined. variable:", var->id);
				}
			}
		}
		return var->type;
	}
	case AST_BINOP: { 
		auto binop = (_binop*)expr;
		// the type of a binop expression is the result type of the
		// underlying function.
		if (empty_type(binop->type)) {
			string fn_id = token_to_string(binop->op);
			_arg lhs_arg, rhs_arg;
			lhs_arg.type = typeof(binop->lhs, local_function, global_symbols, functions);
			rhs_arg.type = typeof(binop->rhs, local_function, global_symbols, functions);
			vector<_arg> fn_args = { lhs_arg, rhs_arg };
			auto fn = lookup_fn(fn_id, fn_args, functions);
			binop->type = fn.return_type;
		}
		// either way we return the type.
		return binop->type;
	}
	case AST_UNOP: {
		auto unop = (_unop*)expr;
		if (empty_type(unop->type)) {
			string fn_id = token_to_string(unop->op);
			_arg arg;
			arg.type = typeof(unop->rhs, local_function, global_symbols, functions);
		}
		return unop->type;
	}
	case AST_SCOPE: {
		auto scope = (_scope*)expr;
		// the typeof a scope, could be None,
		// the type of all return expressions in the function
		// or if there are no returns the type of the last statement
		// in the scope.
	}
	case AST_IF: {
		// the type of an if is either None, (if it had no return stmts,
		// and the type of
	}
	case AST_WHILE: {

	}
	case AST_RETURN: { 
		auto ret = (_return*)expr;
		if (empty_type(ret->type)) {
			auto expr_type = typeof(ret->expr, local_function, global_symbols, functions);
			ret->type = expr_type;
		}
		return ret->type;
	}
	case AST_FCALL: { 
		auto fcall = (_fcall*)expr;
		auto fn = lookup_fn(fcall->id, fcall->argument_list, functions);
		if (empty_type(fcall->return_type))
			fcall->return_type = fn.return_type;
		return fcall->return_type;
	}
	case AST_FN: { 
		auto fn = (_fn*)expr;
		if (empty_type(fn->return_type)) {
			infer_type(*fn, global_symbols, functions);
		}
		return fn->return_type;
	}
	case AST_ARG: { 
		// AST_ARG is a special case, for now we don't allow functions
		// to elide the types of their arguments, however we may need to
		// know the type of an argument, so this case exists.
		auto arg = (_arg*)expr;
		return arg->type;
	}
	case AST_EXPR: { 
		auto e = (_expr*)expr;
		if (empty_type(e->type)) {
			e->type = typeof(e->expr, local_function, global_symbols, functions);
		}
		return e->type;
	}
	case AST_INT:
		return int_type;
	case AST_REAL:
		return real_type;
	case AST_TEXT:
		return text_type;
	case AST_BOOL:
		return bool_type;
	}
}

_type _semantic_analyzer::typeof_global(_ast* expr, symbol_table& global_symbols, function_table& functions)
{
	switch (expr->ast_type) {
	case AST_VAR: {
		auto var = (_var*)expr;
		// this seems like the wrong place to put this, but i don't have
		// a location earlier than this that makes sense right now,
		// this statement is necessary because we could be encountering a declaration
		// as the first statement in a function. and the parser does not
		// know what the type of 'var x := 2 + y;' is. it only looks at the lexical structure.
		if (empty_type(var->type)) {
			_vardecl* decl;

			try {
				// is this variable refrencing a global symbol?
				decl = &global_symbols.at(var->id);
				var->type = decl->lhs.type;
			}
			catch (std::out_of_range) {
				throw _semantic_error(__FILE__, __LINE__, "Variable used but not defined. variable:", var->id);
			}
		}
		return var->type;
	}
	case AST_BINOP: {
		auto binop = (_binop*)expr;
		// the type of a binop expression is the result type of the
		// underlying function.
		if (empty_type(binop->type)) {
			string fn_id = token_to_string(binop->op);
			_arg lhs_arg, rhs_arg;
			lhs_arg.type = typeof_global(binop->lhs, global_symbols, functions);
			rhs_arg.type = typeof_global(binop->rhs, global_symbols, functions);
			vector<_arg> fn_args = { lhs_arg, rhs_arg };
			auto fn = lookup_fn(fn_id, fn_args, functions);
			binop->type = fn.return_type;
		}
		// either way we return the type.
		return binop->type;
	}
	case AST_UNOP: {
		auto unop = (_unop*)expr;
		if (empty_type(unop->type)) {
			string fn_id = token_to_string(unop->op);
			_arg arg;
			arg.type = typeof_global(unop->rhs, global_symbols, functions);
		}
		return unop->type;
	}
	case AST_FCALL: {
		auto fcall = (_fcall*)expr;
		auto fn = lookup_fn(fcall->id, fcall->argument_list, functions);
		if (empty_type(fcall->return_type))
			fcall->return_type = fn.return_type;
		return fcall->return_type;
	}
	case AST_INT:
		return int_type;
	case AST_REAL:
		return real_type;
	case AST_TEXT:
		return text_type;
	case AST_BOOL:
		return bool_type;
	}
}

void _semantic_analyzer::infer_type(_fn& fn, symbol_table& global_symbols, function_table& functions)
{
	// so, this logic will catch returns at the top level of
	// a function, but how do we catch returns at any arbitrary
	// scope depth?
	bool first_return = true;
	for (auto&& stmt : fn.body.statements) {
		if (stmt->ast_type == AST_RETURN) {
			
			auto return_stmt = (_return*)stmt;
			
			if (empty_type(return_stmt->type)) {
				return_stmt->type = typeof(return_stmt->expr, fn, global_symbols, functions);
			}

			if (first_return) {
				// if this is the first return statement,
				// we infer this functions type from it.
				first_return = false;

				fn.return_type = return_stmt->type;
			}
			else {
				// this isn't the first return statement we have seen,
				// but do the types match? (the types obviously match 
				// if it's the first return.
				if (!name_equality(fn.return_type, return_stmt->type))
					throw _semantic_error(__FILE__, __LINE__, "return type does not match functions return type");
			}
		} 

	}

	// if we didn't see a return statement the
	// functions return type is infered as none.
	if (!first_return) {

	}
}

void _semantic_analyzer::infer_global_type(_vardecl& decl, symbol_table& global_symbols, function_table& functions)
{
	// if the name is not empty then the type was defined when we parsed
	// the declaration. meaning the user explicitly typed out the type, so
	// the variables type does not need to be infered.

	if (empty_type(decl.lhs.type)) {
		// if the ename is empty, then the user elided specifying the type
		// and the type will be inferred from the initialization expression.
		_type inferred_type = typeof_global(decl.init, global_symbols, functions);
		decl.lhs.type = inferred_type;
	}
	// the third case, where both type and initialization
	// are elided, is a syntax error. and so will be reported in the parser
}

void _semantic_analyzer::infer_type(_vardecl& decl, _fn & local_function, symbol_table& global_symbols, function_table& functions)
{
	// if the expr is not null then the type was defined when we parsed
	// the declaration. meaning the user explicitly typed out the type.
	if (empty_type(decl.lhs.type)) {
		// if the expression is null, then the user elided specifying the type
		// and the type will be inferred from the initialization expression.
		_type inferred_type = typeof(decl.init, local_function, global_symbols, functions);
		decl.lhs.type = inferred_type;
	}
	// the third case, where both type and initialization
	// are elided, is a syntax error. and so will be reported in the parser
}

_fn& _semantic_analyzer::lookup_fn(string id, vector<_arg> args, function_table& functions)
{
	try {
		auto&& overload_set = functions.at(id);
		for (auto&& fn : overload_set) {
			if (fn.id == id) {
				bool success = true;
				for (int i = 0; i < fn.argument_list.size(); ++i) {
					if (!(args[i] == fn.argument_list[i]))
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
	for (auto arg : args) {
		fn_string += arg.id + ":" + arg.type.name + " ";
	}
	fn_string += " ) ";
	return fn_string;
}

bool _semantic_analyzer::empty_type(_type& t)
{
	return t.name == "" && t.expr == nullptr;
}

void _semantic_analyzer::typecheck(_module& mdl)
{
	// make sure each statement in the function makes sense
	// make sure functions that return a value have
	// an appropriate return type on each unique control
	// path in the function.
	// if there is no return expression, the last statement
	// of the function is assumed to be the return value.
	for (auto&& pair : mdl.functions)
		for (auto&& fn : pair.second)
			typecheck(fn, mdl.global_symbols);


}

void _semantic_analyzer::typecheck(_fn& f, symbol_table& global_symbols)
{
	// step 1: make sure each statement in each function makes sense
	for (auto&& stmt : f.body.statements) {
		// 
		switch (stmt->ast_type) {
		case AST_IF: {
			auto if_stmt = (_if*)stmt;
			typecheck(*if_stmt, f, global_symbols);
		}
		case AST_WHILE: {
			auto while_stmt = (_while*)stmt;
			typecheck(*while_stmt, f, global_symbols);
		}
		case AST_RETURN: {
			auto return_stmt = (_return*)stmt;
			typecheck(*return_stmt, f, global_symbols);
		}
		case AST_SCOPE: {
			auto scope_stmt = (_scope*)stmt;
			typecheck(*scope_stmt, f, global_symbols);
		}
		default: { // an expression
			// there are no constraints around what an affix expression
			// needs to do, or what types it needs to operate on.
			// they will still need to be analyzed as any affix
			// expression's exectution makes up the complete algorithm
			// that the function executes. that analysis is not typechecking
		}
		}
	}

	// step 2: ensure every return expression matches the return type
	//			  of the function. infered as the type of the first return stmt
	//			  if the return type is elided; and if no return statements are
	//			  present in the function the function implicitly returns it's
	//			  last statement. this can be circumnavigated by specifying
	//			  the return type as None, or by letting the compiler infer
	//			  the return type via a 'return None' statement, or by
	//			  having the result type of the last statement of the function
	//			  be None.

	for (auto&& stmt : f.body.statements) {
		if (stmt->ast_type == AST_RETURN) {
			auto return_stmt = *((_return*)stmt);

			// is the return type equal to the return type of the fn?
			if (!name_equality(return_stmt.type, f.return_type))
				throw _semantic_error(__FILE__, __LINE__, "typeof return expr: " + return_stmt.type.name + " does not match return type of function: " + f.return_type.name);
		}
	}
	

}

void _semantic_analyzer::typecheck(_if& i, _fn& f,symbol_table& global_symbols)
{
	// typechecking if statements require two things,
	// step 1: make sure the if condition is

}

void _semantic_analyzer::typecheck(_while& w, _fn& f,symbol_table& global_symbols)
{
}

void _semantic_analyzer::typecheck(_return& r, _fn& f,symbol_table& global_symbols)
{
	
}
