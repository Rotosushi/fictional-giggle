#pragma once
#include <string>
using std::string;
#include <stack>
using std::stack;
#include "ast.h"
#include "type.h"


class _semantic_analyzer {
public:
	_semantic_analyzer() {}

	void init_module_with_kernel(_module& m);

	void infer_types(_module& mdl);
	

	_type typeof(_ast* expr, _fn& local_function, symbol_table& global_symbols, function_table& functions);
	_type typeof_global(_ast* expr, symbol_table& global_symbols, function_table& functions);


	void infer_type(_fn& fn, symbol_table& global_symbols, function_table& functions);

	// note, the only reason we don't need a refrence to a table of type definitions
	// is because there are only four types in v1, all of them primitive.
	void infer_global_type(_vardecl& decl, symbol_table& global_symbols, function_table& functions);
	void infer_type(_vardecl& decl, _fn & local_function, symbol_table& global_symbols, function_table& functions);


	_fn& lookup_fn(string id, vector<_arg> args, function_table& functions);
	string fn_to_string(string id, vector<_arg> args);

	bool name_equality(_type& lt, _type& rt) { 
		return lt.name == rt.name;
	}

	bool structural_equality(_type& lt, _type& rt) {
		// note, the reason this works is that
		// there are only four types in the language,
		// and all of them are primitive.
		// _int, _real, _text, _bool
		// the real definition will need to be recursive
		if (lt.expr == nullptr || rt.expr == nullptr) throw _semantic_error(__FILE__, __LINE__, "Semantic Error: Structural Equality is not defined for nullptr types");
		return lt.expr->ast_type == rt.expr->ast_type;
	}

	bool empty_type(_type& t);

	void typecheck(_module& mdl);
	void typecheck(_fn& f, symbol_table& global_symbols);
	void typecheck(_if& i, _fn& f , symbol_table& global_symbols);
	void typecheck(_while& w, _fn& f , symbol_table& global_symbols);
	void typecheck(_return& r, _fn& f , symbol_table& global_symbols);
	void typecheck(_scope& s, _fn f, symbol_table& global_symbols);



private:
	const _type None_type = { "none", nullptr };
	const _type int_type = { "int", nullptr };
	const _type real_type = { "real", nullptr };
	const _type text_type = { "text", nullptr };
	const _type bool_type = { "bool", nullptr };
	/*
	
		type checking is the first goal of semantic analysis.
		type checking is primarily concerned with three questions.

		type equality:
			what defines one type being equal to the other?

			name equality: types are equal when their names are the same.

			when you define a type with the adt mechanism, the compiler can use the
			type to generate pass-by-value semantics. which means we can also generate
			an overloaded '=' operator for the type.

			when we typecheck a function call, we check if each typename in the
			callee matches the types of the functions type.

			structural equality: types are equal when they have the same underlying representation.

			when we cast between types with equivalent representations we can generate that code for the user.
			when we assign between types with equivalent representation we can leverage the cast mechanism.

		type compatability:
			when can we say that a usage of a type is valid?

			we define compatability first by defining our expectation of use.
			there are four statements which must be considered in v1 of the language.
			choice, iteration, returns, and expressions.

			if-else if-else chains need to be type checked.
			their body then needs to be checked.
			the expression in the condition of the if() needs to have type bool.
			the body has no expectation of type implicitly.

			while loops need to be type checked.
			then the body needs to be checked
			the expression in the condition of the while() needs to have type bool.
			the body has no expectation of type implicitly.

			affix expressions need to be type checked.
			each function call present in the expression must be typechecked.
			the result type of the expression is the type of it's outermost function, variable, or literal.

			return statements constitute an essential property of functions, their returned value.
			a returns statements expression must have the same result type as the
			return type of the function. we consider name equivalence here.
			if the return type of the function has been
			elided, then the first return expression of the function defines the result
			type of the function.
			if there are no return statements in the function, then
			the return statement is assumed to be the last statement in the body of the
			function. if there are no return statements, and the return type of the function
			has been elided, then the return type is considered to be the type of the
			last statement in the body of the function.
			
		type inference:
			how do we tell what type things are?

			the defining appearance of a name must be associated with a type.
			variables must have a defining occurance before they can be used.
			(in the future, the defining occurance of a variable will be extended
			 to include the first time it is encountered as a destination. (an l-value in c terminology.)
			 this will only be allowed in situations where it is unambiguous what the type
			 of the variable is from context.)

			we cannot call functions which have no definition.
			we cannot read or write to variables which do not exist.

			the type of an expression is the type of the outermost
			function call, variable refrence, or literal value.

			the type of a variable is either present in it's definition of inferred from the
			type of the initialization expression.

			the type of a function is it's name + argument list
	*/

};