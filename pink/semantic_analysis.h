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

	void analyze(_module& mdl);

private:
	// dummy convienence definitions
	const _type none_type = { "none", nullptr };
	const _type int_type  = { "int", nullptr };
	const _type real_type = { "real", nullptr };
	const _type text_type = { "text", nullptr };
	const _type bool_type = { "bool", nullptr };

	void init_module_with_kernel(_module& m);

	_fn& lookup_fn(string id, vector<_arg> args, function_table& functions);
	string fn_to_string(string id, vector<_arg> args);

	bool name_equivalent(_type lt, _type rt);
	bool structurally_equivalent(_type lt, _type rt);

	_type resolve_type(_var& var, stack<_scope>& scope_stack);

	void infer_types(_module& mdl, stack<_scope>& scope_stack);

	void infer_type(_fn& fn, stack<_scope>& scope_stack, function_table& functions);
	_type infer_return_type_from_scope(stack<_scope>& scope_stack, function_table& functions);
	void infer_type(_vardecl& decl, stack<_scope>& scope_stack, function_table& functions);

	_type typeof(_ast* expr, stack<_scope>& scope_stack, function_table& functions);

	bool empty_type(_type& t);

	void typecheck(_module& mdl, stack<_scope> scope_stack);
	void typecheck(_fn& fn, stack<_scope> scope_stack, function_table& functions);
	void typecheck(_vardecl& decl, stack<_scope> scope_stack, function_table& functions);
	void typecheck(_if& if_cond, stack<_scope> scope_stack, function_table& functions);
	void typecheck(_while& while_loop, stack<_scope> scope_stack, function_table& functions);
	void typecheck(_return& ret, stack<_scope> scope_stack, function_table& functions);
	void typecheck(_ast* expr, stack<_scope> scope_stack, function_table& functions);
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