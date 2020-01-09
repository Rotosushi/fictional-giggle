#pragma once
#include <string>
using std::string;
#include <stack>
using std::stack;
#include "ast.h"


class _semantic_analyzer {
public:
	_semantic_analyzer() {
		
	}

	bool analyze_module(_module& m);
private:
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


	// state
	_module curr_module;
	stack<_scope> scopes;

	// helper functions
	_scope& curr_scope();
	void push_scope(_scope& scope);
	void pop_scope();
	map<string, _vardecl>& global_variables();

	// we need helper functions to query our symbol tables
	// we query variables by name
	_vardecl* lookup_vardecl(string id);

	// we query functions by type and by argument list
	// the argument list is to disambiguate from the
	// overload set.
	_fn* lookup_fn(string id, vector<_arg> args);



	void resolve_type(_vardecl& dec);
	void resolve_type(_var& var);
	void resolve_type(_expr& expr);
	void resolve_type(_fn& fn);
	void resolve_type(_fcall& fcall);
	void resolve_type(_binop& binop);
	void resolve_type(_unop& unop);

	_type typeof(_var& var);
	_type typeof(_vardecl& var);
	_type typeof(_fn& fn);
	_type typeof(_fcall& fn);
	_type typeof(_expr& expr);
	_type typeof(_ast* expr);
	_type typeof(_binop& binop);
	_type typeof(_unop& unop);
	_type typeof(_return& ret);

	_type typecheck_statement(_ast* stmt);
	_type typecheck_conditional(_if& conditional);
	_type typecheck_iteration(_while& loop);
	_type typecheck_return(_return& ret);
	_type typecheck_expression(_ast* expr);
};