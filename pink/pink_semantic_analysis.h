#pragma once

/*
	semantic analysis

	what questions do we need to ask

	about declarations
		what type is it?
		is this name taken?

	about expressions
		are we refrencing declared variables?
		do the types of the operands make sense?

	about conditionals
		is the type of the expression bool?

	about loops
		is the type of the expression bool?

	about function declarations
		is this name taken?

	about function calls
		are we calling a declared function?
		do we have the correct arguments (type, number, order)


	Pink Semantic Rules

	variable rules
		variables must be defined before they are used
		
		variables are statically typed, type can be inferred
			from rhs of definition, or specified.

	function rules
		functions must be defined before they are used
			(the function however may be defined at a later
				line in the file, or in a later translation
				unit's export list)
			((recursion is allowed, c-like stack based lang))
		functions cannot be defined inside of functions
			but lambdas may be.

	module rules
		modules are defined by a single translation unit
		modules define the name visibility of functions
			and file local variables


		
	scope rules
		conditionals and loops
			have dynamic scope
		lambdas have dynamic scope
		functions have static scope

		modules don't have the concept of scope
		(global and file local variables exist in
			seprate memory)

*/
#include <string>
using std::string;
#include <vector>
using std::vector;
#include "pink_ast.h"
#include "pink_parser.h"


class _semantic_analyzer {
public:
	bool typecheck(_ast * expr, _scope& module_scope);
	bool typecheck(_ast* expr, _scope& module_scope, _scope& current_scope);
	_ast_type _typecheck(_ast* expr, _scope& module_scope);
	_ast_type _typecheck(_ast* expr, _scope& module_scope, _scope& current_scope);
	_ast_type typecheck_binop(_tok op, _ast_type lhs_type, _ast_type rhs_type);
	_ast_type typecheck_unop(_tok op, _ast_type rhs_type);
	_ast_type typecheck_postops(_var* var, _scope& module_scope);
	bool do_semantic_analysis(_declaration& decl, _scope& module_scope);
	bool do_semantic_analysis(_declaration& decl, _scope& module_scope, _scope& current_scope);
	void resolve_declaration(_declaration& decl, _scope& module_scope);
	void resolve_declaration(_declaration& decl, _scope& module_scope, _scope& current_scope);

	_ast_type tok_to_ast_type(_tok tok);
};