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

*/
#include <string>
using std::string;
#include <vector>
using std::vector;
#include "pink_ast.h"
#include "pink_parser.h"


class _semantic_analyzer {
	bool analyze(_scope& current_scope, _scope* parent_scope, _declaration& declaration);
	bool analyze(_scope& current_scope, _scope* parent_scope, _lambda& function);

};