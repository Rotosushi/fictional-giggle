#pragma once
#include <string>
using std::string;

#include "ast.h"


class _semantic_analyzer {
public:
	_semantic_analyzer() {}

	bool typecheck_module(_module& m);
private:
	_module curr_module;

	bool is_intrinsic_type(_type t);

	bool typecheck_declaration(_vardecl& var);
	bool typecheck_function(_fndecl& fn);
	bool typecheck_conditional(_if& conditional);
	bool typecheck_iteration(_while& loop);
	bool typecheck_iteration(_dowhile& loop);

	_type typecheck_expression(_ast* expr);

	bool typecheck_lambda(_lambda& l);
	
};