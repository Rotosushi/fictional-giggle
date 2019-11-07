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
	// state
	_module curr_module;
	stack<_scope> scopes;
	

	// helper functions
	_scope& curr_scope();
	void push_scope(_scope& scope);
	void pop_scope();

	

	// we need helper functions to query our symbol tables
	// we query variables by name
	_vardecl* lookup_vardecl(string id);

	// we query functions by type and by argument list
	_fn* lookup_fn(string id, vector<_arg> args);


	void resolve_type(_vardecl& dec);
	void resolve_type(_var& var);
	void resolve_type(_ast* expr);
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

	bool typecheck_conditional(_if& conditional);
	bool typecheck_iteration(_while& loop);
	
	_type typecheck_expression(_ast* expr);

	
};