#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <map>
using std::map;

#include "token.h"
#include "type.h"

enum _ast_type {
	AST_ERR,
	AST_VAR,
	AST_VARDECL,
	AST_ARG,
	AST_FN,
	AST_FCALL,
	AST_IF,
	AST_WHILE,
	AST_EXPR,
	AST_BINOP,
	AST_UNOP,
	AST_RETURN,
	AST_SCOPE,
	AST_MODULE,
	AST_INT,
	AST_FLOAT,
	AST_TEXT,
	AST_BOOL,
};

string ast_type_to_string(_ast_type at);

typedef struct _ast {
	_ast_type ast_type;

	_ast() { ast_type = AST_ERR; }
	_ast(_ast_type a_t) { ast_type = a_t; }
	virtual ~_ast() {}

} _ast;

typedef struct _expr : public _ast {
	_type type;
	_ast* expr;
	//vector <_ast*> expr_list;

	_expr() : _ast(AST_EXPR), expr() {}
} _expr;

typedef struct _var : public _ast {
	string id;
	_type type;
	_ast* type_expression;

	_var() : _ast(AST_VAR), id(), type() {}
} _var;

_var* create_var(string id, _type type = _NIL, _ast* type_expression = nullptr);

typedef struct _vardecl : public _ast {
	_var lhs;
	_token op;
	_ast* init;

	_vardecl() : _ast(AST_VARDECL), lhs(), op(), init() {}
} _vardecl;

_vardecl* create_vardecl(_var lhs, _token op, _ast* init = nullptr);

typedef struct _scope : public _ast {
	map <string, _vardecl> variable_table;
	vector <_ast*> statements;

	_scope() : _ast(AST_SCOPE), variable_table(), statements() {}
} _scope;

_scope* create_scope(vector<_vardecl> variables, vector<_ast*> statements);

typedef struct _arg : public _ast {
	string id;
	_type type;
	_ast* type_expression;

	_arg() : _ast(AST_ARG), id(), type(), type_expression() {}
} _arg;

_arg* create_arg(string id, _type type, _ast* type_expression = nullptr);

typedef struct _fn : public _ast {
	string id;
	vector<_arg> argument_list;
	_var return_value;
	_scope body;

	_fn() : _ast(AST_FN), id(), argument_list(), return_value(), body(){}
} _fn;

_fn* create_fn(string id, vector<_arg> argument_list, _var return_value, _scope body);

typedef struct _if : public _ast {
	_ast* cond;
	_ast* then;
	_ast* els;

	_if() : _ast(AST_IF), cond(nullptr), then(nullptr), els(nullptr) {}
} _if;

_if* create_if(_ast* cond = nullptr, _ast* then = nullptr, _ast* els = nullptr);

typedef struct _while : public _ast {
	_ast* cond;
	_ast* body;
	_ast* els;

	_while() : _ast(AST_WHILE), cond(nullptr), body(nullptr), els(nullptr) {}
} _while;

_while* create_while(_ast* cond = nullptr, _ast* then = nullptr, _ast* els = nullptr);


typedef struct _binop : public _ast {
	_type type;
	_token op;
	_ast* lhs;
	_ast* rhs;

	_binop() : _ast(AST_BINOP), type(_DEDUCE), op(T_ERR), lhs(nullptr), rhs(nullptr) {}
	_binop(_token o, _ast* l, _ast* r) : _ast(AST_BINOP), type(_DEDUCE) {
		op = o;
		lhs = l;
		rhs = r;
	}
} _binop;

_binop* create_binop(_type type, _token op, _ast* lhs, _ast* rhs);

typedef struct _unop : public _ast {
	_type type;
	_token op;
	_ast* rhs;

	_unop() : _ast(AST_UNOP), op(), rhs(nullptr) {}
} _unop;

_unop* create_unop(_type type, _token op, _ast* rhs);

typedef struct _return : public _ast {
	_type type;
	_ast* expr;

	_return() : _ast(AST_RETURN), expr() {}
} _return;

_return* create_return(_type type, _ast* expr);

typedef struct _fcall : public _ast {
	string id;
	vector<_arg> argument_list;
	_var return_value;

	_fcall() : _ast(AST_FCALL), argument_list() {}
} _fcall;

_fcall* create_fcall(string id, vector<_arg> argument_list, _var return_value);

typedef struct _module : public _ast {
	string id;
	vector <string> import_list;
	vector <string> export_list;
	string root;
	map<string, vector<_fn>> function_table;
	map<string, _vardecl> variable_table;

	_module() : _ast(AST_MODULE), id(), import_list(), export_list(), root(), function_table(), variable_table(){}
} _module;

void init_module_with_kernel(_module& m);

typedef struct _int : public _ast {
	int value;

	_int() : _ast(AST_INT), value(0) {}
	_int(int i) : _ast(AST_INT), value(i) {}
} _int;

typedef struct _float : public _ast {
	float value;

	_float() : _ast(AST_FLOAT), value(0.0) {}
	_float(float f) : _ast(AST_FLOAT), value(f) {}
} _float;

typedef struct _text : public _ast {
	string value;

	_text() : _ast(AST_TEXT), value() {}
	_text(string s) : _ast(AST_TEXT), value(s) {}
} _text;

typedef struct _bool : public _ast {
	bool value;

	_bool() : _ast(AST_BOOL), value(false) {}
	_bool(bool b) : _ast(AST_BOOL), value(b) {}
} _bool;