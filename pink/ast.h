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

typedef struct _var : public _ast {
	string id;
	_type type;
	vector<_ast*> postops;

	_var() : _ast(AST_VAR), id(), type(), postops() {}
} _var;

typedef struct _vardecl : public _ast {
	_var lhs;
	_token op;
	_expr init;

	_vardecl() : _ast(AST_VARDECL), lhs(), op(), init() {}
} _vardecl;

typedef struct _scope : public _ast {
	map <string, _vardecl> variables;
	vector <_ast*> statements;

	_scope() : _ast(AST_SCOPE), variables(), statements() {}
} _scope;

_vardecl& resolve(_scope& scope, string id);
void define(_scope& scope, _vardecl& decl);

typedef struct _arg : public _ast {
	string id;
	_type type;
	_ast* type_expression;

	_arg() : _ast(AST_ARG), id(), type(), type_expression() {}
} _arg;

typedef struct _fn : public _ast {
	string id;
	vector<_arg> argument_list;
	vector<_arg> return_list;
	_scope body;

	_fn() : _ast(AST_FN), id(), argument_list(), return_list(), body(){}
} _fn;

typedef struct _if : public _ast {
	_ast* cond;
	_ast* then;
	_ast* els;

	_if() : _ast(AST_IF), cond(nullptr), then(nullptr), els(nullptr) {}
} _if;

typedef struct _while : public _ast {
	_ast* cond;
	_ast* body;
	_ast* els;

	_while() : _ast(AST_WHILE), cond(nullptr), body(nullptr), els(nullptr) {}
} _while;

typedef struct _expr : public _ast {
	vector <_ast*> expr_list;

	_expr() : _ast(AST_EXPR), expr_list() {}
} _expr;

typedef struct _binop : public _ast {
	_token op;
	_ast* lhs;
	_ast* rhs;

	_binop() : _ast(AST_BINOP), op(T_ERR), lhs(nullptr), rhs(nullptr) {}
	_binop(_token o, _ast* l, _ast* r) : _ast(AST_BINOP) {
		op = o;
		lhs = l;
		rhs = r;
	}
} _binop;

typedef struct _unop : public _ast {
	_token op;
	_ast* rhs;

	_unop() : _ast(AST_UNOP), op(), rhs(nullptr) {}
} _unop;

typedef struct _return : public _ast {
	_expr expr;

	_return() : _ast(AST_RETURN), expr() {}
} _return;

typedef struct _fcall : public _ast {
	vector<_arg> argument_list;

	_fcall() : _ast(AST_FCALL), argument_list() {}
} _fcall;

typedef struct _module : public _ast {
	string id;
	vector <string> import_list;
	vector <string> export_list;
	string root;
	map<string, _fn> functions;
	map<string, _vardecl> variables;

	_module() : _ast(AST_MODULE), id(), import_list(), export_list(), root(), functions(), variables(){}
} _module;



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