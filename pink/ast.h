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
	AST_LAMBDA,
	AST_FNDECL,
	AST_IF,
	AST_WHILE,
	AST_BINOP,
	AST_UNOP,
	AST_SCOPE,
	AST_MODULE,
	AST_INT,
	AST_FLOAT,
	AST_TEXT,
	AST_BOOL,
};

typedef struct _ast {
	_ast_type ast_type;

	_ast() { ast_type = AST_ERR; }
	_ast(_ast_type a_t) { ast_type = a_t; }
	virtual ~_ast() {}
} _ast;

typedef struct _var : public _ast {
	string id;
	_type type;
	_ast* value;
	vector<_ast*> postops;

	_var() : _ast(AST_VAR), id(), type(), postops() {}
	_var(string i, _type t, _ast* v, vector<_ast*> p) : _ast(AST_VAR) {
		id = i;
		type = t;
		value = v;
		postops = p;
	}
} _var;

typedef struct _vardecl : public _ast {
	_var lhs;
	_token op;
	_ast* rhs;

	_vardecl() : _ast(AST_VARDECL), lhs(), op(), rhs() {}
	_vardecl(_var l, _token o, _ast* r) : _ast(AST_VARDECL) {
		lhs = l;
		op = o;
		rhs = r;
	}
} _vardecl;

typedef struct _scope : public _ast {
	map <string, _vardecl> variables;
	vector <_ast*> expressions;

	_vardecl& resolve(string id) {
		return variables.at(id);
	}

	void define(_vardecl& decl) {
		variables[decl.lhs.id] = decl;
	}

	_scope() : _ast(AST_SCOPE), variables(), expressions() {}
	_scope(map<string, _vardecl> v, vector<_ast*> e) : _ast(AST_SCOPE) {
		variables = v;
		expressions = e;
	}
} _scope;

typedef struct _arg : public _ast {
	string id;
	_type type;
	_ast* value;

	_arg() : _ast(AST_ARG), id(), type(), value() {}
	_arg(string i, _type t, _ast* v) : _ast(AST_ARG) {
		id = i;
		type = t;
		value = v;
	}
} _arg;

typedef struct _lambda : public _ast {
	vector<_arg> argument_list;
	vector<_arg> return_list;
	_scope body;

	_lambda() : _ast(AST_LAMBDA), argument_list(), return_list(), body() {}
	_lambda(vector<_arg> a, vector<_arg> r, _scope b) : _ast(AST_LAMBDA) {
		argument_list = a;
		return_list = r;
		body = b;
	}
} _lambda;

typedef struct _fndecl : public _ast {
	string id;
	_lambda fn;

	_fndecl() : _ast(AST_FNDECL), id(), fn() {}
	_fndecl(string i, _lambda& f) : _ast(AST_FNDECL) {
		id = i;
		fn = f;
	}
} _fndecl;

typedef struct _if : public _ast {
	_ast* cond;
	_ast* then;
	_ast* els;

	_if() : _ast(AST_IF), cond(nullptr), then(nullptr), els(nullptr) {}
	_if(_ast* c, _ast* t, _ast* e) : _ast(AST_IF) {
		cond = c;
		then = t;
		els = e;
	}
} _if;

typedef struct _while : public _ast {
	_ast* cond;
	_ast* body;
	_ast* els;

	_while() : _ast(AST_WHILE), cond(nullptr), body(nullptr), els(nullptr) {}
	_while(_ast* c, _ast* b, _ast* e) : _ast(AST_WHILE) {
		cond = c;
		body = b;
		els  = e;
	}
} _while;

typedef struct _binop : public _ast {
	_token op;
	_ast* lhs;
	_ast* rhs;

	_binop() : _ast(AST_BINOP), lhs(nullptr), rhs(nullptr) {}
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
	_unop(_token o, _ast* r) : _ast(AST_UNOP) {
		op = o;
		rhs = r;
	}
} _unop;

typedef struct _module : public _ast {
	string id;
	vector<_ast*> types;
	_scope body;

	void define_type(_ast* type) {
		types.push_back(type);
	}

	_ast* resolve_type(string id) {
		for (auto t : types) {
			switch (t->ast_type) {
			case AST_FNDECL:
				if (((_fndecl*)t)->id == id)
					return t;
				break;
			/* TODO:
				case AST_STRUCT:
				case AST_UNION:
			*/
			}
		}
		return nullptr;
	}

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