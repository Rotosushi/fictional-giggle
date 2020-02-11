#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <map>
using std::map;

#include "token.h"
#include "type.h"
#include "symbol_table.h"

// because primitive tagged unions are too much to ask for...
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
	AST_PROGRAM,
	AST_INT,
	AST_REAL,
	AST_CHAR, // TODO: add char data support to the syntax
	AST_TEXT,
	AST_BOOL,
};

// because reflection over the names of elements is
// too much to ask for.
string ast_type_to_string(_ast_type at);

typedef struct _ast {
	_ast_type ast_type;

	_ast() { ast_type = AST_ERR; }
	_ast(_ast_type a_t) { ast_type = a_t; }
	virtual ~_ast() {}

} _ast;

//typedef struct _entity {
//	string name;
//	_type type;
//	_ast* value;
//} _entity;

typedef unsigned int _flags;
/*
	note: flags are used in a bitwise fashion
	this means the enum's need to progress by
	the powers of two.
	hence:
	first_flag = 1,
	the_next_flag = 2,
	... = 4,
	... = 8,
	... = 16,
	...
*/
// helper function for flag extraction
bool nth_bit_set(unsigned int value, short nth) { 
	if (nth > 32) return false; // sanity check

	// shift the nth bit to the first position
	// then bitwise and with the value 1,
	// this sets every bit other than the
	// first position to 0, then we check
	// if the result is 1.
	return ((value >> nth) & 1) == 1; 
}
// helper function for flag setting
void set_nth_bit(unsigned int& value, short nth, bool direction) {
	unsigned int temp = 1;
	value = (temp << nth) & direction;
}

// expressions in the language are used in a variety of
// semantic locations, for the purpose of giving
// programmers the most flexibility in what they can express.
typedef struct _expr : public _ast {
	_type type;
	_ast* expr;

	_expr() : _ast(AST_EXPR), expr(nullptr) {}
} _expr;

typedef struct _var : public _ast {
	string id;
	_type type;

	_var() : _ast(AST_VAR), id(), type() {}
} _var;


enum class DECL_FLAGS : short  {
	CONST = 1,

};


typedef struct _vardecl : public _ast {
	_flags flags;
	_var lhs;
	_ast* init;

	_vardecl() : _ast(AST_VARDECL), lhs(), init() {}
	_vardecl(_flags f, _var l, _ast* i) : _ast(AST_VARDECL) {
		flags = f;
		lhs = l;
		init = i;
	}
} _vardecl;

bool is_const(const _vardecl& decl) { return nth_bit_set(decl.flags, (short)DECL_FLAGS::CONST); }


typedef struct _scope : public _ast {
	_symbol_table local_symbols;
	vector <_ast*> statements;

	_scope() : _ast(AST_SCOPE), local_symbols(), statements() {}
} _scope;

typedef struct _arg : public _ast {
	string id;
	_type type;

	bool operator==(const _arg& rhs) {
		return type.name == rhs.type.name;	
	}

	bool operator!=(const _arg& rhs) {
		return type.name != rhs.type.name;
	}

	_arg() : _ast(AST_ARG), id(), type() {}
	_arg(string s, _type t) : _ast(AST_ARG), id(s), type(t) {}
} _arg;

enum class FN_FLAGS : short {
	SEEN_RETURN = 1,
};

typedef struct _fn : public _ast {
	_flags flags;
	string id;
	vector<_arg> argument_list;
	_type return_type;
	_scope body;

	_fn() : _ast(AST_FN), id(), argument_list(), return_type(), body() {}
} _fn;

bool seen_return(const _fn& fn) { return nth_bit_set(fn.flags, (short)FN_FLAGS::SEEN_RETURN); }

typedef struct _if : public _ast {
	_ast* cond;
	_ast* then;
	_ast* els;

	_if() : _ast(AST_IF), cond(nullptr), then(nullptr), els(nullptr) {}
} _if;

typedef struct _while : public _ast {
	_ast* cond;
	_ast* body;

	_while() : _ast(AST_WHILE), cond(nullptr), body(nullptr) {}
} _while;

typedef struct _binop : public _ast {
	_type type;
	_token op;
	_ast* lhs;
	_ast* rhs;

	_binop() : _ast(AST_BINOP), type(), op(T_ERR), lhs(nullptr), rhs(nullptr) {}
	_binop(_token o, _ast* l, _ast* r) : _ast(AST_BINOP), type() {
		op = o;
		lhs = l;
		rhs = r;
	}
} _binop;

typedef struct _unop : public _ast {
	_type type;
	_token op;
	_ast* rhs;

	_unop() : _ast(AST_UNOP), op(), rhs(nullptr) {}
} _unop;

typedef struct _return : public _ast {
	_type type;
	_ast* expr;

	_return() : _ast(AST_RETURN), expr() {}
} _return;

typedef struct _fcall : public _ast {
	string id;
	vector<_arg> argument_list;
	_type return_type;

	_fcall() : _ast(AST_FCALL), argument_list() {}
} _fcall;

typedef map<string, vector<_fn>> function_table;

typedef struct _module : public _ast {
	string id;
	vector <string> import_list;
	vector <string> export_list;
	string root;
	function_table functions;
	_scope module_scope;

	_module() : _ast(AST_MODULE), id(), import_list(), export_list(), root(), functions(), module_scope(){}
} _module;

typedef struct _program : public _ast {
	vector <_module> modules;
	_scope global_scope;

	_program() : _ast(AST_PROGRAM), modules(), global_scope() {}
} _program;

typedef struct _int : public _ast {
	int value;

	_int() : _ast(AST_INT), value(0) {}
	_int(int i) : _ast(AST_INT), value(i) {}
} _int;

typedef struct _real : public _ast {
	float value;

	_real() : _ast(AST_REAL), value(0.0) {}
	_real(float f) : _ast(AST_REAL), value(f) {}
} _real;

typedef struct _char : public _ast {
	char value;

	_char() : _ast(AST_CHAR), value('\0') {}
	_char(char c) : _ast(AST_CHAR), value(c) {}
};

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