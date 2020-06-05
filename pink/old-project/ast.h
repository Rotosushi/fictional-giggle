#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;

#include "token.h"

enum class AstType {
	ERR,
	STRINGLITERAL,
	MODULE,
	PRINT,
};

typedef struct Ast {
	AstType type;

	Ast() : type(AstType::ERR) {}
	Ast(AstType t) : type(t)   {}
} Ast;

typedef struct StringLiteral : public Ast {
	int which;
	string text;
	
	StringLiteral : Ast(AstType::STRINGLITERAL), which(), text() {}
} StringLiteral;

typedef struct Module : public Ast {
	string name;
	vector<Ast*> stmts;

	Module() : Ast(AstType::MODULE), name(), stmts() {}
} Module;

typedef struct Print : public Ast {
	Ast* arg;

	Print() : Ast(AstType::PRINT), arg(nullptr) {}
} Print;

//#include <map>
//using std::map;
//
//#include "token.h"
//#include "type.h"
//#include "flags.h"
//#include "symbol_table.h"
//
//// because primitive tagged unions are too much to ask for...
//enum _ast_type {
//	AST_ERR,
//	AST_VAR,
//	AST_VARDECL,
//	AST_ARG,
//	AST_FN,
//	AST_FCALL,
//	AST_IF,
//	AST_WHILE,
//	AST_EXPR,
//	AST_BINOP,
//	AST_UNOP,
//	AST_RETURN,
//	AST_SCOPE,
//	AST_MODULE,
//	AST_PROGRAM,
//	AST_INT,
//	AST_REAL,
//	AST_CHAR, // TODO: add char data support to the syntax
//	AST_TEXT,
//	AST_BOOL,
//};
//
//// because reflection over the names of elements is
//// too much to ask for.
//string ast_type_to_string(_ast_type at);
//
//typedef struct Ast {
//	_ast_type ast_type;
//
//	Ast() { ast_type = AST_ERR; }
//	Ast(_ast_type a_t) { ast_type = a_t; }
//	virtual ~Ast() {}
//
//} Ast;
//
////typedef struct _entity {
////	string name;
////	_type type;
////	_ast* value;
////} _entity;
//
//
//// expressions in the language are used in a variety of
//// semantic locations, for the purpose of giving
//// programmers the most flexibility in what they can express.
//typedef struct _expr : public Ast {
//	_type type;
//	Ast* expr;
//
//	_expr() : Ast(AST_EXPR), expr(nullptr) {}
//} _expr;
//
//typedef struct _var : public Ast {
//	string id;
//	_type type;
//
//	_var() : Ast(AST_VAR), id(), type() {}
//} _var;
//
//typedef struct _vardecl : public Ast {
//	_flags flags;
//	_var lhs;
//	Ast* init;
//
//	_vardecl() : Ast(AST_VARDECL), lhs(), init() {}
//	_vardecl(_flags f, _var l, Ast* i) : Ast(AST_VARDECL) {
//		flags = f;
//		lhs = l;
//		init = i;
//	}
//} _vardecl;
//
//enum class DECL_FLAGS : short {
//	CONST = 1,
//
//};
//
//bool is_const(const _vardecl& decl) { return is_nth_bit_set(decl.flags, (short)DECL_FLAGS::CONST); }
//
//
//typedef struct _scope : public Ast {
//	_symbol_table local_symbols;
//	vector <Ast*> statements;
//
//	_scope() : Ast(AST_SCOPE), local_symbols(), statements() {}
//} _scope;
//
//typedef struct _arg : public Ast {
//	string id;
//	_type type;
//
//	bool operator==(const _arg& rhs) {
//		return type.name == rhs.type.name;	
//	}
//
//	bool operator!=(const _arg& rhs) {
//		return type.name != rhs.type.name;
//	}
//
//	_arg() : Ast(AST_ARG), id(), type() {}
//	_arg(string s, _type t) : Ast(AST_ARG), id(s), type(t) {}
//} _arg;
//
//typedef struct _fn : public Ast {
//	_flags flags;
//	string id;
//	vector<_arg> argument_list;
//	_type return_type;
//	_scope body;
//
//	_fn() : Ast(AST_FN), id(), argument_list(), return_type(), body() {}
//} _fn;
//
//enum class FN_FLAGS : short {
//	SEEN_RETURN = 1,
//
//};
//
//bool seen_return(const _fn& fn) { return is_nth_bit_set(fn.flags, (short)FN_FLAGS::SEEN_RETURN); }
//string fn_to_string(string id, vector<_arg> args);
//
//typedef struct _if : public Ast {
//	Ast* cond;
//	Ast* then;
//	Ast* els;
//
//	_if() : Ast(AST_IF), cond(nullptr), then(nullptr), els(nullptr) {}
//} _if;
//
//typedef struct _while : public Ast {
//	Ast* cond;
//	Ast* body;
//
//	_while() : Ast(AST_WHILE), cond(nullptr), body(nullptr) {}
//} _while;
//
//typedef struct _binop : public Ast {
//	_type type;
//	_token op;
//	Ast* lhs;
//	Ast* rhs;
//
//	_binop() : Ast(AST_BINOP), type(), op(T_ERR), lhs(nullptr), rhs(nullptr) {}
//	_binop(_token o, Ast* l, Ast* r) : Ast(AST_BINOP), type() {
//		op = o;
//		lhs = l;
//		rhs = r;
//	}
//} _binop;
//
//typedef struct _unop : public Ast {
//	_type type;
//	_token op;
//	Ast* rhs;
//
//	_unop() : Ast(AST_UNOP), op(), rhs(nullptr) {}
//} _unop;
//
//typedef struct _return : public Ast {
//	_type type;
//	Ast* expr;
//
//	_return() : Ast(AST_RETURN), expr() {}
//} _return;
//
//typedef struct _fcall : public Ast {
//	string id;
//	vector<_arg> argument_list;
//	_type return_type;
//
//	_fcall() : Ast(AST_FCALL), argument_list() {}
//} _fcall;
//
//typedef map<string, vector<_fn>> function_table;
//
//typedef struct _module : public Ast {
//	string id;
//	vector <string> import_list;
//	vector <string> export_list;
//	string root_name;
//	_fn* root_fn;
//	function_table functions;
//	_scope module_scope;
//
//	_module() : Ast(AST_MODULE), id(), import_list(), export_list(), root_fn(), functions(), module_scope(){}
//} _module;
//
//typedef struct _program : public Ast {
//	vector <_module> modules;
//	_scope global_scope;
//
//	_program() : Ast(AST_PROGRAM), modules(), global_scope() {}
//} _program;
//
//typedef struct _int : public Ast {
//	int value;
//
//	_int() : Ast(AST_INT), value(0) {}
//	_int(int i) : Ast(AST_INT), value(i) {}
//} _int;
//
//typedef struct _real : public Ast {
//	float value;
//
//	_real() : Ast(AST_REAL), value(0.0) {}
//	_real(float f) : Ast(AST_REAL), value(f) {}
//} _real;
//
//typedef struct _char : public Ast {
//	char value;
//
//	_char() : Ast(AST_CHAR), value('\0') {}
//	_char(char c) : Ast(AST_CHAR), value(c) {}
//};
//
//typedef struct _text : public Ast {
//	string value;
//
//	_text() : Ast(AST_TEXT), value() {}
//	_text(string s) : Ast(AST_TEXT), value(s) {}
//} _text;
//
//typedef struct _bool : public Ast {
//	bool value;
//
//	_bool() : Ast(AST_BOOL), value(false) {}
//	_bool(bool b) : Ast(AST_BOOL), value(b) {}
//} _bool;
