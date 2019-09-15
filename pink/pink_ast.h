#pragma once

#include <string>
using std::string;
#include <vector>
using std::vector;
#include "pink_lexer.h"

enum _ast_type {
	AST_ERR,
	AST_DECLARATION,
	AST_STATEMENT,
	AST_FUNCTION,
	AST_SCOPE,
	AST_MODULE,
	AST_BINOP,
	AST_UNARYOP,
	AST_IF,
	AST_WHILE,
	AST_DOWHILE,
	AST_FOR,
	AST_STRUCT,
	AST_UNION,
	AST_ALIAS,
	AST_USERTYPE,
	AST_VAR,
	AST_ARRAY,
	AST_FUNCTION_CALL,
	AST_ITERATOR,
	AST_MEMBER,
	AST_ENUM,
	AST_INT,
	AST_FLOAT,
	AST_STRING,
	AST_BOOL,
};

// because forward declarations are not enough...
typedef struct _ast {
	_ast_type ast_type;
	//virtual void visit();
	// yay polymorphism!
	// TODO: debugging info
	//int linenum;
	//int charnum;
	//string filename;
	_ast() { ast_type = AST_ERR; }
	_ast(_ast_type type) { ast_type = type; }

	virtual ~_ast() {};
} _ast;

typedef struct _declaration : public _ast {
	string id; // while every ID is lexed as a Token, I don't want to type
	_token lhs; //  decl.id.value everytime we need the identifier
	_token op = { T_ERR, "" };
	_ast* rhs;
	_token type;
	vector<_token> directives;

	_declaration() : _ast(AST_DECLARATION) {}
	bool operator==(const _declaration& rhs)
	{
		return this->id == rhs.id;
	}

	virtual void visit() {}
} _declaration;

typedef struct _statement : public _ast {
	_ast* value;

	_statement() : _ast(AST_STATEMENT) {}
} _statement;


typedef struct _scope : public _ast {
	string id;
	vector<_declaration> cntxt;
	vector<_declaration> decls;
	vector<_statement> stmts;

	bool already_in_scope(_declaration& decl) {
		for (auto item : decls) {
			if (item == decl) return true;
		}
		return false;
	}

	bool already_in_context(_declaration& decl) {
		for (auto item : cntxt) {
			if (item == decl) return true;
		}
		return false;
	}

	_scope() : _ast(AST_SCOPE) {}
} _scope;

typedef struct _module : public _ast {
	string id;
	vector<_ast*> types;
	vector<string> imports;
	vector<string> exports;
	_scope body;

	_module() : _ast(AST_MODULE) {}
	virtual void visit() {}
} _module;

typedef struct _arg {
	string id;
	_token type;

	_ast* value = nullptr;
} _arg;

typedef struct _lambda : public _ast {
	vector<_arg> argument_list;
	vector<_arg> return_list;
	_scope body;

	_lambda() : _ast(AST_FUNCTION) {}
	virtual void visit() {}
} _lambda;

typedef struct _function : public _lambda {
	string id;

	_function() {}
} _function;

typedef struct _binop : public _ast {
	_token op;
	_ast* lhs = nullptr;
	_ast* rhs = nullptr;

	_binop() : _ast(AST_BINOP) {}
	_binop(_token o, _ast* l, _ast* r)
		: _ast(AST_BINOP), op(o), lhs(l), rhs(r) {}
	virtual void visit() {}
} _binop;

typedef struct _unop : public _ast {
	_token op;
	_ast* rhs = nullptr;

	_unop() : _ast(AST_UNARYOP) {}
	virtual void visit() {}
} _unop;

typedef struct _if : public _ast {
	_ast* cond = nullptr;
	_ast* then;
	_ast* els;

	_if() : _ast(AST_IF) {}
	virtual void visit() {}
} _if;

typedef struct _while : public _ast {
	_ast* cond;
	_ast* body;
	_ast* els;

	_while() : _ast(AST_WHILE) {}
	virtual void visit() {}
} _while;

typedef struct _dowhile : public _ast {
	_ast* cond = nullptr;
	_ast* body;
	_ast* els;

	_dowhile() : _ast(AST_DOWHILE) {}
	virtual void visit() {}
} _dowhile;

typedef struct _for : public _ast {
	_ast* init = nullptr;
	_ast* cond = nullptr;
	_ast* body;
	_ast* post = nullptr;
	_ast* els;

	_for() : _ast(AST_FOR) {}
	virtual void visit() {}
} _for;

typedef struct _usertype : public _ast {
	string type_name;
	_ast* value;

	_usertype() : _ast(AST_USERTYPE) {}
	virtual void visit() {}
} _usertype;

typedef struct _struct : public _ast {
	string id;
	vector<_declaration> body;

	_struct() : _ast(AST_STRUCT) {}
	virtual void visit() {}
} _struct;

typedef struct _union : public _ast {
	string id;
	vector<_declaration> body;

	_union() : _ast(AST_UNION) {}
	virtual void visit() {}
} _union;

typedef struct _enum : public _ast {
	string id;
	vector<_declaration> body;

	_enum() : _ast(AST_ENUM) {}
	virtual void visit() {}
} _enum;

typedef struct _array : public _ast {
	string id;
	_token type;
	int length;

	_array() : _ast(AST_ARRAY) {}
} _array;

typedef struct _iterator : public _ast {
	string id;
	_ast* value;

	_iterator() : _ast(AST_ITERATOR) {}
} _iterator;

typedef struct _member : public _ast {
	string id;

	_member() : _ast(AST_MEMBER) {}
} _member;


/* TODO: in pink.v2
typedef struct _pointer : public _ast {

} _pointer;
*/

typedef struct _alias : public _ast {
	string alias;
	_token type;

	_alias() : _ast(AST_ALIAS) {}
	virtual void visit() {}
} _alias;

typedef struct _var : public _ast {
	string value;
	vector<_ast*> postops;

	_var() : _ast(AST_VAR) {}
	_var(string s) : _ast(AST_VAR), value(s) {}
} _var;

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

typedef struct _string : public _ast {
	string value;

	_string() : _ast(AST_STRING) {}
	_string(string s) : _ast(AST_STRING), value(s) {}
} _string;

typedef struct _bool : public _ast {
	bool value;

	_bool() : _ast(AST_BOOL), value(false) {}
	_bool(bool b) : _ast(AST_BOOL), value(b) {}
} _bool;