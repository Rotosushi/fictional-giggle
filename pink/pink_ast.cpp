

#include <string>
#include <vector>

using std::string;
using std::vector;

#include "pink_lexer.h"
#include "pink_parser.h"
#include "pink_ast.h"

typedef struct Ast {
	// yay polymorphism!
	// TODO: debugging info
	//int linenum;
	//int charnum;
	//string filename;
} Ast;

typedef struct declaration : public Ast {
	string id; // while the ID is lexed as a Token, I don't want to type
	Token lhs; //  decl.id.value everytime we need the identifier
	Ast rhs;
	vector<Token> directives;
	declaration(Token id, Token op, Ast rhs, vector<Token> drctvs)
		: id(id.value), lhs(id), rhs(rhs), directives(drctvs) {}
} declaration;

typedef struct function : public Ast {
	string id;
	vector<Ast> argument_list;
	vector<Ast> return_list;
	Ast body;
	vector<Token> directives;
	function()
} function;

typedef struct binop : public Ast {
	Ast lhs;
	Ast rhs;
	binop(Token op, Ast lhs, Ast rhs) : Ast(op), lhs(lhs), rhs(rhs) {}
} binop;

typedef struct val_int : public Ast {
	int value;
	val_int(Token t, int i) : Ast(t), value(i) {}
} val_int;

typedef struct val_float : public Ast {
	float value;
	val_float(Token t, float f) : Ast(t), value(f) {}
} val_float;

typedef struct val_string : public Ast {
	string value;
	val_string(Token t, string s) : Ast(t), value(s) {}
} val_string;

