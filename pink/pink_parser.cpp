#include <string>
#include <list>
#include <vector>
#include <stack>
#include <unordered_map>
#include <iostream>

#include "pink_parser.h"
#include "pink_lexer.h"

using std::string;
using std::list;
using std::vector;
using std::stack;
using std::unordered_map;
using std::cout;
using std::endl;

enum Ast_type {
	AST_DECLARATION,
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
	AST_ENUM,
	AST_INT,
	AST_FLOAT,
	AST_STRING,
	AST_BOOL,
};

enum Type {
	ERR,
	INFER,
	INT,
	FLOAT,
	STRING,
	BOOL,
};

// because forward declarations are not enough...
typedef struct _ast {
	Ast_type ast_type;
	// yay polymorphism!
	// TODO: debugging info
	//int linenum;
	//int charnum;
	//string filename;
	_ast(Ast_type type) { ast_type = type; }
	virtual ~_ast() {};
} _ast;

typedef struct _declaration : public _ast {
	string id; // while every ID is lexed as a Token, I don't want to type
	Token lhs; //  decl.id.value everytime we need the identifier
	Token op;
	_ast * rhs;
	Token type;
	vector<Token> directives;
	_declaration() : _ast(AST_DECLARATION) {}
} _declaration;

typedef struct _arg {
	string id;
	Type type;
} _arg;

typedef struct _lambda : public _ast {
	vector<Type> argument_list;
	vector<Type> return_list;
	_ast* body;
	_lambda() : _ast(AST_FUNCTION) {}
} _lambda;

typedef struct _scope : public _ast {
	string id;
	vector<_ast *> cntxt;
	vector<string> symbls;
	vector<_declaration *> decls;
	vector<_ast *> stmts;
	_scope() : _ast(AST_SCOPE) {}
} _scope;

typedef struct _module : public _ast {
	string id;
	vector<_ast*> types;
	vector<string> imports;
	vector<string> exports;
	vector<_ast*> cntxt;
	vector<_declaration *> decls;
	vector<_ast*> stmts;
	_module() : _ast(AST_MODULE) {}
} _module;

typedef struct _binop : public _ast {
	Token op;
	_ast * lhs;
	_ast * rhs;
	_binop() : _ast(AST_BINOP) {}
} _binop;

typedef struct _unaryop : public _ast {
	Token op;
	_ast * rhs;
    _unaryop() : _ast(AST_UNARYOP) {}
} _unaryop;

typedef struct _if : public _ast {
	_ast * cond;
	_ast * true_block;
	_ast * els;
	_if() : _ast(AST_IF) {}
} _if;

typedef struct _while : public _ast {
	_ast * cond;
	_ast * body;
	_ast * els;
	_while() : _ast(AST_WHILE) {}
} _while;

typedef struct _dowhile : public _ast {
	_ast * cond;
	_ast * body;
	_ast * els;
	_dowhile() : _ast(AST_DOWHILE) {}
} _dowhile;

typedef struct _for : public _ast {
	_ast * init;
	_ast * cond;
	_ast * body;
	_ast * post;
	_ast * els;
	_for() : _ast(AST_FOR) {}
} _for;

typedef struct _struct : public _ast {
	string id;
	vector<_ast *> body;
	_struct() : _ast(AST_STRUCT) {}
} _struct;

typedef struct _union : public _ast {
	string id;
	vector<_ast *> body;
	_union() : _ast(AST_UNION) {}
} _union;

typedef struct _enum : public _ast {
	string id;
	vector<_ast *> body;
	_enum() : _ast(AST_ENUM) {}
} _enum;

typedef struct _alias : public _ast {
	Tok alias;
	Tok type;
	_alias() : _ast(AST_ALIAS) {}
} _alias;

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

/* language support */
stack<_scope> scopes; // we push new scopes on the stack to declare modules, functions
					  //	conditionals and loops.

bool speculate_alias();
bool speculate_struct();
bool speculate_union();
bool speculate_function();
bool speculate_declaration();

bool speculate_type_primitive();
bool speculate_lambda();
bool speculate_literal();
bool speculate_composite_type_block();

bool speculate_argument_list();
bool speculate_return_list();
bool speculate_lambda_block();
bool speculate_arg();
bool speculate_assignment_operator();
bool speculate_type_specifier();
bool speculate_statement(); //TODO:

void build_alias(_alias& alias);
void build_struct(_struct& strct);
void build_union(_union& unn);
void build_function(_declaration& decl);
void build_declaration(_declaration& decl);

void build_lambda(_lambda& fun);
void build_argument_list(vector<_arg>& args);
void build_return_list(vector<_arg>& args);

/* backtracking support */
stack<int>	  marks;  // tokbuf indexes for nested backtracking
vector<Token> tokbuf; // buffer of tokens
int			  tokidx; // index of current token

// used by debugging
void print_token_buffer() {
	for (int i = 0; (size_t)i < tokbuf.size(); i++)
		cout << "token.type: "  << tokbuf[i].type  << '\n'
			 << "token.value: " << tokbuf[i].value << '\n'
			 << " index: " << i << '\n' << endl;
}

// token buffer access primitives:
// these manage the buffer such that the parser
// can speculate and backtrack

// the token buffer is a dynamically sized array (vector)
// when the parser speculates, it fills this array with
// tokens until the match succeeds or fails. on success the
// parser unwinds to the start of speculation and reparses the
// input, except this time it builds the AST of the tokens.
int mark() {
	marks.push(tokidx);
	return tokidx;
}

void release() {
	int mark = marks.top();
	marks.pop();
	seek(mark);
}

// why does this function exist?
void seek(int i) {
	tokidx = i;
}

// parsing primitives:
// compares passed token to the lookahead token

bool speculate(Tok tok) {
	if (tok == tokbuf[tokidx].type) {
		consume();
		return true;
	} else return false;
}

Token get_last_match() {
	return tokbuf[((uint64_t)tokidx - 1)];
}

/* memoization support */


// consumes a token in the buffer, replacing with a new token.
// if not backtracking; resets the token buffer
void consume() {
	tokidx++; // "consume" the token
			  // recall that this is a backtracking parser,
			  // so we cannot remove a token from the buffer
			  // in case we need to reparse it, so we instead
			  // move the pointer one position along the buffer,
			  // this means that the next function to try and
			  // "match" a token will look at the next token
			  // in sequence instead of the same token. Since
			  // all consumption is done through this function
			  // the program will be able to reparse the input.

	// if at end of buffer and not backtracking
	if ((size_t)tokidx == tokbuf.size() && !speculating()) {
		// this is an opportunity to reset the buffer
		// and start filling from 0 again.
		tokidx = 0;
		tokbuf.clear();
	}
	sync(1); // get a new token to replace the consumed token.
			 // either we have a semifull buffer and sync
			 // is a noop, or we have a full or empty buffer
			 // and we need to prime the next token.
}

// ensure buffer has i more tokens from the current tokidx
void sync(int i) {
	if ((size_t)((uint64_t)tokidx + i) > tokbuf.size()) { // do we need more tokens than we have?
		int n = (tokidx + i) - tokbuf.size(); // how many more do we need?
		for (int i = 0; i < n; i++) tokbuf.push_back(gettok()); // get n tokens
	}
}

// if we have a tokidx mark stored, then the parser is currently
// speculating a match, the .size() function returns the number
// of indicies currently pushed onto the stack.
bool speculating() {
	return (marks.size() > 0);
}

bool build_module() {
	// this function implements this segment of the grammar:
	//		"a program is composed of zero or more top level declarations
	//			followed by the 'EOF' token."
	//		<program> := (<top-level-declaration>)* EOF
	//
	//		"a top level declaration can be a context, type, or variable"
	//		<top-level-declaration>  := <context-declaration>
	//								  | <type-definition>
	//								  | <declaration>
	//
	//		"a context declaration looks like a block of declarations"
	//		<context-declaration> := 'context' '::' <context-block>
	//		"a type definition can be an alias, struct, union, enum, or a function"
	//		<type-definition>     := 'alias' <identifier> '::' <type-specifier> ';'
	//							   | ('struct' | 'union') (<identifier>)? '::' <composite-type-block>
	//							   | 'enum' (<identifier>) ? '::' <enumeration-block>
	//							   | 'fn' <identifier> '::' <lambda-definition>
	//
	//		"a declaration is a variable declaration, specified by type"
	//		<declaration>         := <identifier> <assignment-operator> <type-specifier> ';'
	
	// a module in pink is a file. a file is simply the context, and its collection
	// of declarations. so lets create a module level scope
	// TODO: top.id = <name-of-file-being-parsed>
	_module top;

	sync(1); // prime our input
	
	// these are all top level declarations
	while (tokbuf[tokidx].type != T_EOF) {
		/* <type-definition> */
		if (speculate_alias()) {
			auto a = new _alias;
			build_alias(*a);
			top.types.push_back(a);
		}
		else if (speculate_struct()) {
			auto s = new _struct;
			build_struct(*s);
			top.types.push_back(s);
		}
		else if (speculate_union()) {
			auto u = new _union;
			build_union(*u);
			top.types.push_back(u);
		}
		/* <declaration> */
		else if (speculate_function()) {
			auto f = new _declaration;
			build_function(*f);
			top.decls.push_back(f);
		}
		else if (speculate_declaration()) {
			auto d = new _declaration;
			build_declaration(*d);
			top.decls.push_back(d);
		}
		else {
			cout << "Error while parsing, unknown <top-level-declaration>: \n\t";
			print_token_buffer();
			return false;
		}
	}
	return true;
}


bool speculate_alias()
{
	return false;
}

bool speculate_struct() {
	/// <struct> := 'struct' (<identifier>)? '::' <composite-type-block>
	bool success = true;
	mark();
	if (speculate(T_STRUCT)) {
		if (speculate(T_ID)) {

		}

		if (speculate_composite_type_block()) {

		}
		else success = false;
	} else success = false;
	release();
	return success;
}

bool speculate_union()
{
	return false;
}

bool speculate_function()
{
	/* 'fn' <identifier> '::' <lambda-definition> */
	bool success = true;
	mark();
	if (speculate(T_FUNCTION)) {
		if (speculate(T_ID)) {
			if (speculate_lambda()) {

			}
			else success = false;
		}
		else success = false;
	}
	else success = false;
	release();
	return success;
}

bool speculate_declaration()
{
	// this function implements this portion of the grammar:
	// <declaration> := <identifier> <assignment-operator> <type-specifier> ';'
	// mark the current spot in the tokbuf so that
	// we can rewind later
	bool success = true;
	mark();
	if (speculate(T_ID)) {
		if (speculate_assignment_operator()) {
			if (speculate_type_specifier()) {
				if (speculate(T_SEMICOLON)) {
					
				} else success = false;
			} else success = false;
		} else success = false;
	} else success = false;
	release(); // reset the tokidx succeed or fail, 
			   // we are reparsing either way
	return success;
}

bool speculate_type_primitive()
{
	bool success = true;
	if (speculate(T_MAYBE));
	else if (speculate(T_NONE));
	else if (speculate(T_U8));
	else if (speculate(T_U16));
	else if (speculate(T_U32));
	else if (speculate(T_U64));
	else if (speculate(T_S8));
	else if (speculate(T_S16));
	else if (speculate(T_S32));
	else if (speculate(T_S64));
	else if (speculate(T_F32));
	else if (speculate(T_F64));
	else if (speculate(T_INT));
	else if (speculate(T_FLOAT));
	else if (speculate(T_STRING));
	else if (speculate(T_BOOL));
	else if (speculate(T_LBRACE)); //TODO:
	else if (speculate(T_MULT));   //TODO:
	else success = false;
	return success;
}

bool speculate_lambda()
{
	/*<lambda-definition> := <argument-list> (<return-list>)? <lambda-block>*/
	bool success = true;
	mark();
	if (speculate_argument_list()) {
		if (speculate_return_list()) {

		}
		if (speculate_lambda_block()) {

		}
		else success = false;
	}
	else success = false;
	release();
	return success;
}

bool speculate_literal()
{
	/*
	<numeric-literal>  := <numeric-literal-decimal>
					| <numeric-literal-hexidecimal>
					| <numeric-literal-octal>
					| <numeric-literal-binary>

	<numeric-literal-decimal>	  := [0-9']*(.)?[0-9']+
	<numeric-literal-hexidecimal> := ('h' | 'H')[0-9a-fA-F']+
	<numeric-literal-octal>		  := ('o' | 'O')[0-7']+
	<numeric-literal-binary>	  := ('b' | 'B')[0-1']+
	*/
	bool success = true;
	if (speculate(T_INT_LITERAL));
	else if (speculate(T_FLOAT_LITERAL));
	else if (speculate(T_STRING_LITERAL));
	else if (speculate(T_TRUE));
	else if (speculate(T_FALSE));
	else success = false;
	return success;
}

bool speculate_composite_type_block() {
	/* <composite-type-block> := '{' (<declaration>)* '}' */
	bool success = true;
	mark();
	if (speculate(T_LBRACKET)) { // '{'
		while (speculate_declaration()); // (<declaration>)*

		if (speculate(T_RBRACKET)) { // '}'

		}
		else success = false;
	}
	else success = false;
	release();
	return success;
}

bool speculate_argument_list()
{
	/*<argument-list> := '(' <arg> (',' <arg>)* ')'*/
	bool success = true;
	mark();
	if (speculate(T_LPAREN)) {
		if (speculate_arg()) {

		}
		while (speculate(T_COMMA)) {
			if (speculate_arg()) {

			}
			else {
				success = false;
				break;
			}
		}
		if (speculate(T_RPAREN)) {

		}
		else success = false;
	}
	else success = false;
	release();
	return success;
}

bool speculate_arg()
{
	/*<arg> := <identifier> (':' <type-specifier>)?*/
	bool success = true;
	mark();
	if (speculate(T_ID)) {
		if (speculate(T_COLON)) {
			if (speculate_type_specifier()) {

			}
			else success = false;
		}
	}
	else success = false;
	release();
	return success;
}

bool speculate_return_list()
{
	/*<return-list> := '->' <argument-list> */
	bool success = true;
	mark();
	if (speculate(T_ARROW)) {
		if (speculate(T_LPAREN)) {
			if (speculate_arg()) {

			}
			while (speculate(T_COMMA)) {
				if (speculate_arg()) {

				}
				else {
					success = false;
					break;
				}
			}
			if (speculate(T_RPAREN)) {

			}
			else success = false;
		}
		else success = false;
	}
	release();
	return success;
}

bool speculate_lambda_block()
{
	/*<lambda-block> := '{' (<declaration> | <statement>)* '}'*/
	bool success = true;
	mark();
	if (speculate(T_LBRACKET)) {
		while (speculate_declaration() || speculate_statement());

		if (speculate(T_RBRACKET)) {

		}
		else success = false;
	}
	else success = false;
	release();
	return success;
}


bool speculate_assignment_operator() {
	// this function implements this part of the grammar:
	// <assignment-operator> := ':' (<compiler-directive>)*
	//						  | ':=' (<compiler-directive>)*
	//						  | '::' (<compiler-directive>)*
	bool success = true;
	if (speculate(T_COLON)
		|| speculate(T_CONST_ASSIGN)
		|| speculate(T_DYNAMIC_ASSIGN)) 
	{ // ':' (<compiler-directive>)*
		while (speculate(T_COMPILER_DIRECTIVE));
	}
	else success = false;
	return success;
}

bool speculate_type_specifier()
{
	// this function implements this section of the grammar:
	// <type-specifier> := <identifier>
	//					 | <type-primitive>
	//					 | <lambda-definition>
	//					 | <literal>
	bool success = true;
	if (speculate(T_ID)) {

	}
	else if (speculate_type_primitive()) {

	}
	else if (speculate_lambda()) {

	}
	else if (speculate_literal()) {

	}
	else success = false;
	return success;
}

bool speculate_statement()
{
	return false;
}

void build_alias(_alias& alias)
{

}

void build_struct(_struct& strct)
{

}

void build_union(_union& unn)
{

}

void build_function(_declaration& decl)
{

}

bool is_type_primitive(Tok tok) {
	bool success = true;
	if (tok == T_MAYBE);
	else if (tok == T_NONE);
	else if (tok == T_U8);
	else if (tok == T_U16);
	else if (tok == T_U32);
	else if (tok == T_U64);
	else if (tok == T_S8);
	else if (tok == T_S16);
	else if (tok == T_S32);
	else if (tok == T_S64);
	else if (tok == T_F32);
	else if (tok == T_F64);
	else if (tok == T_INT);
	else if (tok == T_FLOAT);
	else if (tok == T_STRING);
	else if (tok == T_BOOL);
	// TODO: else if ( '[' (<constant-expression>)? ']' )
	// TODO: else if ( '*' <type-specifier> )
	else success = false;
	return success;
}

bool is_literal(Tok tok) {
	bool success = true;
	if (tok == T_INT_LITERAL);
	else if (tok == T_FLOAT_LITERAL);
	else if (tok == T_STRING_LITERAL);
	else if (tok == T_TRUE);
	else if (tok == T_FALSE);
	else success = false;
	return success;
}

void build_declaration(_declaration& decl)
{
	// this function implements this portion of the grammar:
	// <declaration> := <identifier> <assignment-operator> <type-specifier> ';'
	// when this function gets called, we make a few assumptions about
	// the state of our program:
	// 1. tokbuf contains the valid syntactic form of a declaration.
	// 2. tokidx is set to the first symbol of this syntactic form
	//
	// This function can throw, but shall only do so if either
	// of the assumptions are invalidated.

	/* <identifier> */
	if (tokbuf[tokidx].type != T_ID) throw; // c-style assert ;P
	decl.id = tokbuf[tokidx].value;
	decl.lhs = tokbuf[tokidx];
	consume();
	/* <assignment-operator> := (':' || '::' || ':=') (<compiler-directive>)* */
	decl.op = tokbuf[tokidx];
	consume();

	// this doesn't throw because having no compiler directives
	// is still a valid declaration.
	while (tokbuf[tokidx].type == T_COMPILER_DIRECTIVE) {
		decl.directives.push_back(tokbuf[tokidx]);
		consume();
	}
	
	/* <type-specifier> */
	if (tokbuf[tokidx].type == T_ID) { // it's a user defined type
		decl.type = tokbuf[tokidx];    
		consume();
	}
	else if (is_type_primitive(tokbuf[tokidx].type)) { // it's a primitive type
		decl.type = tokbuf[tokidx]; 
		switch (decl.type.type) {
		case T_INT: {
			decl.rhs = new _int;
		}
		case T_FLOAT: {
			decl.rhs = new _float;

		}
		case T_STRING: {
			decl.rhs = new _string;

		}
		case T_BOOL: {
			decl.rhs = new _bool;
		}
		}
		consume();
	}
	else if (tokbuf[tokidx].type == T_LPAREN) { // it's a lambda
		auto l = new _lambda;	// make a new lambda object
		build_lambda(*l);		// build it from the input
		decl.rhs = l;			// assign the result to the rhs
		decl.type = { T_FUNCTION, "" };
	}
	else if (is_literal(tokbuf[tokidx].type)) { // it's a literal
		decl.type = tokbuf[tokidx];
		consume();

		switch (decl.type.type) {
		case T_INT_LITERAL: {
			decl.rhs = new _int(std::stoi(decl.type.value));
			break;
		}
		case T_FLOAT_LITERAL: {
			decl.rhs = new _float(std::stof(decl.type.value));
			break;
		}
		case T_STRING_LITERAL: {
			decl.rhs = new _string(decl.type.value);
			break;
		}
		case T_TRUE: {
			decl.rhs = new _bool(true);
			break;
		}
		case T_FALSE: {
			decl.rhs = new _bool(false);
			break;
		}
		}
	}
	else throw; // it wasn't a valid declaration
	if (tokbuf[tokidx].type != T_SEMICOLON) throw; // all declarations end with a semicolon
	consume();
}

void build_lambda(_lambda& fun)
{

}

void build_argument_list(vector<_arg>& args)
{

}

void build_return_list(vector<_arg>& args)
{

}


