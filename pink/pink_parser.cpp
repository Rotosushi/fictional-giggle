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
	AST_TYPECAST,
	AST_SIZEOF,
	AST_USERTYPE,
	AST_ENUM,
	AST_INT,
	AST_FLOAT,
	AST_STRING,
	AST_BOOL,
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
	Token op = {T_ERR, ""};
	_ast * rhs;
	Token type;
	vector<Token> directives;
	_declaration() : _ast(AST_DECLARATION) {}
} _declaration;

typedef struct _scope : public _ast {
	string id;
	vector<_ast*> cntxt;
	vector<string> symbls;
	vector<_declaration*> decls;
	vector<_ast*> stmts;
	_scope() : _ast(AST_SCOPE) {}
} _scope;

typedef struct _module : public _ast {
	string id;
	vector<_ast*> types;
	vector<string> imports;
	vector<string> exports;
	vector<_ast*> cntxt;
	vector<_declaration*> decls;
	vector<_ast*> stmts;
	_module() : _ast(AST_MODULE) {}
} _module;

typedef struct _arg {
	string id;
	Token type;
	_ast * value = nullptr;
} _arg;

typedef struct _lambda : public _ast {
	vector<_arg> argument_list;
	vector<_arg> return_list;
	_scope body;
	_lambda() : _ast(AST_FUNCTION) {}
} _lambda;

typedef struct _binop : public _ast {
	Token op;
	_ast * lhs = nullptr;
	_ast * rhs = nullptr;
	_binop() : _ast(AST_BINOP) {}
} _binop;

typedef struct _unaryop : public _ast {
	Token op;
	_ast * rhs = nullptr;
    _unaryop() : _ast(AST_UNARYOP) {}
} _unaryop;

typedef struct _typecast : public _ast {
	Token type;
	_ast* rhs;
	_typecast() : _ast(AST_TYPECAST) {}
} _typecast;

typedef struct _sizeof : public _ast {
	Token type;
	_ast* expr;
	_sizeof() : _ast(AST_SIZEOF) {}
} _sizeof;

typedef struct _if : public _ast {
	_ast * cond = nullptr;
	_scope then;
	_scope els;
	_if() : _ast(AST_IF) {}
} _if;

typedef struct _while : public _ast {
	_ast * cond;
	_scope body;
	_scope els;
	_while() : _ast(AST_WHILE) {}
} _while;

typedef struct _dowhile : public _ast {
	_ast * cond = nullptr;
	_scope body;
	_scope els;
	_dowhile() : _ast(AST_DOWHILE) {}
} _dowhile;

typedef struct _for : public _ast {
	_ast * init = nullptr;
	_ast * cond = nullptr;
	_scope body;
	_ast * post = nullptr;
	_scope els;
	_for() : _ast(AST_FOR) {}
} _for;

typedef struct _usertype : public _ast {
	string type_name;
	_ast* value;
	_usertype() : _ast(AST_USERTYPE) {}
} _usertype;

typedef struct _struct : public _ast {
	string id;
	vector<_declaration> body;
	_struct() : _ast(AST_STRUCT) {}
} _struct;

typedef struct _union : public _ast {
	string id;
	vector<_declaration> body;
	_union() : _ast(AST_UNION) {}
} _union;

typedef struct _enum : public _ast {
	string id;
	vector<_declaration> body;
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
bool speculate_lambda_header();
bool speculate_literal();
bool speculate_composite_type_block();

bool speculate_argument_list();
bool speculate_return_list();
bool speculate_arg();
bool speculate_type_specifier();
bool speculate_initializer();

bool speculate_statement(); //TODO:
bool speculate_expression();
bool speculate_conditional();
bool speculate_iteration();
bool speculate_block();

void build_alias(_alias& alias);
void build_struct(_struct& strct);
void build_union(_union& unn);
void build_function(_declaration& decl);
void build_declaration(_declaration& decl);

void build_type_specifier(_declaration& decl);
void build_type_specifier(_alias& alias);
void build_type_specifier(_arg& arg);
void build_type_specifier(_typecast& cast);
void build_type_specifier(_sizeof& expr);

void build_initializer(_declaration& decl);

void build_lambda(_lambda& fun);
void build_lambda_header(_lambda& fun);
void build_argument_list(vector<_arg>& args);
void build_return_list(vector<_arg>& args);
void build_block(_scope& scope);

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
// returns the current lookahead token
Token curtok() {
	return tokbuf[tokidx];
}

// compares passed token to the lookahead token
bool speculate(Tok tok) {
	if (tok == curtok().type) {
		consume();
		return true;
	} else return false;
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
	while (curtok().type != T_EOF) {
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
			if (speculate(T_CONST_ASSIGN)) {
				if (speculate_lambda()) {

				}
				else success = false;
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
	/*
<declaration>  := <identifier> ':' <type-specifier> ';'
				| <identifier> ':' <type-specifier> '=' <initializer> ';'
				| <identifier> '::' <initializer> ';'
				| <identifier> ':=' <initializer> ';'

	*/
	// mark the current spot in the tokbuf so that
	// we can rewind later
	bool success = true;
	mark();
	if (speculate(T_ID)) {
		if (speculate(T_COLON)) {
			if (speculate_type_specifier()) {
				if (speculate(T_SEMICOLON)) {

				}
				else if (speculate(T_EQUALS)) {
					if (speculate_initializer()) {
						if (speculate(T_SEMICOLON)) {

						}
						else success = false;
					}
					else success = false;
				}
				else success = false;
			}
			else success = false;
		}
		else if (speculate(T_CONST_ASSIGN)) {
			if (speculate_initializer()) {
				if (speculate(T_SEMICOLON)) {

				}
				else success = false;
			}
			else success = false;
		}
		else if (speculate(T_DYNAMIC_ASSIGN)) {
			if (speculate_initializer()) {
				if (speculate(T_SEMICOLON)) {

				}
				else success = false;
			}
			else success = false;
		}
		else success = false;
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
	/*<lambda-definition> := <lambda-header> <lambda-body>
	  <lambda-body> := <block>*/
	bool success = true;
	if (speculate_lambda_header()) {
		if (speculate_block()) {

		}
		else success = false;
	}
	else success = false;
	return success;
}

bool speculate_lambda_header()
{
	bool success = true;
	if (speculate_argument_list()) {
		if (speculate(T_ARROW)) {
			if (speculate_return_list()) {

			}
		}
		else success = false;
	}
	else success = false;
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
	return success;
}

bool speculate_arg()
{
	/*<arg> := <identifier> (':' <type-specifier>)?*/
	bool success = true;
	if (speculate(T_ID)) {
		if (speculate(T_COLON)) {
			if (speculate_type_specifier()) {

			}
			else success = false;
		}
	}
	else success = false;
	return success;
}


bool speculate_return_list()
{
	/*<return-list> := '(' <type-specifier> (',' <type-specifier>)*')' */
	bool success = true;
	if (speculate(T_LPAREN)) {
		if (speculate_type_specifier()) {

		}
		while (speculate(T_COMMA)) {
			if (speculate_type_specifier()) {

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
	return success;
}


bool speculate_type_specifier()
{
	// this function implements this section of the grammar:
	// <type-specifier> := <identifier>
	//					 | <type-primitive>
	//					 | <lambda-header>
	bool success = true;
	if (speculate(T_ID)) {

	}
	else if (speculate_type_primitive()) {

	}
	else if (speculate_lambda_header()) {

	}
	else success = false;
	return success;
}

bool speculate_initializer()
{
	bool success = true;
	if (speculate_lambda()) {

	}
	else if (speculate_literal()) {

	}
	else if (speculate(T_ID)) {

	}
	return success;
}

bool speculate_statement()
{
	bool success = true;
	mark();
	if (speculate_block()) {

	}
	else if (speculate_conditional()) {

	}
	else if (speculate_iteration()) {

	}
	else if (speculate_expression()) {

	}
	else success = false;
	release();
	return success;
}

bool speculate_expression()
{
	return false;
}

bool speculate_conditional()
{
	/*
<conditional>  := 'if' '(' <expression> ')' <statement> ('else' <statement>)?
		// TODO:| 'switch' '(' <expression> ')' <switch-block>

	*/
	bool success = true;
	mark();
	if (speculate(T_IF)) {
		if (speculate(T_LPAREN)) {
			if (speculate_expression()) {
				if (speculate(T_RPAREN)) {
					if (speculate_statement()) {
						if (speculate(T_ELSE)) {
							if (speculate_statement()) {

							}
							else success = false;
						}
					}
					else success = false;
				}
				else success = false;
			}
			else success = false;
		}
		else success = false;
	}
	else success = false;
	release();
	return success;
}

bool speculate_iteration()
{
	/*
	<iteration> := 'while' '(' <expression> ')' <statement>
			 | 'do' <statement> 'while' '(' <expression> ')'
	// TODO: | 'for' <identifier> 'in' <iterable> <statement>

	*/
	bool success = true;
	mark();
	if (speculate(T_WHILE)) {
		if (speculate(T_LPAREN)) {
			if (speculate_expression()) {
				if (speculate(T_RPAREN)) {
					if (speculate_statement()) {

					}
					else success = false;
				}
				else success = false;
			}
			else success = false;
		}
		else success = false;
	}
	else if (speculate(T_DO)) {
		if (speculate_statement()) {
			if (speculate(T_WHILE)) {
				if (speculate(T_LPAREN)) {
					if (speculate_expression()) {

					}
					else success = false;
				}
				else success = false;
			}
			else success = false;
		}
		else success = false;
	}
	else success = false;

	release();
	return success;
}

bool speculate_block()
{
	bool success = true;
	if (speculate(T_LBRACKET)) {
		while (speculate_declaration() || speculate_statement());

		if (speculate(T_RBRACKET)) {

		}
		else success = false;
	}
	else success = false;
	return success;
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
	/*
<declaration>  := <identifier> ':' <type-specifier> ';'
				| <identifier> ':' <type-specifier> '=' <initializer> ';'
				| <identifier> '::' <initializer> ';'
				| <identifier> ':=' <initializer> ';'

	*/
	// when this function gets called, we make a few assumptions about
	// the state of our program:
	// 1. tokbuf contains the valid syntactic form of a declaration.
	// 2. tokidx is set to the first symbol of this syntactic form
	// 3. each build_* function shall act like a consume() function,
	//		so as to leave the curtok() in a valid state for the caller
	// This function can throw, but shall only do so if either
	// of the assumptions are invalidated.

	/* <identifier> */
	if (curtok().type != T_ID) throw; // c-style assert
	decl.id = curtok().value;
	decl.lhs = curtok();
	consume(); // each time the parser querys the state of curtok()
			   // it shall consume that token.
	
	if (curtok().type == T_COLON) {
		build_type_specifier(decl); // build_* actions act like consume() 
									// for the state of the token buffer
		if (curtok().type == T_EQUALS) {
			build_initializer(decl);
		}
		if (decl.op.type == T_ERR) {
			decl.op = { T_COLON, "" }; // declaration w/out initialization.
		}
	}
	else if (curtok().type == T_CONST_ASSIGN
		|| curtok().type == T_DYNAMIC_ASSIGN) {
		build_initializer(decl);
	}
	else throw;

	if (curtok().type != T_SEMICOLON) throw; // all declarations end with a semicolon
	consume();
}

void build_type_specifier(_declaration& decl)
{
	// if this function is called, curtok() == ':'
	// so we consume it to look at the <type-specifier> token
	consume();
	/*
	<type-specifier>   := <identifier>
						| <type-primitive>
						| <lambda-header>
	*/
	switch (curtok().type) {
	case T_ID:
		decl.type = curtok();
		consume();
		break;
	case T_INT:
		decl.type = curtok();
		decl.rhs = new _int;
		consume();
		break;
	case T_FLOAT:
		decl.type = curtok();
		decl.rhs = new _float;
		consume();
		break;
	case T_STRING:
		decl.type = curtok();
		decl.rhs = new _string;
		consume();
		break;
	case T_BOOL:
		decl.type = curtok();
		decl.rhs = new _bool;
		consume();
		break;
	case T_LPAREN:
		auto l = new _lambda;	
		build_lambda_header(*l);		
		decl.rhs = l;			
		decl.type = { T_FUNCTION, "" };
		break;
	}
}

void build_type_specifier(_alias& alias)
{
}

void build_type_specifier(_arg& arg)
{
	// when this function is called the context is
	// <arg> := <identifier> (':' <type-specifier>)?
	// so curtok() is ':'
	consume();
	
	if (curtok().type == T_LPAREN) { // it's a lambda arg
		auto lambda = new _lambda;
		build_argument_list(lambda->argument_list); // build_* functions act like consume()
													// in their effect on the state of tokbuf

		if (curtok().type != T_ARROW) throw;
		consume();

		if (curtok().type != T_LPAREN) throw;
		build_return_list(lambda->return_list);

		arg.type.type = T_FUNCTION;
		arg.value = lambda;
	}
	else { // it's a single token (<identifier> || <type-primitive>
		arg.type = curtok();
		consume();
	}
}

void build_type_specifier(_typecast& cast)
{
}

void build_type_specifier(_sizeof& expr)
{
}

void build_initializer(_declaration& decl)
{
	/*
	This function is only ever called in this context:
<declaration>  := <identifier> ':' <type-specifier> ';'
			>	| <identifier> ':' <type-specifier> '=' <initializer> ';'
			>	| <identifier> '::' <initializer> ';'
			>	| <identifier> ':=' <initializer> ';'

<initializer>  := <lambda-definition>
				| <literal>
				| <identifier>
	*/
	// there will be a preceding '=' || '::' || ':='
	if (curtok().type == T_CONST_ASSIGN) {
		decl.op = curtok();
	}
	else {
		decl.op = { T_DYNAMIC_ASSIGN, ":=" };
	}
	consume(); 

	switch (curtok().type) {
	case T_ID:
		decl.rhs = new _usertype;
		decl.type = curtok();
		consume();
		break;
	case T_INT_LITERAL:
		decl.rhs = new _int(stoi(curtok().value));
		decl.type = curtok();
		consume();
		break;
	case T_FLOAT_LITERAL:
		decl.rhs = new _float(stof(curtok().value));
		decl.type = curtok();
		consume();
		break;
	case T_STRING_LITERAL:
		decl.rhs = new _string(curtok().value);
		decl.type = curtok();
		consume();
		break;
	case T_TRUE:
		decl.rhs = new _bool(true);
		decl.type = curtok();
		consume();
		break;
	case T_FALSE:
		decl.rhs = new _bool(false);
		decl.type = curtok();
		consume();
		break;
	case T_LPAREN:
		auto lambda = new _lambda;
		build_lambda(*lambda); // build_* functions act like consume() 
							   // on the state of the tokbuf.
		decl.rhs = lambda;
		decl.type = { T_FUNCTION, "" };
		break;
	}
}

void build_function(_declaration& decl)
{
	/*  'fn' <identifier> '::' <lambda-definition>	*/
	if (curtok().type != T_FUNCTION) throw;
	consume(); // 'fn'

	if (curtok().type != T_ID) throw;
	decl.lhs = curtok(); 
	decl.id = curtok().value;
	consume(); // <identifier>

	if (curtok().type != T_CONST_ASSIGN) throw;
	decl.op = curtok();
	consume(); // '::'

	auto lambda = new _lambda;
	build_lambda(*lambda);
	decl.rhs = lambda;
	decl.type.type = T_FUNCTION;
}

void build_lambda(_lambda& fun)
{
	/* When this function is called, it makes the same
		assumptions that other build_* functions make.
	// 1. tokbuf contains the valid syntactic form of a declaration.
	// 2. tokidx is set to the first symbol of this syntactic form
	//
	// This function can throw, but shall only do so if either
	// of the assumptions are invalidated.
		<lambda-definition> := <lambda-header> <lambda-body> 
		<lambda-header> := <argument-list> '->' (<return-list>)?
		<lambda-body> := <block>
		<block> := '{' (<declaration> | <statement>)* '}'
		*/
	
	build_lambda_header(fun);
	build_block(fun.body);

}

void build_lambda_header(_lambda& fun)
{
	/*
	<lambda-header> := <argument-list> '->' (<return-list>)?
	*/
	build_argument_list(fun.argument_list);

	if (curtok().type != T_ARROW) throw;
	consume();

	if (curtok().type == T_LPAREN) {
		build_return_list(fun.return_list);
	}
}

void build_argument_list(vector<_arg>& args)
{
	/*<arg> := <identifier> (':' <type-specifier>)?*/
	auto build_arg = [](_arg& a) {
		if (curtok().type != T_ID) throw;
		a.id = curtok().value;
		consume();
		
		if (curtok().type == T_COLON) { // ':'
			build_type_specifier(a);
		}
	};
	/*
	<argument-list> := '(' (<arg> (',' <arg>)*)? ')'
	*/
	if (curtok().type != T_LPAREN) throw;
	consume(); 

	if (curtok().type == T_ID) {
		_arg arg;
		build_arg(arg);
		args.push_back(arg);
		while (curtok().type == T_COMMA) {
			consume(); // ','
			build_arg(arg);
			args.push_back(arg);
		}
	}

	if (curtok().type != T_RPAREN) throw;
	consume();

}

void build_return_list(vector<_arg>& args)
{
	//<return-list> :='(' (<type-specifier> (',' <type-specifier>)*)? ')'
	if (curtok().type != T_LPAREN) throw;
	consume(); // '('

	_arg arg;

	if (curtok().type == T_LPAREN) { // it's a lambda
		auto lambda = new _lambda;
		build_argument_list(lambda->argument_list);

		if (curtok().type != T_ARROW) throw;
		consume();

		if (curtok().type != T_LPAREN) throw;
		build_return_list(lambda->return_list);

		arg.type.type = T_FUNCTION;
		arg.value = lambda;
		args.push_back(arg);
	}
	else { // the arg is a single token
		arg.type = curtok();
		args.push_back(arg);
		consume();
	}

	while (curtok().type == T_COMMA)
	{
		consume();
		if (curtok().type == T_LPAREN) { // it's a lambda
			auto lambda = new _lambda;
			build_argument_list(lambda->argument_list);

			if (curtok().type != T_ARROW) throw;
			consume();

			if (curtok().type != T_LPAREN) throw;
			build_return_list(lambda->return_list);

			arg.type.type = T_FUNCTION;
			arg.value = lambda;
			args.push_back(arg);
		}
		else { // the arg is a single token
			arg.type = curtok();
			args.push_back(arg);
			consume();
		}
	} 
	if (curtok().type != T_RPAREN) throw;
	consume();
}

void build_block(_scope& scope)
{
	/*<lambda-block> := '{' (<declaration> | <statement>)* '}'*/
	if (curtok().type != T_LBRACKET) throw;
	consume();

	_declaration* declaration;
	_ast* statement;

	while (1) {
		if (speculate_declaration()) {
			declaration = new _declaration;
			build_declaration(*declaration);
			scope.decls.push_back(declaration);
		}
		else if (speculate_statement()) {
			// TODO:
			//statement = new _statement;
			//build_statement(*statement);
			//scope.stmts.push_back(statement);
		}

		if (curtok().type == T_RBRACKET) break;
	}
	consume();
}


