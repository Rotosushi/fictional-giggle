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
	Ast_type ast_type;
	//virtual void visit();
	// yay polymorphism!
	// TODO: debugging info
	//int linenum;
	//int charnum;
	//string filename;
	_ast() { ast_type = AST_ERR;  }
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
	virtual void visit() {}
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
	_scope body;

	_module() : _ast(AST_MODULE) {}
	virtual void visit() {}
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
	virtual void visit() {}
} _lambda;

typedef struct _binop : public _ast {
	Token op;
	_ast * lhs = nullptr;
	_ast * rhs = nullptr;

	_binop() : _ast(AST_BINOP) {}
	_binop(Token o, _ast* l, _ast* r)
		: _ast(AST_BINOP), op(o), lhs(l), rhs(r) {}
	virtual void visit() {}
} _binop;

typedef struct _unop : public _ast {
	Token op;
	_ast * rhs = nullptr;

	_unop() : _ast(AST_UNARYOP) {}
	virtual void visit() {}
} _unop;

typedef struct _if : public _ast {
	_ast * cond = nullptr;
	_scope then;
	_scope els;

	_if() : _ast(AST_IF) {}
	virtual void visit() {}
} _if;

typedef struct _while : public _ast {
	_ast * cond;
	_scope body;
	_scope els;

	_while() : _ast(AST_WHILE) {}
	virtual void visit() {}
} _while;

typedef struct _dowhile : public _ast {
	_ast * cond = nullptr;
	_scope body;
	_scope els;

	_dowhile() : _ast(AST_DOWHILE) {}
	virtual void visit() {}
} _dowhile;

typedef struct _for : public _ast {
	_ast * init = nullptr;
	_ast * cond = nullptr;
	_scope body;
	_ast * post = nullptr;
	_scope els;

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
	Token type;
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

typedef struct _statement : public _ast {
	_ast* value;

	_statement() : _ast(AST_STATEMENT) {}
} _statement;

/* TODO: in pink.v2
typedef struct _pointer : public _ast {
	
} _pointer;
*/

typedef struct _alias : public _ast {
	Tok alias;
	Tok type;

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

bool try_speculate_alias();
bool try_speculate_struct();
bool try_speculate_union();
bool try_speculate_function();
bool speculate_function();
bool try_speculate_declaration();
bool speculate_declaration();

bool speculate_type_primitive();
bool try_speculate_lambda();
bool speculate_lambda();
bool speculate_lambda_header();
bool speculate_literal();
bool speculate_composite_type_block();

bool speculate_argument_list();
bool speculate_return_list();
bool speculate_arg();
bool speculate_type_specifier();
bool speculate_initializer();

bool speculate_array_access();

bool speculate_statement();
bool try_speculate_conditional();
bool speculate_conditional();
bool try_speculate_iteration();
bool speculate_iteration();
bool speculate_block();

bool speculate_expression();

void parse_alias(_alias& alias);
void parse_struct(_struct& strct);
void parse_union(_union& unn);
void parse_function(_declaration& decl);
void parse_declaration(_declaration& decl);

void parse_type_specifier(_declaration& decl);
void parse_type_specifier(_alias& alias);
void parse_type_specifier(_arg& arg);

void parse_initializer(_declaration& decl);

void parse_lambda(_lambda& fun);
void parse_lambda_header(_lambda& fun);
void parse_argument_list(vector<_arg>& args);
void parse_return_list(vector<_arg>& args);
void parse_block(_scope& scope);

void parse_expression(_statement& expr);
_ast* _parse_expression(_ast* lhs, int min_prec);

void parse_function_call(_lambda& fun);
void parse_array_access(_iterator& iter);
void parse_member_access(_member& memb);

/* Precedence Table 
	1: '=', '*=', '/=', '%=', '+='
	'-=', '<<=', '>>=', '&&=', '^^=', '||='

	2: '?:'
	
	3: '==', '!!='

	4: '<', '>', '<=', '>='

	5: '|' 

	6: '^'

	7: '&'

	8: '!'
	
	9: '||'

	10: '^^'

	11: '&&'

	12: '!!'

	13: '<<', '>>', '<<=', '>>='

	14: '+', '-'

	15: '*', '/', '%'

	16: '[]', '()', '.'
*/
unordered_map<Tok, int> precedence;

void init_precedence_table() {
	precedence[T_EQUALS] = 1;
	precedence[T_ADD_ASSIGN] = 1;
	precedence[T_SUB_ASSIGN] = 1;
	precedence[T_MULT_ASSIGN] = 1;
	precedence[T_DIV_ASSIGN] = 1;
	precedence[T_MOD_ASSIGN] = 1;
	precedence[T_AND_ASSIGN] = 1;
	precedence[T_XOR_ASSIGN] = 1;
	precedence[T_OR_ASSIGN] = 1;
	precedence[T_LSHIFT_ASSIGN] = 1;
	precedence[T_RSHIFT_ASSIGN] = 1;

	precedence[T_QUESTION] = 2; // ?:

	precedence[T_LOG_EQUALS] = 3;
	precedence[T_LOG_NOT_EQUALS] = 3;

	precedence[T_LOG_LESS] = 4;
	precedence[T_LOG_GREATER] = 4;
	precedence[T_LOG_LESS_EQUALS] = 4;
	precedence[T_LOG_GREATER_EQUALS] = 4;

	precedence[T_LOG_OR] = 5;

	precedence[T_LOG_XOR] = 6;

	precedence[T_LOG_AND] = 7;

	precedence[T_LOG_NOT] = 8;

	precedence[T_BITWISE_OR] = 9;

	precedence[T_BITWISE_XOR] = 10;

	precedence[T_BITWISE_AND] = 11;

	precedence[T_BITWISE_NOT] = 12;

	precedence[T_BITWISE_LSHIFT] = 13;
	precedence[T_BITWISE_RSHIFT] = 13;

	precedence[T_ADD] = 14;
	precedence[T_SUB] = 14;

	precedence[T_MULT] = 15;
	precedence[T_DIV] = 15;
	precedence[T_MOD] = 15;

	precedence[T_PERIOD] = 16;
	precedence[T_LBRACKET] = 16;
	precedence[T_RBRACKET] = 16;
	precedence[T_LPAREN] = 16;
	precedence[T_RPAREN] = 16;
	precedence[T_COMMA] = 16;
}


/* support functions for the parser 

	observation: is_* functions are all testing
		for token membership within a -subset- of Tok

		if this pattern appears more, it may be useful to
		add some sort of 'slicing' or cleaner way of expressing
		subset or set membership with enums. 

		the other common way that enums are used is 
		 to make switch statements more readable 
		switch (enum) {
			case X:
			...
			case X+n:
		}
*/
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
	// TODO: else if ( '[' (<expression>)? ']' )
	// TODO: else if ( '*' <type-specifier> )
	else success = false;
	return success;
}

bool is_unop(Tok t) {
	// valid prefix tokens '-', '+', '*', '&', '!', '!!'
	if (t == T_ADD)			return true;
	if (t == T_SUB)			return true;
	if (t == T_BITWISE_AND) return true;
	if (t == T_MULT)		return true;
	if (t == T_BITWISE_NOT) return true;
	if (t == T_LOG_NOT)		return true;
	return false;
}

bool is_binop(Tok t) {
	/* valid binop tokens: =, +=, -=, *=,
							/=, %=, ||=, &&=,
							^^=, >>=, <<=, +,
							-, *, /, %,
							||, ^^, &&,
							|, ^, &, 
							!=, == 
	*/
	if (t == T_EQUALS)				return true;
	if (t == T_ADD_ASSIGN)			return true;
	if (t == T_SUB_ASSIGN)			return true;
	if (t == T_MULT_ASSIGN)			return true;
	if (t == T_DIV_ASSIGN)			return true;
	if (t == T_MOD_ASSIGN)			return true;
	if (t == T_OR_ASSIGN)			return true;
	if (t == T_XOR_ASSIGN)			return true;
	if (t == T_AND_ASSIGN)			return true;
	if (t == T_LSHIFT_ASSIGN)		return true;
	if (t == T_RSHIFT_ASSIGN)		return true;
	if (t == T_ADD)					return true;
	if (t == T_SUB)					return true;
	if (t == T_MULT)				return true;
	if (t == T_DIV)					return true;
	if (t == T_MOD)					return true;
	if (t == T_BITWISE_OR)			return true;
	if (t == T_BITWISE_XOR)			return true;
	if (t == T_BITWISE_AND)			return true;
	if (t == T_LOG_OR)				return true;
	if (t == T_LOG_XOR)				return true;
	if (t == T_LOG_AND)				return true;
	if (t == T_LOG_EQUALS)			return true;
	if (t == T_LOG_NOT_EQUALS)		return true;
	if (t == T_LOG_LESS)			return true;
	if (t == T_LOG_LESS_EQUALS)		return true;
	if (t == T_LOG_GREATER)			return true;
	if (t == T_LOG_GREATER_EQUALS)	return true;
	return false;
}

bool is_literal(Tok t) {
	if (t == T_INT_LITERAL)		return true;
	if (t == T_FLOAT_LITERAL)	return true;
	if (t == T_STRING_LITERAL)	return true;
	if (t == T_TRUE)			return true;
	if (t == T_FALSE)			return true;
	return false;
}

bool is_postop(Tok t) {
	// postop :=  '(' <arg> (',' <arg>)* ')'
	//			| '[' <expr> ']'
	//			| '.' <id>
	if (t == T_LPAREN)	 return true;
	if (t == T_LBRACE)	 return true;
	if (t == T_PERIOD)	 return true;
	return false;
}

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
	tokidx = mark; // seek to last mark
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

bool parse_module() {
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

	init_precedence_table();

	sync(1); // prime our input
	
	// these are all top level declarations
	while (curtok().type != T_EOF) {
		/* <type-definition> */
		if (try_speculate_alias()) {
			auto a = new _alias;
			parse_alias(*a);
			top.types.push_back(a);
		}
		else if (try_speculate_struct()) {
			auto s = new _struct;
			parse_struct(*s);
			top.types.push_back(s);
		}
		else if (try_speculate_union()) {
			auto u = new _union;
			parse_union(*u);
			top.types.push_back(u);
		}
		/* <declaration> */
		else if (try_speculate_function()) {
			auto f = new _declaration;
			parse_function(*f);
			top.body.decls.push_back(f);
		}
		else if (try_speculate_declaration()) {
			auto d = new _declaration;
			parse_declaration(*d);
			top.body.decls.push_back(d);
		}
		else {
			cout << "Error while parsing, unknown <top-level-declaration>: \n\t";
			print_token_buffer();
			return false;
		}
	}
	return true;
}

bool try_speculate_alias()
{
	return false;
}

bool try_speculate_struct() {
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

bool try_speculate_union()
{
	return false;
}

bool try_speculate_function()
{
	bool success = true;
	mark();
	success = speculate_function();
	release();
	return success;
}

bool speculate_function()
{
	/* 'fn' <identifier> '::' <lambda-definition> */
	bool success = true;
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
	return success;
}

bool try_speculate_declaration()
{
	bool success = true;
	mark();
	success = speculate_declaration();
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
	else if (speculate(T_LBRACE)) { // '[' (<expression>)? ']' <type-specifier>
		if (speculate_expression());
		if (speculate(T_RBRACE)) {
			if (speculate_type_specifier());
			else success = false;
		}
		else success = false;
	}
	else if (speculate(T_MULT)) { // '*' <type-specifier> (pointer)
		if (speculate_type_specifier());
		else success = false;
	}
	else success = false;
	return success;
}

bool try_speculate_lambda() {
	bool success = true;
	mark();
	success = speculate_lambda();
	release();
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
	/*<lambda-header> := <argument-list> '->' (<return-list>)?*/
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
	if (speculate(T_LBRACKET)) { // '{'
		while (speculate_declaration()); // (<declaration>)*

		if (speculate(T_RBRACKET)) { // '}'

		}
		else success = false;
	}
	else success = false;
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
	else if (speculate_argument_list()) {
		if (speculate(T_ARROW)) { 
			if (speculate_return_list());
			else success = false;
		}
		else success = false;
	}
	else success = false;
	return success;
}

bool speculate_initializer()
{
	bool success = true;
	if (try_speculate_lambda()) {
		speculate_lambda();
	}
	else if (speculate_expression()) {

	}
	else success = false;
	return success;
}

bool speculate_array_access()
{
	// '[' <expression> ']'
	bool success = true;
	if (speculate(T_LBRACE)) {
		if (speculate_expression()) {
			if (speculate(T_RBRACE)) {

			}
			else success = false;
		}
		else success = false;
	}
	else success = false;
	return success;
}

bool speculate_statement()
{
	bool success = true;
	if (speculate_block()) {

	}
	else if (speculate_conditional()) {

	}
	else if (speculate_iteration()) {

	}
	else if (speculate_expression()) {

	}
	else success = false;
	return success;
}

bool try_speculate_conditional()
{
	bool success = true;
	mark();
	success = speculate_conditional();
	release();
	return success;
}

bool speculate_conditional()
{
	/*
<conditional>  := 'if' '(' <expression> ')' <statement> ('else' <statement>)?
		// TODO:| 'switch' '(' <expression> ')' <switch-block>

	*/
	bool success = true;
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
	return success;
}

bool try_speculate_iteration()
{
	bool success = true;
	mark();
	success = speculate_iteration();
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

// these are functions that should only be called from
// _speculate_expression(), hence the leading '_'
bool _speculate_expression();
bool _speculate_id();
bool _speculate_unop();
bool _speculate_binop();
bool _speculate_literal();
bool _speculate_postop();
bool _speculate_lparen();
bool _speculate_rparen();

stack<int> parens;

bool speculate_expression()
{
	return _speculate_expression();
}

bool _speculate_expression() {
	// an expression is started by <id>, <literal>, <unop>, '('
	//  or immediately ended by ')', ';'
	if (curtok().type == T_ID)
		return _speculate_id();
	if (is_literal(curtok().type))
		return _speculate_literal();
	if (is_unop(curtok().type))
		return _speculate_unop();
	if (curtok().type == T_LPAREN)
		return _speculate_lparen();
	if (curtok().type == T_RPAREN) // empty expressions are valid
		return _speculate_rparen();
	if (curtok().type == T_SEMICOLON && parens.size() == 0) // empty expressions are valid
		return true;
	
	while (parens.size() > 0) parens.pop();
	return false;
}

bool _speculate_id() {
	// <id> can be followed by <binop>, <postop>, '(', ')', '[', ']', '.', or ';'
	consume(); // consume <id>
	if (is_binop(curtok().type))
		return _speculate_binop();
	if (is_postop(curtok().type))
		return _speculate_postop();
	if (curtok().type == T_RPAREN)
		return _speculate_rparen();
	// this may be questionable, but when <id> is followed by ']', ']' is assumed to be a terminal character 
	// for the case of array (pointer) math:
	//		A[B + C] = D;
	if (curtok().type == T_RBRACE) 
		return true;
	if (curtok().type == T_SEMICOLON && parens.size() == 0)
		return true;

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _speculate_binop() {
	// <binop> can be followed by <unop>, <id>, '(', or <literal>
	consume(); // consume <binop>
	if (is_unop(curtok().type))
		return _speculate_unop();
	if (is_literal(curtok().type))
		return _speculate_literal();
	if (curtok().type == T_ID)
		return _speculate_id();
	if (curtok().type == T_LPAREN)
		return _speculate_lparen();

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _speculate_unop() {
	// <unop> can be followed by <unop>, <id>, or <literal>
	consume(); // consume <unop>
	if (is_unop(curtok().type))
		return _speculate_unop();
	if (is_literal(curtok().type))
		return _speculate_literal();
	if (curtok().type == T_ID)
		return _speculate_id();
	if (curtok().type == T_LPAREN)
		return _speculate_lparen();

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _speculate_literal() {
	// <literal> can be followed by <binop>, ')' or ';'
	consume();
	if (is_binop(curtok().type))
		return _speculate_binop();
	if (curtok().type == T_RPAREN)
		return _speculate_rparen();
	if (curtok().type == T_SEMICOLON && parens.size() == 0)
		return true;

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _speculate_postop() {
	// <postop> can be followed by <postop>, <binop>, or ';'
	// consume <postop>
	if (speculate_argument_list());
	else if (speculate_array_access());
	else if (curtok().type == T_PERIOD) {
		consume();
		if (curtok().type != T_ID) {
			while (parens.size() > 0) parens.pop();
			return false;
		}
		consume();
	}
	else { // malformed postop == malformed expression
		while (parens.size() > 0) parens.pop();
		return false;
	}
	
	if (is_postop(curtok().type))
		return _speculate_postop();
	if (is_binop(curtok().type))
		return _speculate_binop();
	if (curtok().type == T_RPAREN)
		return _speculate_rparen();
	if (curtok().type == T_SEMICOLON && parens.size() == 0)
		return true;

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _speculate_lparen() {
	// '(' can be followed by <expr>
	consume();
	parens.push(0); // push an open paren on the stack that needs closing
	return _speculate_expression();
}

bool _speculate_rparen() {
	// ')' can be followed by ')', <binop>, ';' or be the terminal character
	consume();
	parens.pop();

	if (is_binop(curtok().type))
		return _speculate_binop();
	if (curtok().type == T_SEMICOLON && parens.size() == 0)
		return true;
	if (curtok().type == T_RPAREN) {
		if (parens.size() == 0) return true;
		return _speculate_rparen();
	}
	while (parens.size() > 0) parens.pop();
	return false;
}

/* parse_* functions */

void parse_alias(_alias& alias)
{

}

void parse_struct(_struct& strct)
{

}

void parse_union(_union& unn)
{

}

void parse_declaration(_declaration& decl)
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
		parse_type_specifier(decl); // build_* actions act like consume() 
									// for the state of the token buffer
		if (curtok().type == T_EQUALS) {
			parse_initializer(decl);
		}
		if (decl.op.type == T_ERR) {
			decl.op = { T_COLON, "" }; // declaration w/out initialization.
		}
	}
	else if (curtok().type == T_CONST_ASSIGN
		|| curtok().type == T_DYNAMIC_ASSIGN) {
		parse_initializer(decl);
	}
	else throw;

	if (curtok().type != T_SEMICOLON) throw; // all declarations end with a semicolon
	consume();
}

void parse_type_specifier(_declaration& decl)
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
	case T_ID: // <identifier>
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
	case T_LPAREN: // <lambda-header>
		auto l = new _lambda;	
		parse_lambda_header(*l);		
		decl.rhs = l;			
		decl.type = { T_FUNCTION, "" };
		break;
	}
}

void parse_type_specifier(_alias& alias)
{
}

void parse_type_specifier(_arg& arg)
{
	// when this function is called the context is
	// <arg> := <identifier> (':' <type-specifier>)?
	// so curtok() is ':'
	consume();
	
	if (curtok().type == T_LPAREN) { // it's a lambda arg
		auto lambda = new _lambda;
		parse_argument_list(lambda->argument_list); // build_* functions act like consume()
													// in their effect on the state of tokbuf

		if (curtok().type != T_ARROW) throw;
		consume();

		if (curtok().type != T_LPAREN) throw;
		parse_return_list(lambda->return_list);

		arg.type.type = T_FUNCTION;
		arg.value = lambda;
	}
	else { // it's a single token (<identifier> || <type-primitive>
		arg.type = curtok();
		consume();
	}
}

void parse_initializer(_declaration& decl)
{
	/*
	This function is only ever called in this context:
<declaration>  := <identifier> ':' <type-specifier> ';'
			>	| <identifier> ':' <type-specifier> '=' <initializer> ';'
			>	| <identifier> '::' <initializer> ';'
			>	| <identifier> ':=' <initializer> ';'

<initializer>  := <lambda-definition>
				| <expression> 

	*/
	_statement statement;
	_lambda* lambda;
	// there will be a preceding '=' || '::' || ':='
	if (curtok().type == T_CONST_ASSIGN) {
		decl.op = curtok();
	}
	else { 
		decl.op = { T_DYNAMIC_ASSIGN, ":=" };
	}
	consume();

	// speculate_expression is normally called in a context where
	// we want the input to advance if it is successfull, such as
	// <conditional> or <iteration>, in the context of this function
	// we want to unwind the input after speculation, hence this wrapper
	// function.
	auto try_speculate_expression = []() {
		bool success = true;
		mark();
		success = speculate_expression();
		release();
		return success;
	};

	/*
		In theory parse_expression handles the cases of a single <id>, <literal>, or <fcall>
		so the only distinction that needs to be made is between defining a new function
		using lambda syntax and the rhs being an expression.
	*/
	if (try_speculate_expression()) { 
		parse_expression(statement);
		decl.rhs = statement.value;
	}
	else if (try_speculate_lambda()) {
		lambda = new _lambda;
		parse_lambda(*lambda);
		decl.rhs = lambda;
	}
	else throw;
	
}

void parse_function(_declaration& decl)
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
	parse_lambda(*lambda);
	decl.rhs = lambda;
	decl.type.type = T_FUNCTION;
}

void parse_lambda(_lambda& fun)
{
	/* When this function is called, it makes the same
		assumptions that other parse_* functions make.
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
	
	parse_lambda_header(fun);
	parse_block(fun.body);

}

void parse_lambda_header(_lambda& fun)
{
	/*
	<lambda-header> := <argument-list> '->' (<return-list>)?
	*/
	parse_argument_list(fun.argument_list);

	if (curtok().type != T_ARROW) throw;
	consume();

	if (curtok().type == T_LPAREN) {
		parse_return_list(fun.return_list);
	}
}

void parse_argument_list(vector<_arg>& args)
{
	/*<arg> := <identifier> (':' <type-specifier>)?*/
	auto build_arg = [](_arg& a) {
		if (curtok().type != T_ID) throw;
		a.id = curtok().value;
		consume();
		
		if (curtok().type == T_COLON) { // ':'
			parse_type_specifier(a);
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

void parse_return_list(vector<_arg>& args)
{
	//<return-list> :='(' (<type-specifier> (',' <type-specifier>)*)? ')'
	if (curtok().type != T_LPAREN) throw;
	consume(); // '('

	_arg arg;

	if (curtok().type == T_LPAREN) { // it's a lambda
		auto lambda = new _lambda;
		parse_argument_list(lambda->argument_list);

		if (curtok().type != T_ARROW) throw;
		consume();

		if (curtok().type != T_LPAREN) throw;
		parse_return_list(lambda->return_list);

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
			parse_argument_list(lambda->argument_list);

			if (curtok().type != T_ARROW) throw;
			consume();

			if (curtok().type != T_LPAREN) throw;
			parse_return_list(lambda->return_list);

			arg.type.type = T_FUNCTION;
			arg.value = lambda;
			args.push_back(arg);
		}
		else { // the arg is a single token, <id>, <primitive>, or <user-type>
			arg.type = curtok();
			args.push_back(arg);
			consume();
		}
	} 
	if (curtok().type != T_RPAREN) throw;
	consume();
}

void parse_block(_scope& scope)
{
	/*<lambda-block> := '{' (<declaration> | <statement>)* '}'*/
	if (curtok().type != T_LBRACKET) throw;
	consume(); // eat '{'

	_declaration* declaration;
	_statement* statement;

	while (1) {
		if (speculate_declaration()) {
			declaration = new _declaration;
			parse_declaration(*declaration);
			scope.decls.push_back(declaration);
		}
		else if (speculate_statement()) {
			statement = new _statement;
			parse_expression(*statement);
			scope.stmts.push_back(statement);
		}

		if (curtok().type == T_RBRACKET) break;
	}
	consume(); // eat '}'
}

void parse_function_call(_lambda& fun)
{
	// '(' (<carg> (',' <carg>)*)? ')'
	// <carg> := <id> | <literal> | <lambda>
	auto parse_carg = [](_arg& arg) {
		if (curtok().type == T_ID) {
			arg.id = curtok().value;
			arg.type = curtok();
			consume();
		}
		else if (is_literal(curtok().type)) {
			arg.type = curtok();
			consume();
		}
		else {
			auto l = new _lambda;
			parse_lambda(*l);
		}
	};
	
	consume(); // eat '('
	if (curtok().type != T_RPAREN) { // empty arg list is semantically valid
		_arg arg;
		parse_carg(arg);
		fun.argument_list.push_back(arg);
		while (curtok().type == T_COMMA) {
			consume();
			parse_carg(arg);
			fun.argument_list.push_back(arg);
		}
	}
	
	if (curtok().type != T_RPAREN) throw;
	consume(); // eat ')'
}

void parse_array_access(_iterator& iter)
{
	// '[' <expression> ']'
	consume(); // eat '['
	_statement expr;
	parse_expression(expr);
	iter.value = expr.value;

	if (curtok().type != T_RBRACE) throw;
	consume(); // eat ']'
}

void parse_member_access(_member& memb)
{
	// <id> '.' <id>
	consume(); // eat '.'
	if (curtok().type != T_ID) throw;
	memb.id = curtok().value;
	consume(); // eat <id>
}

_ast* parse_postop() {
	_lambda* fun;
	_iterator* iter;
	_member* memb;
	switch (curtok().type) {
	case T_LPAREN:
		fun = new _lambda;
		parse_function_call(*fun);
		return fun;
	case T_LBRACE:
		iter = new _iterator;
		parse_array_access(*iter);
		return iter;
	case T_PERIOD:
		memb = new _member;
		parse_member_access(*memb);
		return memb;
	default:
		throw;
	}
}

_ast* parse_primary_expr() {
	// A primary expression is the base grapheme
	// that expressions are composed of. They have the
	// highest precedence and must be fully evaluated
	// before binops can be evaluated.
	// primary expressions: <id> (<postop>)*, <literal>, '(' <expression> ')', <unop> <primary>
	Token i, f, s;
	_var* var;
	_ast* expr;
	_unop* unop;
	switch (curtok().type) {
	case T_ID: 
		var = new _var(curtok().value);
		consume(); // eat <id>
		while (is_postop(curtok().type))
			var->postops.push_back(parse_postop());
		return var;
	case T_LPAREN:
		consume(); // eat '('

		expr = _parse_expression(parse_primary_expr(), 0);

		if (curtok().type != T_RPAREN) throw;
		consume(); // eat ')'
		return expr;
	case T_ADD: case T_SUB: case T_MULT: 
	case T_LOG_AND: case T_LOG_NOT: case T_BITWISE_NOT:
		unop = new _unop;

		unop->op = curtok();
		consume();
		unop->rhs = parse_primary_expr();
		
		return unop;
	case T_INT_LITERAL:
		i = curtok();
		consume();
		return new _int(stoi(i.value));
	case T_FLOAT_LITERAL:
		f = curtok();
		consume();
		return new _float(stof(f.value));
	case T_STRING_LITERAL:
		s = curtok();
		consume();
		return new _string(s.value);
	case T_TRUE:
		consume();
		return new _bool(true);
	case T_FALSE:
		consume();
		return new _bool(false);
	default: throw;
	}
}

_ast* _parse_expression(_ast* lhs, int min_prec) 
{
	// lhs = <primary>
	auto lad = curtok(); // lad - lookahead
	while (is_binop(lad.type) && precedence[lad.type] >= min_prec) {
		auto op = lad;
		consume(); 
		auto rhs = parse_primary_expr();
		lad = curtok();
		while (is_binop(lad.type) && (precedence[lad.type] > precedence[op.type]))
		{
			rhs = _parse_expression(rhs, precedence[lad.type]);
			lad = curtok();
		}
		lhs = new _binop(op, lhs, rhs);
	}
	return lhs;
}

/*  parse_expression
	https://en.wikipedia.org/wiki/Operator-precedence_parser


	Precedence Table:
	1: '=', '*=', '/=', '%=', '+='
		'-=', '<<=', '>>=', '&&=', '^^=', '||='
	2: '?:' // maybe remove ternary as it is a shorthand if-then-else instead of its own semantic construct?
	3: '==', '!='
	4: '<', '>', '<=', '>='
	5: '|'
	6: '^'
	7: '&'
	8: '||'
	9: '^^'
	10: '&&'
	11: '<<', '>>'
	12: '+', '-'
	13: '*', '/', '%'
	14: '[]', '()', '.', '!', '!!'
*/
void parse_expression(_statement& expr) {
	// parse_primary_expr() will advance curtok to the next token
	expr.value = _parse_expression(parse_primary_expr(), 0);
}

