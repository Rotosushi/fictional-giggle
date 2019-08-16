#include <string>
#include <list>
#include <vector>
#include <stack>
#include <iostream>

#include "pink_parser.h"
#include "pink_lexer.h"
#include "pink_ast.h"

using std::string;
using std::list;
using std::vector;
using std::stack;
using std::cout;
using std::endl;

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
// TODO: <packrat parser> "stores correctly parsed symbols
//			so they done need to be reparsed when building
//			the AST."
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
bool match(Tok tok) {
	if (tok == tokbuf[tokidx].type) {
		consume();
		return true;
	}
	else return false;
}

Token get_last_match() {
	return tokbuf[((uint64_t)tokidx - 1)];
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

bool match_top_level() {
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
	
	sync(1); // prime our input

	while (tokbuf[tokidx].type  != T_EOF) {
		if (speculate_context_declaration())
			match_context_declaration();
		else if (speculate_type_definition())
			match_type_definition();
		else if (speculate_declaration())
			match_declaration();
		else {
			cout << "Error while parsing, unknown <top-level-declaration>: \n";
			print_token_buffer();
			return false;
		}
	}
	return true;
}
// TODO:
bool speculate_context_declaration()
{
	return false;
}
// TODO:
bool speculate_type_definition()
{
	return false;
}

bool speculate_declaration()
{
	// this function implements this portion of the grammar:
	// <declaration> := <identifier> <assignment-operator> <type-specifier> ';'
	// mark the current spot in the tokbuf so that
	// we can rewind later
	bool success = true;
	mark();

	// all variable declarations are led by an <identifier>
	if (match(T_ID)) {
		// TODO: store the ID parsed
		// followed by an <assignment-operator>
		if (match_assignment_operator()) {
			// followed by a <type-specifier>
			if (match_type_specifier()) {
				if (match(T_SEMICOLON)) {

				}
				else success = false;
			}
			else success = false;
		}
		else success = false;
	}
	else success = false;

	release(); // reset the tokidx succeed or fail, 
			   // we are reparsing either way
	return success;
}
// TODO:
bool match_context_declaration()
{
	return false;
}
// TODO:
bool match_type_definition()
{

	return false;
}

bool match_declaration()
{
	Token id, assign_op, rhs;
	bool success = true;
	// <declaration> := <identifier> <assignment-operator> <type-specifier> ';'	
	if (match(T_ID)) {
		id = get_last_match();
		if (match_assignment_operator()) {
			assign_op = get_last_match();
			if (match_type_specifier()) {
				rhs = get_last_match();
				if (match(T_SEMICOLON)) {

				}
				else success = false;
			}
			else success = false;
		}
		else success = false;
	}
	else success = false;

	// build the AST declaration now.
	declaration dec(id, assign_op, rhs);

	return success;
}

bool match_alias()
{
	return false;
}

bool match_struct()
{
	return false;
}

bool match_union()
{
	return false;
}

bool match_enum()
{
	return false;
}

bool match_function()
{
	return false;
}

bool match_assignment_operator() {
	// this function implements this part of the grammar:
	// <assignment-operator> := ':' (<compiler-directive>)* (':' | '=')?
	//						  | '=' (<compiler-directive>)*
	if (match(T_COLON)) {
		if (match_compiler_directive()) {
			// TODO: consume directive
			if (!speculating()) {
				
			}
			// try to find more directives
			while (match_compiler_directive()) { // consume these tokens
				if (!speculating()) {
					// preform AST building actions
				}
			}

			// this could still be a constant or dynamic binding
			if (match(T_EQUALS)) { // ':='

			}
			else if (match(T_COLON)) { // '::'

			}
			else { // is this needed?

			}
		}
		else { // just ':'
			if (!speculating()) {
				// preform AST building actions
			}
		}
		return true;
	}
	else if (match(T_EQUALS)) {
		while (match_compiler_directive()) { // consume these tokens
			if (!speculating()) {
				// preform AST building actions
			}
		}
		if (!speculating()) {
			// preform AST building actions
		}
		return true;
	}
	else if (match(T_DYNAMIC_ASSIGN)) {

		return true;
	}
	else if (match(T_CONST_ASSIGN)) {

		return true;
	}
	// TODO: +=, -=, ..., >>=
	else return false;
}

bool match_type_primitive()
{
	bool success = true;
	if		(match(T_MAYBE)) {
		if (!speculating()) {

		}
	}
	else if (match(T_NONE)) {
		if (!speculating()) {

		}
	}
	else if (match(T_INT)) {
		if (!speculating()) {

		}
	}
	else if (match(T_FLOAT)) {
		if (!speculating()) {

		}
	}
	else if (match(T_STRING)) {
		if (!speculating()) {

		}
	}
	else if (match(T_BOOL)) {
		if (!speculating()) {

		}
	}
	else if (match(T_S8)) {
		if (!speculating()) {

		}
	}
	else if (match(T_S16)) {
		if (!speculating()) {

		}
	}
	else if (match(T_S32)) {
		if (!speculating()) {

		}
	}
	else if (match(T_S64)) {
		if (!speculating()) {

		}
	}
	else if (match(T_U8)) {
		if (!speculating()) {

		}
	}
	else if (match(T_U16)) {
		if (!speculating()) {

		}
	}
	else if (match(T_U32)) {
		if (!speculating()) {

		}
	}
	else if (match(T_U64)) {
		if (!speculating()) {

		}
	}
	else if (match(T_F32)) {
		if (!speculating()) {

		}
	}
	else if (match(T_F64)) {
		if (!speculating()) {

		}
	}
	else if (match(T_LBRACE)) {
		while (match_constant_expression()) {
			if (!speculating()) {
				// do AST stuff
			}
		}
		if (match(T_RBRACE)) {
			if (!speculating()) {
				// do AST stuff
			}
		}
		else { // malformed array (missing ']')
			success = false;
		}
	}
	else success = false;

	return success;
}

bool match_literal()
{
/*
<literal>   := <numeric-literal>
			 | <string-literal>
			 | TODO: <enum-literal>

<numeric-literal>  := <numeric-literal-decimal>
					| <numeric-literal-hexidecimal>
					| <numeric-literal-octal>
					| <numeric-literal-binary>

<numeric-literal-decimal>	  := [0-9']*(.)?[0-9']+
<numeric-literal-hexidecimal> := ('0h' | '0H')[0-9a-fA-F']+
<numeric-literal-octal>		  := ('0o' | '0O')[0-7']+
<numeric-literal-binary>	  := ('0b' | '0B')[0-1']+
*/
	bool success = true;
	if (match(T_STRING_LITERAL)) {
		if (!speculating()) {
			// do AST stuff
		}
	}
	else if (match(T_INT_LITERAL)) {
		if (!speculating()) {
			// do AST stuff
		}
	}
	else if (match(T_FLOAT_LITERAL)) {
		if (!speculating()) {
			// do AST stuff
		}
	}
	else if (match(T_ID)) { // its an enum literal (unsupported rn)
		if (!speculating()) {

		}
	}
	else success = false;
	return success;
}

bool match_type_specifier()
{
	// this function implements this section of the grammar:
	// <type-specifier> := <identifier>
	//					 | <type-primitive>
	//					 | <lambda-definition>
	//					 | <literal>
	bool success = true;
	if (match(T_ID)) { // TODO:
		if (!speculating()) {
			// do AST stuff
		}
	}
	else if (match_type_primitive()) { // TODO:
		if (!speculating()) {
			// do AST stuff
		}
	}
	else if (match_lambda_definition()) { // TODO:
		if (!speculating()) {
			// do AST stuff
		}
	}
	else if (match_literal()) {
		if (!speculating()) {
			// do AST stuff
		} 
	}
	else success = false;
	return success;
}

bool match_identifier()
{
	return false;
}

bool match_lambda_definition()
{
	return false;
}

bool match_statement()
{
	return false;
}

bool match_compiler_directive()
{
	return false;
}

bool match_constant_expression()
{
	return false;
}

//
//struct variable {
//	variable(int type, string name) : type(type), name(name) {}
// 
//	int type;
//	string name;
//};
//
//struct var_int : public variable {
//	var_int(int type, string name, int value) : variable(type, name), value(value){}
//
//	int value;
//};
//
//struct var_float : public variable {
//	var_float(int type, string name, float value) : variable(type, name), value(value){}
//
//	float value;
//};
//
//struct var_string : public variable {
//	var_string(int type, string name, string value) : variable(type, name), value(value){}
//
//	string value;
//};
