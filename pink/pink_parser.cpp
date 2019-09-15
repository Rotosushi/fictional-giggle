#include <string>
#include <list>
#include <vector>
#include <stack>
#include <unordered_map>
#include <iostream>

#include "pink_parser.h"
#include "pink_lexer.h"
#include "pink_ast.h"

using std::string;
using std::list;
using std::vector;
using std::stack;
using std::unordered_map;
using std::cout;
using std::endl;
using std::stoi;
using std::stof;

_module* _parser::parse_module()
{
	auto top = new _module;

	sync(1);

	/* <module> := (<top-level-declaration>)* <EOF> */
	while (curtok().type != T_EOF) {
		parse_top_level_declaration(*top);
	}

	return top;
}

_module* _parser::parse_module(char* filename)
{
	lex.set_infile(filename);
	return parse_module();
}

_module* _parser::parse_module(string s)
{
	lex.set_instring(s);
	return parse_module();
}

_parser::_parser()
{
	init_precedence_table();
	init_valid_list();
}

void _parser::init_precedence_table()
{
	precedence[T_EQUALS]				= 1;
	precedence[T_ADD_ASSIGN]			= 1;
	precedence[T_SUB_ASSIGN]			= 1;
	precedence[T_MULT_ASSIGN]			= 1;
	precedence[T_DIV_ASSIGN]			= 1;
	precedence[T_MOD_ASSIGN]			= 1;
	precedence[T_AND_ASSIGN]			= 1;
	precedence[T_XOR_ASSIGN]			= 1;
	precedence[T_OR_ASSIGN]				= 1;
	precedence[T_LSHIFT_ASSIGN]			= 1;
	precedence[T_RSHIFT_ASSIGN]			= 1;

	precedence[T_LOG_EQUALS]			= 2;
	precedence[T_LOG_NOT_EQUALS]		= 2;

	precedence[T_LOG_LESS]				= 3;
	precedence[T_LOG_GREATER]			= 3;
	precedence[T_LOG_LESS_EQUALS]		= 3;
	precedence[T_LOG_GREATER_EQUALS]	= 3;

	precedence[T_LOG_OR]				= 4;
	precedence[T_LOG_XOR]				= 5;
	precedence[T_LOG_AND]				= 6;

	precedence[T_BITWISE_OR]			= 7;
	precedence[T_BITWISE_XOR]			= 8;
	precedence[T_BITWISE_AND]			= 9;

	precedence[T_BITWISE_LSHIFT]		= 10;
	precedence[T_BITWISE_RSHIFT]		= 10;

	precedence[T_ADD]					= 11;
	precedence[T_SUB]					= 11;

	precedence[T_MULT]					= 12;
	precedence[T_DIV]					= 12;
	precedence[T_MOD]					= 12;
}

void _parser::init_valid_list()
{
	valid_list[AST_INT] = { AST_INT, AST_FLOAT, };
	valid_list[AST_FLOAT] = { AST_FLOAT, AST_INT, };
}

void _parser::print_token_buffer()
{
	for (auto i : tokbuf)
		cout << "token type: "  << i.type  << ' '
		     << "token value: " << i.value << endl;
}

void _parser::print_token_buffer(int i)
{
	for (int j = i; j < tokbuf.size(); j++)
		cout << "token type: "  << tokbuf[j].type  << ' '
		     << "token value: " << tokbuf[j].value << endl;
}

void _parser::parser_error(const char* str)
{
	cout << "parser error: " << str << "\n";
	print_token_buffer(tokidx);
}

int _parser::mark()
{
	marks.push(tokidx);
	return tokidx;
}

void _parser::release()
{
	int mark = marks.top();
	marks.pop();
	tokidx = mark; // seek to mark
}

_token _parser::curtok()
{
	return tokbuf[tokidx];
}

bool _parser::speculate(_tok tok)
{
	if (tok == curtok().type) {
		consume();
		return true;
	}
	else return false;
}

void _parser::consume()
{
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

void _parser::sync(int i)
{
	if ((size_t)((uint64_t)tokidx + i) > tokbuf.size()) { // do we need more tokens than we have?
		int n = (tokidx + i) - tokbuf.size(); // how many more do we need?
		for (int i = 0; i < n; i++) tokbuf.push_back(lex.gettok()); // get n tokens
	}
}

bool _parser::speculating()
{
	return marks.size() > 0;
}

bool _parser::is_unop(_tok t)
{
	// valid prefix tokens '-', '+', '*', '&', '!', '!!'
	if (t == T_ADD)			return true;
	if (t == T_SUB)			return true;
	if (t == T_BITWISE_AND) return true;
	if (t == T_MULT)		return true;
	if (t == T_BITWISE_NOT) return true;
	if (t == T_LOG_NOT)		return true;
	return false;
}

bool _parser::is_binop(_tok t)
{
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

bool _parser::is_literal(_tok t)
{
	if (t == T_INT_LITERAL)		return true;
	if (t == T_FLOAT_LITERAL)	return true;
	if (t == T_STRING_LITERAL)	return true;
	if (t == T_TRUE)			return true;
	if (t == T_FALSE)			return true;
	return false;
}

bool _parser::is_postop(_tok t)
{
	// postop :=  '(' <arg> (',' <arg>)* ')'
//			| '[' <expr> ']'
//			| '.' <id>
	if (t == T_LPAREN)	 return true;
	if (t == T_LBRACE)	 return true;
	if (t == T_PERIOD)	 return true;
	return false;
}

bool _parser::analyze(_declaration& decl, _scope& current_scope)
{
	/*
	about declarations
		what type is it?
		is this name taken?
	*/
	if (current_scope.already_in_scope(decl)) {
		parser_error("declaration uses previously defined identifier.");
		return false;
	}

	// this declaration is a lambda
	if (decl.rhs->ast_type == AST_FUNCTION) {
		auto rhs = (_lambda*)decl.rhs;
		for (auto dec : rhs->body.decls) {
			analyze(dec, rhs->body);
		}
	}
	// this declaration is the result of an expression
	else if (decl.rhs->ast_type == AST_BINOP) {
		auto rhs = (_binop*)decl.rhs;
		analyze(*rhs, current_scope);
	}

}

_ast_type _parser::analyze(_binop& binop, _scope& current_scope)
{
	_ast_type lhs_type, rhs_type;
	if (binop.lhs->ast_type == AST_BINOP) {
		auto lhs = (_binop*)binop.lhs;
		if ((lhs_type = analyze(*lhs, current_scope)) == AST_ERR)
			return AST_ERR;
	}
	else {
		lhs_type = binop.lhs->ast_type;
	}

	if (binop.rhs->ast_type == AST_BINOP) {
		auto rhs = (_binop*)binop.rhs;
		if ((rhs_type = analyze(*rhs, current_scope)) == AST_ERR)
			return AST_ERR;
	}
	else {
		rhs_type = binop.rhs->ast_type;
	}

	return valid_for(binop.op.type, lhs_type, rhs_type);
}

_ast_type _parser::valid_for(_tok tok, _ast_type lhs_type, _ast_type rhs_type)
{
	switch (tok) {
	case T_EQUALS:
		if (lhs_type == AST_VAR && rhs_type == AST_INT)
			return AST_INT;
		if (lhs_type == AST_VAR && rhs_type == AST_FLOAT)
			return AST_FLOAT;
		if (lhs_type == AST_VAR && rhs_type == AST_STRING)
			return AST_STRING;
		if (lhs_type == AST_VAR && rhs_type == AST_BOOL)
			return AST_BOOL;
	case T_ADD:

	case T_SUB:

	case T_MULT:

	case T_DIV:

	case T_MOD:

	case T_BITWISE_OR:

	case T_BITWISE_XOR:

	case T_BITWISE_AND:

	case T_LOG_OR:

	case T_LOG_XOR:

	case T_LOG_AND:

	case T_LOG_EQUALS:

	case T_LOG_NOT_EQUALS:

	case T_LOG_LESS:

	case T_LOG_LESS_EQUALS:

	case T_LOG_GREATER:

	case T_LOG_GREATER_EQUALS:
	}
}

bool _parser::try_speculate_declaration()
{
	bool success = true;
	mark();
	success = speculate_declaration();
	release();
	return success;
}

bool _parser::try_speculate_alias()
{
	bool success = true;
	mark();
	success = speculate_alias();
	release();
	return success;
}

bool _parser::try_speculate_struct()
{
	bool success = true;
	mark();
	success = speculate_struct();
	release();
	return success;
}

bool _parser::try_speculate_union()
{
	bool success = true;
	mark();
	success = speculate_union();
	release();
	return success;
}

bool _parser::try_speculate_function()
{
	bool success = true;
	mark();
	success = speculate_function();
	release();
	return success;
}

bool _parser::try_speculate_lambda()
{
	bool success = true;
	mark();
	success = speculate_lambda();
	release();
	return success;
}

bool _parser::try_speculate_conditional()
{
	bool success = true;
	mark();
	success = speculate_conditional();
	release();
	return success;
}

bool _parser::try_speculate_iteration()
{
	bool success = true;
	mark();
	success = speculate_iteration();
	release();
	return success;
}

bool _parser::try_speculate_expression()
{
	bool success = true;
	mark();
	success = speculate_expression();
	release();
	return success;
}

bool _parser::speculate_declaration()
{
	/*
		<declaration>  := <identifier> ':' <type-specifier> ';'
						| <identifier> ':' <type-specifier> '=' <initializer> ';'
						| <identifier> '::' <initializer> ';'
						| <identifier> ':=' <initializer> ';'

	*/
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
	}
	else success = false;
	return success;
}

bool _parser::speculate_type_specifier()
{
	/*
		<type-specifier>	:= <identifier>
							 | <type-primitive>
							 | <lambda-header>
	*/
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

bool _parser::speculate_type_primitive()
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

bool _parser::speculate_initializer()
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

bool _parser::speculate_alias()
{
	bool success = true;
	if (speculate(T_ALIAS)) {
		if (speculate(T_ID)) {
			if (speculate(T_CONST_ASSIGN)) {
				if (speculate_type_specifier()) {

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

bool _parser::speculate_struct()
{
	bool success = true;
	if (speculate(T_STRUCT)) {
		if (speculate(T_ID));
		if (speculate(T_CONST_ASSIGN)) {
			if (speculate_composite_type_block()) {

			}
			else success = false;
		}
		else success = false;

	}
	else success = false;
	return success;
}

bool _parser::speculate_union()
{
	bool success = true;
	if (speculate(T_UNION)) {
		if (speculate(T_ID));
		if (speculate(T_CONST_ASSIGN)) {
			if (speculate_composite_type_block()) {

			}
			else success = false;
		}
		else success = false;

	}
	else success = false;
	return success;
}

bool _parser::speculate_composite_type_block()
{
	bool success = true;
	if (speculate(T_LBRACKET)) {
		while (speculate_declaration());

		if (speculate(T_RBRACKET));
		else success = false;
	}
	else success = false;
	return success;
}

bool _parser::speculate_function()
{
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

bool _parser::speculate_lambda()
{
	/*
		<lambda-definition> := <lambda-header> <lambda-body>
		<lambda-body> := <block>
	*/
	bool success = true;
	if (speculate_lambda_header()) {
		if (speculate_block()) {

		}
		else success = false;
	}
	else success = false;
	return success;
}

bool _parser::speculate_lambda_header()
{
	/*
		<lambda-header> := <argument-list> '->' (<return-list>)?
	*/
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

bool _parser::speculate_argument_list()
{
	/*
		<argument-list> := '(' <arg> (',' <arg>)* ')'
	*/
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

bool _parser::speculate_arg()
{
	/*
		<arg> := <identifier> (':' <type-specifier>)?
	*/
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

bool _parser::speculate_return_list()
{
	/*
		<return-list> := '(' <type-specifier> (',' <type-specifier>)*')' 
	*/
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

bool _parser::speculate_statement()
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

bool _parser::speculate_conditional()
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

bool _parser::speculate_iteration()
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

bool _parser::speculate_block()
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

bool _parser::speculate_expression()
{
	// an expression is started by <id>, <literal>, <unop>, '('
	//  or immediately ended by ')', ';'
	if (curtok().type == T_ID)
		return speculate_id();
	if (is_literal(curtok().type))
		return speculate_literal();
	if (is_unop(curtok().type))
		return speculate_unop();
	if (curtok().type == T_LPAREN)
		return speculate_lparen();
	if (curtok().type == T_RPAREN) // empty expressions are valid
		return speculate_rparen();
	if (curtok().type == T_SEMICOLON && parens.size() == 0) // empty expressions are valid
		return true;
}

bool _parser::speculate_literal()
{
	// <literal> can be followed by <binop>, ')' or ';'
	consume();
	if (is_binop(curtok().type))
		return speculate_binop();
	if (curtok().type == T_RPAREN)
		return speculate_rparen();
	if (curtok().type == T_SEMICOLON && parens.size() == 0)
		return true;

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_id()
{
	// <id> can be followed by <binop>, <postop>, '(', ')', '[', ']', '.', or ';'
	consume(); // consume <id>
	if (is_binop(curtok().type))
		return speculate_binop();
	if (is_postop(curtok().type))
		return speculate_postop();
	if (curtok().type == T_RPAREN)
		return speculate_rparen();
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

bool _parser::speculate_binop()
{
	// <binop> can be followed by <unop>, <id>, '(', or <literal>
	consume(); // consume <binop>
	if (is_unop(curtok().type))
		return speculate_unop();
	if (is_literal(curtok().type))
		return speculate_literal();
	if (curtok().type == T_ID)
		return speculate_id();
	if (curtok().type == T_LPAREN)
		return speculate_lparen();

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_unop()
{
	// <unop> can be followed by <unop>, <id>, or <literal>
	consume(); // consume <unop>
	if (is_unop(curtok().type))
		return speculate_unop();
	if (is_literal(curtok().type))
		return speculate_literal();
	if (curtok().type == T_ID)
		return speculate_id();
	if (curtok().type == T_LPAREN)
		return speculate_lparen();

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_postop()
{
	// <postop> can be followed by <postop>, <binop>, or ';'
	// consume <postop>
	if (speculate_argument_list());
	else if (speculate(T_LBRACE)) {
		if (speculate_expression()) {

		}
		if (speculate(T_RBRACE));
		else return false;
	}
	else if (speculate(T_PERIOD)) {
		if (!speculate(T_ID)) {
			while (parens.size() > 0) parens.pop();
			return false;
		}
	}
	else { // malformed postop == malformed expression
		while (parens.size() > 0) parens.pop();
		return false;
	}

	if (is_postop(curtok().type))
		return speculate_postop();
	if (is_binop(curtok().type))
		return speculate_binop();
	if (curtok().type == T_RPAREN)
		return speculate_rparen();
	if (curtok().type == T_SEMICOLON && parens.size() == 0)
		return true;

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_lparen()
{
	// '(' can be followed by <expr>
	consume();
	parens.push(0); // push an open paren on the stack that needs closing
	return speculate_expression();
}

bool _parser::speculate_rparen()
{
	// ')' can be followed by ')', <binop>, ';' or be the terminal character
	consume();
	parens.pop();

	if (is_binop(curtok().type))
		return speculate_binop();
	if (curtok().type == T_SEMICOLON && parens.size() == 0)
		return true;
	if (curtok().type == T_RPAREN) {
		if (parens.size() == 0) return true;
		return speculate_rparen();
	}
	while (parens.size() > 0) parens.pop();
	return false;
}

void _parser::parse_top_level_declaration(_module& top)
{
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
	else if (try_speculate_function()) {
		auto f = new _function;
		parse_function(*f);
		// resolve(*f)
		// typecheck(*f)
		top.types.push_back(f);
	}
	else if (try_speculate_declaration()) {
		auto d = new _declaration;
		parse_declaration(*d);
		top.body.decls.push_back(*d);
	}
	else {
		parser_error("unknown top level declaration:");
	}
}

void _parser::parse_declaration(_declaration& decl)
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

void _parser::parse_type_specifier(_declaration& decl)
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

void _parser::parse_type_specifier(_arg& arg)
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

void _parser::parse_initializer(_declaration& decl)
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

void _parser::parse_expression(_statement& expr)
{
	expr.value = _parse_expression(parse_primary_expr(), 0);
}

void _parser::parse_expression(_ast* expr)
{
	expr = _parse_expression(parse_primary_expr(), 0);
}


// _parse_expression is an operator precedence parser
// https://en.wikipedia.org/wiki/Operator-precedence_parser
_ast* _parser::_parse_expression(_ast* lhs, int min_prec)
{
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

_ast* _parser::parse_postop()
{
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

_ast* _parser::parse_primary_expr()
{
	/*
	A primary expression is the base grapheme
		 that expressions are composed of. They have the
		 highest precedence and must be fully evaluated
		 before binops can be evaluated.

	   <primary-expression> := <id> (<postop>)*
							  | <literal>
							  | '(' <expression> ')'
							  | <unop> <primary-expression>
	*/
	_token i, f, s;
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

void _parser::parse_function_call(_lambda& fun)
{
	// '(' (<carg> (',' <carg>)*)? ')'
	// <carg> := <id> | <literal> | <lambda-definition>
	auto parse_carg = [this](_arg& arg) {
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

void _parser::parse_array_access(_iterator& iter)
{
	// '[' <expression> ']'
	consume(); // eat '['
	_statement expr;
	parse_expression(expr);
	iter.value = expr.value;

	if (curtok().type != T_RBRACE) throw;
	consume(); // eat ']'
}

void _parser::parse_member_access(_member& memb)
{
	// <id> '.' <id>
	consume(); // eat '.'
	if (curtok().type != T_ID) throw;
	memb.id = curtok().value;
	consume(); // eat <id>
}

void _parser::parse_alias(_alias& alias)
{
	if (curtok().type == T_ALIAS) {
		consume();
		if (curtok().type != T_ID) throw;
		alias.alias = curtok().value;
		consume();
		if (curtok().type != T_CONST_ASSIGN) throw;
		consume();
		alias.type = curtok();
		if (curtok().type != T_SEMICOLON) throw;
		consume();
	}
	else throw;
}

void _parser::parse_struct(_struct& strct)
{
	if (curtok().type == T_STRUCT) {
		consume();
		if (curtok().type == T_ID) {
			strct.id = curtok().value;
			consume();
		}
		if (curtok().type != T_CONST_ASSIGN) throw;
		consume();
		parse_composite_type_block(strct.body);
	}
	else throw;
}

void _parser::parse_union(_union& unn)
{
	if (curtok().type == T_UNION) {
		consume();
		if (curtok().type == T_ID) {
			unn.id = curtok().value;
			consume();
		}
		if (curtok().type != T_CONST_ASSIGN) throw;
		consume();
		parse_composite_type_block(unn.body);
	}
	else throw;

}

void _parser::parse_composite_type_block(vector<_declaration>& decls)
{
	if (curtok().type == T_RBRACKET) {
		consume();
		while (curtok().type != T_LBRACKET) {
			if (try_speculate_declaration()) {
				auto dec = new _declaration;
				parse_declaration(*dec);
				decls.push_back(*dec);
			}
		}
		consume();
	}
	else throw;
}

void _parser::parse_function(_function& fun)
{
	/*  'fn' <identifier> '::' <lambda-definition>	*/
	if (curtok().type != T_FUNCTION) throw;
	consume(); // 'fn'

	if (curtok().type != T_ID) throw;
	fun.id = curtok().value;
	consume(); // <identifier>

	if (curtok().type != T_CONST_ASSIGN) throw;
	consume(); // '::'

	parse_lambda(fun);
}

void _parser::parse_lambda(_lambda& fun)
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

void _parser::parse_lambda_header(_lambda& fun)
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

void _parser::parse_argument_list(vector<_arg>& args)
{
	/*<arg> := <identifier> (':' <type-specifier>)?*/
	auto build_arg = [this](_arg& a) {
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

void _parser::parse_return_list(vector<_arg>& args)
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

void _parser::parse_statement(_ast* stmt)
{
	if (curtok().type == T_LBRACKET) {
		auto block = new _scope;
		parse_block(*block);
		stmt = block;
	}
	else if (curtok().type == T_IF) {
		auto conditional = new _if;
		parse_conditional(*conditional);
		stmt = conditional;
	}
	else if (curtok().type == T_WHILE) {
		auto loop = new _while;
		parse_iteration(*loop);
		stmt = loop;
	}
	else if (curtok().type == T_DO) {
		auto loop = new _dowhile;
		parse_iteration(*loop);
		stmt = loop;
	}
	else if (try_speculate_expression()) {
		auto st = new _statement;
		parse_expression(st);
		stmt = st;
	}
	else throw;
}

void _parser::parse_iteration(_while& loop)
{
	/*
		<iteration> := 'while' '(' (<expr>)? ')' <statement>
					 | 'do' <statement> 'while' '(' (<expr>)? ')'
	*/
	if (curtok().type == T_WHILE) {
		consume();
		if (curtok().type != T_LPAREN) throw;
		consume();
		parse_expression(loop.cond);
		if (curtok().type != T_RPAREN) throw;
		consume();
		parse_statement(loop.body);
	}
	else throw;
}

void _parser::parse_iteration(_dowhile& loop)
{
	if (curtok().type == T_DO) {
		consume();
		parse_statement(loop.body);
		if (curtok().type != T_WHILE) throw;
		consume();
		if (curtok().type != T_LPAREN) throw;
		consume();
		parse_expression(loop.cond);
		if (curtok().type != T_RPAREN) throw;
		consume();
	}
	else throw;
}

void _parser::parse_conditional(_if& conditional)
{
	/*
	<conditional>  := 'if' '(' (<expr>)? ')' <statement> 
				| 'if' '(' (<expr>)? ')' <statement> 'else' <statement>
	*/
	if (curtok().type == T_IF) {
		consume();
		if (curtok().type != T_LPAREN) throw;
		consume();
		parse_expression(conditional.cond);
		if (curtok().type != T_RPAREN) throw;
		consume();
		parse_statement(conditional.then);
		if (curtok().type == T_ELSE) {
			consume();
			parse_statement(conditional.els);
		}
	}
	else throw;
}

void _parser::parse_block(_scope& scope)
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
			scope.decls.push_back(*declaration);
			delete declaration;
		}
		else if (speculate_statement()) {
			statement = new _statement;
			parse_expression(*statement);
			scope.stmts.push_back(*statement);
			delete statement;
		}

		if (curtok().type == T_RBRACKET) break;
	}
	consume(); // eat '}'
}

