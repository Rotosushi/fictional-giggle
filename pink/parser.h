#pragma once

#include <stack>
using std::stack;
#include <string>
using std::string;
#include <map>
using std::map;
#include <iostream>
using std::ifstream;

#include "token.h"
#include "type.h"
#include "ast.h"
#include "lexer.h"


class _parser {
public:
	_module* parse_module();
	_module* parse_module(string input);
	_module* parse_module(ifstream& input);

	_parser() {
		init_precedence_table();
		reset_internal_state();
	}
private:
	_lexer lexer;
	map<_token, int> ptable;
	
	stack<int> parens;

	vector<_token> tokbuf;
	vector<string> texbuf;
	int tokidx;

	stack<int> marks;

	/* support functions */
	/* general */
	void init_precedence_table();
	void reset_internal_state();

	/* token handling */
	_token curtok();
	void nexttok();
	string curtext();

	/* speculation support */
	int mark();
	void release();

	bool speculate(_token t);
	bool speculating();
	void sync(int i);
	
	/* set membership functions */

	bool is_unop(_token t);
	bool is_binop(_token t);
	bool is_literal(_token t);
	bool is_postop(_token t);
	bool is_module_keyword(_token t);

	/* parsing functions */
	/* Module parsing 
		
	*/
	void parse_module_declaration(_module& mdl);
	void parse_context_statement(_module& mdl);

	/*
		variable parsing,
		by prefixing variable declarations with a keyword
		speculation may be entirely factored out of the compiler.
	*/

	void parse_variable_declaration(_vardecl& decl);
	void parse_initializer(_vardecl& decl);
	void parse_type(_type& t);

	void parse_function_declaration(_fn& fn);
	void parse_function_type(_fn& fn);
	void parse_function_body(_fn& fn);
	void parse_argument_list(vector<_arg>& args);
	void parse_arg(_arg& arg);
	void parse_return_type(_type & var);
	
	/* statement parsing 
		statements are implemented
		with a basic multiplexer

		<statement> := <if>
					 | <while>
					 | <scope>
					 | <expression>

		by putting expressions last, speculation can
		be completely factored out of this segment 
		of the grammar.
	*/
	_ast* parse_statement();
	void parse_if(_if& conditional);
	void parse_while(_while& loop);
	void parse_scope(_scope& body);
	void parse_return(_return& ret);
	void parse_expression(_expr& expr);
	/* expression parsing
		expressions are their own sub-grammar within the
		parser. From my understanding an 'operator precedence
		parser' that parse_expression() implements, is a
		predicated parser. This is because of it's parsing
		descisions, which ultimately affect the shape of
		the resulting parse tree, are affected by the
		'precedence' of each operator; a piece of information
		that is not a part of the input to the parser. 
		(lexically speaking)
	
	*/
	
	_ast* parse_expression();
	_ast* _parse_expression(_ast* lhs, int min_prec);
	_ast* _parse_postop();
	_ast* _parse_primary_expr();

	_fcall* _parse_function_call();
	void parse_carg(_arg& carg);

	/* speculation parsing */
	/* the one place in the parser that is 
	   hard to parse without speculation is
	   <scope> := '{' *(<declaration> | <statement>) '}'
	   
	   implementing speculate_declaration() is less
	   work than implementing speculate_statement(), specifically
	   becuase speculate_statement() also requires speculate_expression()
	   to work, plus the speculate_*() functions for if and while.

	   Q: could speculation be removed entirely from the parser?
	   A: 10/29/2019 -> yes. technically, the first() set for <scope>s 
	        is LL(1) if we prefix vardecls with a keyword.
			my mind has changed, and i feel that we could reasonably
			remove speculation, because a LL(1) language should never
			speculate.
	 */
	bool speculate_declaration();
	bool speculate_type();
	bool speculate_initializer();

	/* speculate_expression is written differently from
		it's parse_*() equivalent. it was written before
		the parse_*() and I haven't felt the need to rewrite
		it so far. 10/29/2019
	*/
	bool speculate_expression();
	// <primary-expr>
	bool speculate_id_expr();
	bool speculate_literal_expr();

	bool speculate_binop_expr();
	bool speculate_unop_expr();
	// '(' ')' handles nesting parens.
	bool speculate_lparen_expr();
	bool speculate_rparen_expr();
	// <postop-expr>
	bool speculate_postop_expr();
	bool speculate_fcall();
	bool speculate_carg();
};