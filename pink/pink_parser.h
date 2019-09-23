#pragma once
#include <stack>
using std::stack;
#include <unordered_map>
using std::unordered_map;
#include "pink_lexer.h"
#include "pink_ast.h"
#include "pink_semantic_analysis.h"

class old_parser {
public:
	_module* parse_module();
	_module* parse_module(char* filename);
	_module* parse_module(string s);

	old_parser();
private:
	/* _parser internal variables */
	old_lexer lex;
	_semantic_analyzer san;
	unordered_map<_tok, int> precedence;
	stack<int>		marks;
	vector<old_token>	tokbuf;
	int				tokidx;
	stack<int> parens;

	/* support functions+ */
	/* general */
	void init_precedence_table();
	void print_token_buffer();
	void print_token_buffer(int i);
	void parser_error(const char* str);

	/* speculation support */
	int  mark();
	void release();

	/* parsing primitives */
	old_token curtok();
	bool  speculate(_tok tok);
	void  consume();
	void  sync(int i);
	bool  speculating();

	bool is_unop(_tok t);
	bool is_binop(_tok t);
	bool is_literal(_tok t);
	bool is_postop(_tok t);

	/* parser speculation functions */
	/*
	try_* functions rewind their input 
		after speculating, these are used
		to direct parsing before syntactic
		information in stored from the
		input text
	*/
	bool try_speculate_declaration();
	bool try_speculate_alias();
	bool try_speculate_struct();
	bool try_speculate_union();
	bool try_speculate_function();
	bool try_speculate_lambda();
	bool try_speculate_conditional();
	bool try_speculate_iteration();
	bool try_speculate_expression();

	/*
	These functions compose together
		to form the parser for the language.
		nearly every grapheme from the grammar
		has a function equivalent, and they are
		mutually recursive to build higher syntactic
		forms out of smaller more manageable forms.
	*/
	bool speculate_declaration();
	bool speculate_type_specifier();
	bool speculate_type_primitive();
	bool speculate_initializer();

	bool speculate_alias();

	bool speculate_struct();
	bool speculate_union();
	bool speculate_composite_type_block();
	
	bool speculate_function();
	bool speculate_lambda();
	bool speculate_lambda_header();
	bool speculate_argument_list();
	bool speculate_arg();
	bool speculate_return_list();

	bool speculate_statement();
	bool speculate_conditional();
	bool speculate_iteration();
	bool speculate_block();

	bool speculate_expression();
	bool speculate_literal();
	bool speculate_id();
	bool speculate_binop();
	bool speculate_unop();
	bool speculate_postop();
	bool speculate_lparen();
	bool speculate_rparen();

	/* 
		parse_* functions expect to be called
		once the parser has already figured
		out what syntactic form we are looking
		at, these pull out the relevant syntactic
		information and create _ast nodes to build
		the Intermediate Representation of the program.
	*/
	_ast* parse_top_level_declaration();
	void parse_declaration(_declaration& decl);

	void parse_type_specifier(_declaration& decl);
	void parse_type_specifier(_arg& arg);

	void parse_initializer(_declaration& decl);

	void parse_expression(_statement& expr);
	void parse_expression(_ast* expr);
	_ast* _parse_expression(_ast* lhs, int min_prec);
	_ast* parse_postop();
	_ast* parse_primary_expr();
	_tok predict_type(_ast* expr);

	void parse_function_call(_lambda& fun);
	void parse_array_access(_iterator& iter);
	void parse_member_access(_member& memb);
	
	void parse_alias(_alias& alias);

	void parse_struct(_struct& strct);
	void parse_union(_union& unn);
	void parse_composite_type_block(vector<_declaration>& decls);

	void parse_function(_function& fun);
	void parse_lambda(_lambda& fun);
	void parse_lambda_header(_lambda& fun);
	void parse_argument_list(vector<_arg>& args);
	void parse_return_list(vector<_arg>& args);

	void parse_statement(_ast* stmt);
	void parse_iteration(_while& loop);
	void parse_iteration(_dowhile& loop);

	void parse_conditional(_if& conditional);

	void parse_block(_scope& scope);
};



