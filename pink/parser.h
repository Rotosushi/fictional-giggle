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
	/* Module parsing */
	void parse_module_declaration(_module& mdl);

	void parse_module_definition(_module& mdl);
	void parse_module_keyword(_module& mdl);

	/* Variable parsing */
	void parse_variable_declaration(_vardecl& decl);
	void parse_type_specifier(_type_specifier& tspec);
	_ast* parse_initializer();
	void parse_initializer(_vardecl& decl);

	/* composite type parsing */
	void parse_record(_record& stct);
	void parse_record_block(_record& stct);
	void parse_record_member(_record& stct);

	void parse_tuple(_tuple& tpl);
	void parse_type_tuple(_tuple& tpl);

	void parse_array_type(_array& arr);

	/* function parsing */
	void parse_function_definition(_fndecl& fn);

	void parse_lambda(_lambda& fn);
	void parse_lambda_header(_lambda& fn);
	void parse_lambda_body(_lambda& fn);
	void parse_argument_list(vector<_arg>& args);
	void parse_arg(_arg& arg);
	void parse_return_list(vector<_arg>& rargs);

	/* expression parsing */
	_ast* parse_expression();
	_ast* _parse_expression(_ast* lhs, int min_prec);
	_ast* _parse_postop();
	_ast* _parse_primary_expr();

	_fcall* _parse_function_call();
	void parse_carg(_arg& carg);

	_named_member_access* _parse_named_access();
	_positional_member_access* _parse_positional_access();

	/* statement parsing */
	void parse_conditional(_if& conditional);
	void parse_iteration(_while& loop);
	void parse_iteration(_dowhile& loop);

	_ast* parse_statement();

	void parse_block(_scope& body);

	/* speculation functions */
	bool speculate_declaration();
	bool speculate_declaration_and_unwind();
	bool speculate_statement();
	bool speculate_statement_and_unwind();

	bool speculate_block();

	bool speculate_iteration();
	bool speculate_conditional();

	bool speculate_return();

	bool speculate_id_expr();
	bool speculate_literal_expr();
	bool speculate_unop_expr();
	bool speculate_binop_expr();
	bool speculate_postop_expr();
	bool speculate_lparen_expr();
	bool speculate_rparen_expr();
	bool speculate_tuple_in_expr();
	bool speculate_positional_access();
	bool speculate_named_access();

	bool speculate_expression();
	bool speculate_expression_and_unwind();

	bool speculate_type_annotated_arg();

	bool speculate_initializer();
	bool speculate_literal();
	bool speculate_type_specifier();
	bool speculate_type_specifier_and_unwind();
	bool speculate_type_primitive();
	bool speculate_lambda();
	bool speculate_lambda_and_unwind();
	bool speculate_lambda_header();
	bool speculate_lambda_body();
	bool speculate_argument_list();
	bool speculate_return_list();
	bool speculate_arg();
};