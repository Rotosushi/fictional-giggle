#include <iostream>
using std::cout;
using std::endl;
#include "parser.h"
#include "error.h"

_module* _parser::parse_module()
{
	auto top = new _module;

	sync(1); // prime our input. (nexttok() will advance one token too far)

	/* <module> := (<module-level-declaration>)* EOF */

	while (curtok() != T_EOF) {
		try {
			parse_module_level_declaration(*top);
		}
		catch (_parser_error err) {
			cout << err.what() << endl;
			exit(0);
		}
	}

	return top;
}

_module* _parser::parse_module(string input)
{
	lexer.set_instring(input);
	return parse_module();
}

_module* _parser::parse_module(ifstream& input)
{
	lexer.set_infile(input);
	return parse_module();
}

void _parser::init_precedence_table()
{
	ptable[T_EQ] = 1;

	ptable[T_LOG_EQUALS] = 2;
	ptable[T_LOG_NOT_EQUALS] = 2;

	ptable[T_LOG_LESS] = 3;
	ptable[T_LOG_GREATER] = 3;
	ptable[T_LOG_LESS_EQUALS] = 3;
	ptable[T_LOG_GREATER_EQUALS] = 3;

	ptable[T_LOG_OR] = 4;
	ptable[T_LOG_XOR] = 5;
	ptable[T_LOG_AND] = 6;

	ptable[T_BIT_OR] = 7;
	ptable[T_BIT_XOR] = 8;
	ptable[T_BIT_AND] = 9;

	ptable[T_BIT_LSHIFT] = 10;
	ptable[T_BIT_RSHIFT] = 10;

	ptable[T_ADD] = 11;
	ptable[T_SUB] = 11;

	ptable[T_MULT] = 12;
	ptable[T_DIV] = 12;
	ptable[T_MOD] = 12;
}

_token _parser::curtok()
{
	return tokbuf[tokidx];
}

void _parser::nexttok()
{
	tokidx++;// "consume" the token
			  // recall that this is a backtracking parser,
			  // so we cannot remove a token from the buffer
			  // in case we need to reparse it, so we instead
			  // move the pointer one position along the buffer,
			  // this means that the next function to try and
			  // "match" a token will look at the next token
			  // in sequence instead of the same token. Since
			  // all consumption is done through this function
			  // the program will be able to reparse the input.

	if (tokidx == tokbuf.size() && !speculating()) {
		tokidx = 0;
		tokbuf.clear();
		texbuf.clear();
	}

	sync(1); // get a new token to replace the consumed token.
			 // either we have a semifull buffer and sync
			 // is a noop, or we have a full or empty buffer
			 // and we need to prime the next token.
}

string _parser::curtext()
{
	return texbuf[tokidx];
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
	tokidx = mark;
}

bool _parser::speculate(_token t)
{
	if (t == curtok()) {
		nexttok();
		return true;
	}
	else return false;
}

bool _parser::speculating()
{
	return marks.size() > 0;
}

void _parser::sync(int i)
{
	if ((size_t)((uint64_t)tokidx + i) > tokbuf.size()) { // do we need more tokens than we have?
		int n = (tokidx + i) - tokbuf.size(); // how many more do we need?
		for (int i = 0; i < n; i++) {
			tokbuf.push_back(lexer.gettok());  // get n tokens
			texbuf.push_back(lexer.gettext()); // get text for each token
		}
	}
}

bool _parser::is_unop(_token t)
{
	// valid prefix tokens '-', '+', '*', '&', '!', '!!'
	if (t == T_ADD)			return true;
	if (t == T_SUB)			return true;
	if (t == T_BIT_AND)		return true;
	if (t == T_MULT)		return true;
	if (t == T_BIT_NOT)		return true;
	if (t == T_LOG_NOT)		return true;
	return false;
}

bool _parser::is_binop(_token t)
{
	/* valid binop tokens: =, +=, -=, *=,
							/=, %=, ||=, &&=,
							^^=, >>=, <<=, +,
							-, *, /, %,
							||, ^^, &&,
							|, ^, &,
							!=, ==
	*/
	if (t == T_EQ)					return true;
	if (t == T_ADD)					return true;
	if (t == T_SUB)					return true;
	if (t == T_MULT)				return true;
	if (t == T_DIV)					return true;
	if (t == T_MOD)					return true;
	if (t == T_BIT_OR)				return true;
	if (t == T_BIT_XOR)				return true;
	if (t == T_BIT_AND)				return true;
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

bool _parser::is_literal(_token t)
{
	if (t == T_LITERAL_INT)		return true;
	if (t == T_LITERAL_FLOAT)	return true;
	if (t == T_LITERAL_TEXT)	return true;
	if (t == T_TRUE)			return true;
	if (t == T_FALSE)			return true;
	return false;
}

bool _parser::is_postop(_token t)
{
	// postop :=  '(' <arg> (',' <arg>)* ')'
//			| '[' <expr> ']'
//			| '.' <id>
	if (t == T_LPAREN)	 return true;
	//if (t == T_LBRACE)	 return true;
	//if (t == T_PERIOD)	 return true;
	return false;
}

void _parser::parse_module_level_declaration(_module& mdl)
{
	bool threw = false;
	_vardecl* vardecl;
	_fndecl* fndecl;
	switch (curtok()) {
	case T_ID:
		vardecl = new _vardecl;
		parse_declaration(*vardecl);

		/*
			This code kinda stinks becuase it uses
			exceptions in regular control flow.

			were there an optional type, this pattern could
			be cleaner
		*/

		
		try {
			mdl.body.resolve(vardecl->lhs.id);
		}
		catch (...) {
			threw = true;
		}
		if (threw) mdl.body.define(*vardecl);
		else throw _parser_error("variable already defined: ", vardecl->lhs.id);

		break;
	case T_FN:
		fndecl = new _fndecl;
		parse_function_definition(*fndecl);
		if (mdl.resolve_type(fndecl->id) == nullptr)
			mdl.define_type(fndecl);
		else
			throw _parser_error("function already defined: ", fndecl->id); 
		break;
	default: throw _parser_error("invalid top level declaration: ", curtext()); 
	}
}

void _parser::parse_declaration(_vardecl& decl)
{
	/*
<declaration>  := <identifier> ':' <type-specifier> ';'
				| <identifier> ':' <type-specifier> '=' <initializer> ';'
				| <identifier> '::' <initializer> ';'
				| <identifier> ':=' <initializer> ';'

	*/
	
	if (curtok() != T_ID) throw _parser_error("declaration doesn't start with identifier, instead got: ", curtok()); // parser error
	decl.lhs.id = curtext();
	nexttok();

	switch (curtok()) {
	case T_COLON:
		decl.op = curtok();
		nexttok(); // eat ':'
		parse_type_specifier(decl);

		if (curtok() == T_EQ) {
			decl.op = T_COLON_EQ;
			nexttok();
			parse_initializer(decl);
		} 

		if (curtok() != T_SEMICOLON)
			throw _parser_error("declaration doesn't end with ';'. instead got: ", curtok());
		else nexttok(); // eat ';'

		break;
	case T_COLON_EQ: case T_COLON_COLON:
		decl.op = curtok();
		nexttok(); // eat ':=' | '::'

		parse_initializer(decl);

		if (curtok() != T_SEMICOLON) 
			throw _parser_error("declaration doesn't end with ';'. instead got: ", curtok());
		else nexttok();

		break;
	default: throw _parser_error("declaration creation requires; ':', ':=', '::'. instead got: ", curtok());
	}
}

void _parser::parse_function_definition(_fndecl& fn)
{
	// 'fn' <identifier> '::' <lambda-definition>
	if (curtok() != T_FN) throw _parser_error("invalid function definition, leading 'fn' missing, instead got: ", curtok());
	nexttok();

	if (curtok() != T_ID) throw _parser_error("invalid function definition, missing identifier, instead got: ", curtok());
	fn.id = curtext();
	nexttok();

	if (curtok() != T_COLON_COLON) throw _parser_error("invalid function definition, missing '::', instead got: ", curtok());
	nexttok();

	parse_lambda(fn.fn);
}

void _parser::parse_lambda(_lambda& fn)
{
	parse_lambda_header(fn);
	parse_lambda_body(fn);
}

void _parser::parse_lambda_header(_lambda& fn)
{
	/*  <argument-list> '->' (  <return-list> )? */

	parse_argument_list(fn.argument_list);

	if (curtok() != T_ARROW) throw _parser_error("argument list not followed by '->'. instead got: ", curtok()); 
	nexttok();

	if (curtok() == T_LPAREN)
		parse_return_list(fn.return_list);
}

void _parser::parse_lambda_body(_lambda& fn)
{
	parse_block(fn.body);
}

void _parser::parse_argument_list(vector<_arg>& args)
{
	/*
	<argument-list> := '(' (<arg> (',' <arg>)*)? ')'
	*/
	if (curtok() != T_LPAREN) throw _parser_error("invalid argument list, missing '(', instead got: ", curtok());
	nexttok();

	if (curtok() == T_ID) {
		_arg arg;
		parse_arg(arg);
		args.push_back(arg);
		while (curtok() == T_COMMA) {
			nexttok(); // ','
			parse_arg(arg);
			args.push_back(arg);
		}
	}

	if (curtok() != T_RPAREN) throw _parser_error("invalid argument list, missing ')', instead got: ", curtok());
	nexttok();
}

void _parser::parse_arg(_arg& arg)
{
	/*
	<arg> := <identifier> (':' <type-specifier>)?
		   | <expr>
	*/
	if (speculate_type_annotated_arg()) {
		arg.id = curtext();
		nexttok();

		if (curtok() == T_COLON) {
			nexttok();
			parse_type_specifier(arg);
		}
		else {
			arg.type = _DEDUCE;
		}
	}
	else {
		arg.type = _DEDUCE; // throw the task of figuring out the type down the line
		arg.value = parse_expression();
	}
}

void _parser::parse_return_list(vector<_rarg>& rargs)
{
	//<return-list> :='(' (<type-specifier> (',' <type-specifier>)*)? ')'
	
	if (curtok() != T_LPAREN) throw _parser_error("invalid return list, missing '(', instead got: ", curtok());
	nexttok(); // eat '('

	_rarg rarg;

	if (curtok() != T_RPAREN) {
		parse_rarg(rarg); // eat <type-specifier>
		rargs.push_back(rarg);
		while (curtok() == T_COMMA) {
			nexttok(); // eat ','
			parse_rarg(rarg);
			rargs.push_back(rarg);
		}
	}

	if (curtok() != T_RPAREN) throw _parser_error("invalid return list, missing ')', instead got: ", curtok());
	nexttok(); // eat ')'
}

void _parser::parse_rarg(_rarg& rarg)
{
	// <rarg> := <id> | <type-primitive> | <lambda-header>
	_lambda* l;
	switch (curtok()) {
	case T_ID:
		rarg.id = curtext();
		rarg.type = _DEDUCE;
		break;
	case T_INT:
		rarg.type = _INT;
		break;
	case T_FLOAT:
		rarg.type = _FLOAT;
		break;
	case T_TEXT:
		rarg.type = _TEXT;
		break;
	case T_BOOL:
		rarg.type = _BOOL;
		break;
	case T_LPAREN:
		l = new _lambda;

		rarg.type = _LAMBDA;
		parse_lambda_header(*l);

		rarg.value = l;
		break;
	default: throw _parser_error("unrecognized return argument, expected: identifier or type primitive or lambda type, instead got: ", curtok());
	}
	nexttok();
}

_ast* _parser::parse_expression()
{
	return _parse_expression(_parse_primary_expr(), 0);
}

_ast* _parser::_parse_expression(_ast* lhs, int min_prec)
{
	auto lad = curtok(); // lad - lookahead
	while (is_binop(lad) && ptable[lad] >= min_prec) {
		auto op = lad;
		nexttok();
		auto rhs = _parse_primary_expr();
		lad = curtok();
		while (is_binop(lad) && (ptable[lad] > ptable[op]))
		{
			rhs = _parse_expression(rhs, ptable[lad]);
			lad = curtok();
		}
		lhs = new _binop(op, lhs, rhs);
	}
	return lhs;
}

_ast* _parser::_parse_postop()
{
	_fcall* fn;
	_member_access* ma;
	_array_access* aa;
	switch (curtok()) {
	case T_LPAREN:
		fn = _parse_function_call();
		return fn;

	case T_LBRACKET:
		aa = _parse_array_access();
		return aa;

	case T_PERIOD:
		ma = _parse_member_access();
		return ma;
	
	default: throw _parser_error("unrecognized postop token, expected '(' | '.' | '[' got: ", curtok()); // parser error
	}
}

_ast* _parser::_parse_primary_expr()
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
	_var* var;
	_ast* expr;
	_unop* unop;
	string value;
	switch (curtok()) {
	case T_ID:
		var = new _var;
		var->id = curtext();
		nexttok(); // eat <id>
		while (is_postop(curtok()))
			var->postops.push_back(_parse_postop());
		return var;
	case T_LPAREN:
		nexttok(); // eat '('

		expr = _parse_expression(_parse_primary_expr(), 0);

		if (curtok() != T_RPAREN) throw _parser_error("invalid primary expression, while parsing parenthised expression expected closing ')', instead got: ", curtok());
		nexttok(); // eat ')'
		return expr;
	case T_ADD: case T_SUB: case T_MULT:
	case T_LOG_AND: case T_LOG_NOT: case T_BIT_NOT:
		unop = new _unop;

		unop->op = curtok();
		nexttok();
		unop->rhs = _parse_primary_expr();

		return unop;
	case T_LITERAL_INT:
		value = curtext();
		nexttok();
		return new _int(stoi(value));
	case T_LITERAL_FLOAT:
		value = curtext();
		nexttok();
		return new _float(stof(value));
	case T_LITERAL_TEXT:
		value = curtext();
		nexttok();
		return new _text(value);
	case T_TRUE:
		nexttok();
		return new _bool(true);
	case T_FALSE:
		nexttok();
		return new _bool(false);
	default: throw _parser_error("invalid primary expression, expected: identifier or unary expression or parenthised expression or literal, instead got: ", curtok());
	}
}

_fcall* _parser::_parse_function_call()
{
	// '(' (<carg> (',' <carg>)*)? ')'

	if (curtok() != T_LPAREN) throw _parser_error("invalid function call, expected '(', instead got: ", curtok());
	nexttok(); // eat '('

	auto fn = new _fcall;

	if (curtok() != T_RPAREN) { // empty arg list is semantically valid
		_carg carg;
		parse_carg(carg); // eat <arg>
		fn->argument_list.push_back(carg);

		while (curtok() == T_COMMA) {
			nexttok(); // eat ','
			parse_carg(carg); // eat <arg>
			fn->argument_list.push_back(carg);
		}
	}

	if (curtok() != T_RPAREN) throw _parser_error("invalid function call, expected ')', instead got: ", curtok());
	nexttok(); // eat ')'

	return fn;
}

void _parser::parse_carg(_carg& carg)
{
	// <carg> := <expr> | <lambda-definition>
	if (speculate_expression_and_unwind()) {
		carg.value = parse_expression();
	}
	else {
		auto lam = new _lambda;
		parse_lambda(*lam);
		carg.value = lam;
	}
}

_member_access* _parser::_parse_member_access()
{

	// <member-access> = '.' <identifier>
	if (curtok() != T_PERIOD) throw;
	nexttok();

	if (curtok() != T_ID) throw;
	auto memb = new _member_access;
	memb->member_id = curtext();

	nexttok();

	return memb;
}

_array_access* _parser::_parse_array_access()
{
	// '[' <expr> ']'
	_array_access* aa;

	if (curtok() != T_LBRACKET) throw;
	nexttok();

	aa = new _array_access;
	aa->offset_expression = parse_expression();

	if (curtok() != T_RBRACKET) throw;
	nexttok();

	return aa;
}

void _parser::parse_conditional(_if& conditional)
{
	/*
	<conditional>  := 'if' '(' (<expr>)? ')' <statement>
				| 'if' '(' (<expr>)? ')' <statement> 'else' <statement>
	*/
	if (curtok() == T_IF) {
		nexttok();
		if (curtok() != T_LPAREN) throw _parser_error("invalid if, expected '(', instead got: ", curtok());
		nexttok();
		conditional.cond = parse_expression();
		if (curtok() != T_RPAREN)  throw _parser_error("invalid if, expected ')', instead got: ", curtok());
		nexttok();
		conditional.then = parse_statement();
		if (curtok() == T_ELSE) {
			nexttok();
			conditional.els = parse_statement();
		}
	}
	else  throw _parser_error("invalid if, expected 'if', instead got: ", curtok());
}

void _parser::parse_iteration(_while& loop)
{
	if (curtok() == T_WHILE) {
		nexttok();
		if (curtok() != T_LPAREN)  throw _parser_error("invalid while, expected '(', instead got: ", curtok());
		nexttok();
		loop.cond = parse_expression();
		if (curtok() != T_RPAREN) throw _parser_error("invalid while, expected ')', instead got: ", curtok());;
		nexttok();
		loop.body = parse_statement();
	}
	else throw _parser_error("invalid while, expected 'while', instead got: ", curtok());;
}

void _parser::parse_iteration(_dowhile& loop)
{
	if (curtok() == T_DO) {
		nexttok();
		loop.body = parse_statement();
		if (curtok() != T_WHILE) throw _parser_error("invalid do while, expected 'while', instead got: ", curtok());;
		nexttok();
		if (curtok() != T_LPAREN) throw _parser_error("invalid do while, expected '(', instead got: ", curtok());;
		nexttok();
		loop.cond = parse_expression();
		if (curtok() != T_RPAREN) throw _parser_error("invalid do while, expected ')', instead got: ", curtok());;
		nexttok();
	}
	else throw _parser_error("invalid do while, expected 'do', instead got: ", curtok());;
}

_ast* _parser::parse_statement()
{
	if (curtok() == T_LBRACE) {
		auto block = new _scope;
		parse_block(*block);
		return block;
	}
	else if (curtok() == T_IF) {
		auto cond = new _if;
		parse_conditional(*cond);
		return cond;
	}
	else if (curtok() == T_WHILE) {
		auto loop = new _while;
		parse_iteration(*loop);
		return loop;
	}
	else if (curtok() == T_DO) {
		auto loop = new _dowhile;
		parse_iteration(*loop);
		return loop;
	}
	else if (curtok() == T_RETURN) {
		nexttok();

		auto ret = new _return;
		ret->rhs = parse_expression();
		
		if (curtok() != T_SEMICOLON) throw;
		nexttok();

		return ret;
	}
	else { // the only other choice is an expression
		_ast* st = nullptr;
		st = parse_expression();
		
		if (curtok() != T_SEMICOLON) throw;
		nexttok();

		return st;
	}
}

void _parser::parse_type_specifier(_vardecl& decl)
{
	/*
	<type-specifier> := <lambda-header>
					  | <type-primitive>
					  | <identifier>
	*/
	switch (curtok()) {
	case T_ID:
		decl.lhs.tname = curtext();
		decl.lhs.type = _DEDUCE;
		decl.rhs = nullptr;
		break;
	case T_INT:
		decl.lhs.type = _INT;
		decl.rhs = new _int;
		break;
	case T_FLOAT:
		decl.lhs.type = _FLOAT;
		decl.rhs = new _float;
		break;
	case T_TEXT:
		decl.lhs.type = _TEXT;
		decl.rhs = new _text;
		break;
	case T_BOOL:
		decl.lhs.type = _BOOL;
		decl.rhs = new _bool;
		break;
	case T_LPAREN:
		decl.lhs.type = _LAMBDA;
		decl.rhs = new _lambda;
		parse_lambda_header(*((_lambda*)decl.rhs));
		return; // parse_* functions act like nexttok(), and
		        // we don't want to eat an extra token, 
				// hence the early return.
	default: 
		throw _parser_error("invalid type specifier, expected: identifier or type primitive or lambda type, instead got: ", curtok());
	}
	nexttok(); // eat the single token type-specifier.
}

void _parser::parse_type_specifier(_arg& arg)
{
	// when this function is called the context is
	// <arg> := <identifier> (':' <type-specifier>)?
	_lambda* lambda;
	switch (curtok()) {
	case T_LPAREN: // it's a lambda arg
		lambda = new _lambda;
		parse_lambda_header(*lambda);
		arg.type = _LAMBDA;
		arg.value = lambda;
		break;
	case T_ID:
		arg.id = curtext();
		arg.type = _DEDUCE;
		nexttok();
		break;
	case T_INT:
		arg.type = _INT;
		nexttok();
		break;
	case T_FLOAT:
		arg.type = _FLOAT;
		nexttok();
		break;
	case T_TEXT:
		arg.type = _TEXT;
		nexttok();
		break;
	case T_BOOL:
		arg.type = _BOOL;
		nexttok();
		break;
	default: throw _parser_error("invalid type specifier, expected: identifier or type primitive or lambda type, instead got: ", curtok());
	}
}

void _parser::parse_initializer(_vardecl& decl)
{
	/*
	<initializer>  := <lambda-definition>
					| <expression>
	*/
	if (speculate_lambda_and_unwind()) {
		auto lambda = new _lambda;
		parse_lambda(*lambda);
		decl.lhs.type = _LAMBDA;
		decl.rhs = lambda;
	}
	else {
		decl.lhs.type = _DEDUCE;
		decl.rhs = parse_expression();
	}
}

void _parser::parse_block(_scope& body)
{
	/* <block> :=
			'{' (<declaration> | <statement>)* '}'
	*/
	if (curtok() != T_LBRACE) throw _parser_error("invalid block, expected '{', instead got: ", curtok());
	nexttok();

	_vardecl d;
	_ast* s = nullptr;
	int n = 0, m = 0;

	do {
		if (speculate_declaration_and_unwind()) {
			parse_declaration(d);
			body.define(d);
			d.clear();
			n++;
		}
		else if (speculate_statement_and_unwind()) {
			s = parse_statement();
			body.statements.push_back(s);
			s = nullptr;
			n++;
		}

		// not a decl, statement, or '}'?
		if (n == 0) {
			// looped more than once without parsing anything?
			m++;
			if (m > 1) throw _parser_error("invalid block, expected: declaration or statement, instead got: ", curtok());
		}
		else {
			n = 0;
		}
	} while (curtok() != T_RBRACE); // there is a whole class of missing '}' errors we will want to report here
	nexttok();
}

bool _parser::speculate_declaration()
{
	bool success = true;
	if (speculate(T_ID)) {
		if (speculate(T_COLON)) {
			if (speculate_type_specifier()) {
				if (speculate(T_SEMICOLON)) {

				}
				else if (speculate(T_EQ)) {
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
		else if (speculate(T_COLON_EQ) || speculate(T_COLON_COLON)) {
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

bool _parser::speculate_declaration_and_unwind()
{
	bool success = true;
	mark();
	success = speculate_declaration();
	release();
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
	else if (speculate_return()) {

	}
	else if (speculate_expression()) {

	}
	else success = false;
	return success;
}

bool _parser::speculate_statement_and_unwind()
{
	bool success = true;
	mark();
	success = speculate_statement();
	release();
	return success;
}

bool _parser::speculate_block()
{
	bool success = true;
	if (speculate(T_LBRACE)) {
		while (speculate_declaration() || speculate_statement());

		if (speculate(T_RBRACE)) {

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

bool _parser::speculate_return()
{
	bool success = true;
	if (speculate(T_RETURN)) {
		if (speculate_expression());

		if (speculate(T_SEMICOLON));
		else success = false;
	}
	else success = false;
	return success;
}

bool _parser::speculate_id_expr()
{
	// <id> can be followed by <binop>, <postop>, '(', ')', '[', ']', '.', or ';'
	nexttok(); // consume <id>
	if (is_binop(curtok()))
		return speculate_binop_expr();
	if (is_postop(curtok()))
		return speculate_postop_expr();
	if (curtok() == T_RPAREN)
		return speculate_rparen_expr();
	// this may be questionable, but when <id> is followed by ']', ']' is assumed to be a terminal character 
	// for the case of array (pointer) math:
	//		A[B + C] = D;
	if (curtok() == T_RBRACE)
		return true;
	if (curtok() == T_SEMICOLON && parens.size() == 0)
		return true;

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_literal_expr()
{
	// <literal> can be followed by <binop>, ')' or ';'
	nexttok();
	if (is_binop(curtok()))
		return speculate_binop_expr();
	if (curtok() == T_RPAREN)
		return speculate_rparen_expr();
	if (curtok() == T_SEMICOLON && parens.size() == 0)
		return true;

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_unop_expr()
{
	// <unop> can be followed by <unop>, <id>, or <literal>
	nexttok(); // consume <unop>
	if (is_unop(curtok()))
		return speculate_unop_expr();
	if (is_literal(curtok()))
		return speculate_literal_expr();
	if (curtok() == T_ID)
		return speculate_id_expr();
	if (curtok() == T_LPAREN)
		return speculate_lparen_expr();

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_binop_expr()
{
	// <binop> can be followed by <unop>, <id>, '(', or <literal>
	nexttok(); // consume <binop>
	if (is_unop(curtok()))
		return speculate_unop_expr();
	if (is_literal(curtok()))
		return speculate_literal_expr();
	if (curtok() == T_ID)
		return speculate_id_expr();
	if (curtok() == T_LPAREN)
		return speculate_lparen_expr();

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_postop_expr()
{
	// <postop> can be followed by <postop>, <binop>, or ';'
	// consume <postop>
	if (speculate_argument_list());
	else { // malformed postop == malformed expression
		while (parens.size() > 0) parens.pop();
		return false;
	}

	if (is_postop(curtok()))
		return speculate_postop_expr();
	if (is_binop(curtok()))
		return speculate_binop_expr();
	if (curtok() == T_RPAREN)
		return speculate_rparen_expr();
	if (curtok() == T_SEMICOLON && parens.size() == 0)
		return true;

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_lparen_expr()
{
	// '(' can be followed by <expr>
	nexttok();
	parens.push(0); // push an open paren on the stack that needs closing
	return speculate_expression();
}

bool _parser::speculate_rparen_expr()
{
	// ')' can be followed by ')', <binop>, ';' or be the terminal character
	if (parens.size() > 0)
		parens.pop();
	else if (parens.size() == 0) // if we don't have any open sub-expressions
								 // we assume that the rparen is terminating the expression
		return true;

	nexttok();

	if (is_binop(curtok()))
		return speculate_binop_expr();
	if (curtok() == T_SEMICOLON && parens.size() == 0)
		return true;
	if (curtok() == T_RPAREN) {
		return speculate_rparen_expr();
	}

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_expression()
{
	// an expression is started by <id>, <literal>, <unop>, '('
	//  or immediately ended by ')' or ';'
	if (curtok() == T_ID)
		return speculate_id_expr();
	if (is_literal(curtok()))
		return speculate_literal_expr();
	if (is_unop(curtok()))
		return speculate_unop_expr();
	if (curtok() == T_LPAREN)
		return speculate_lparen_expr();
	if (curtok() == T_RPAREN) // empty expressions are valid
		return speculate_rparen_expr();
	if (curtok() == T_SEMICOLON && parens.size() == 0) // empty expressions are valid
		return true;
	return false;
}

bool _parser::speculate_expression_and_unwind()
{
	bool success = true;
	mark();
	success = speculate_expression();
	release();
	return success;
}

bool _parser::speculate_type_annotated_arg()
{
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

bool _parser::speculate_initializer()
{
	bool success = true;
	if (speculate_lambda());
	else if (speculate_expression());
	else success = false;
	return success;
}

bool _parser::speculate_literal()
{
	bool success = true;
	if (speculate(T_LITERAL_INT));
	else if (speculate(T_LITERAL_FLOAT));
	else if (speculate(T_LITERAL_TEXT));
	else if (speculate(T_TRUE));
	else if (speculate(T_FALSE));
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
	if (speculate(T_ID));
	else if (speculate_literal());
	else if (speculate_type_primitive());
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
	if (speculate(T_INT));
	else if (speculate(T_FLOAT));
	else if (speculate(T_TEXT));
	else if (speculate(T_BOOL));
	else success = false;
	return success;
}

bool _parser::speculate_lambda()
{
	bool success = true;
	if (speculate_lambda_header())
		if (speculate_lambda_body());
		else success = false;
	else success = false;
	return success;
}

bool _parser::speculate_lambda_and_unwind()
{
	bool success = true;
	mark();
	if (speculate_lambda_header())
		if (speculate_lambda_body());
		else success = false;
	else success = false;
	this->release();
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

bool _parser::speculate_lambda_body()
{
	return speculate_block();
}

bool _parser::speculate_argument_list()
{
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

bool _parser::speculate_arg()
{
	/*
		<arg> := <identifier> (':' <type-specifier>)?
				| <expr>
	*/
	bool success = true;
	if (speculate_expression_and_unwind()) {
		speculate_expression();
	}
	else if (speculate(T_ID)) {
		if (speculate(T_COLON)) {
			if (speculate_type_specifier()) {

			}
			else success = false;
		}
	}
	else success = false;
	return success;
}
