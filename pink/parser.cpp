#include <iostream>
using std::cout;
using std::endl;
#include "parser.h"
#include "error.h"

_module* _parser::parse_module()
{
	auto top = new _module;

	nexttok(); // prime our input.

	/* <module> := (<module-level-declaration>)* EOF */

	while (curtok != T_EOF) {
		parse_module_level_declaration(*top);
	}

	return top;
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

void _parser::nexttok()
{
	curtok = lexer.gettok();
}

string _parser::gettext()
{
	return lexer.gettext();
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
	switch (curtok) {
	case T_ID:
		auto vardecl = new _vardecl;
		parse_declaration(*vardecl);
		bool threw = false;
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
		auto fndecl = new _fndecl;
		parse_function_definition(*fndecl);
		if (mdl.resolve_type(fndecl->id) == nullptr)
			mdl.define_type(fndecl);
		else
			throw _parser_error("function already defined: ", fndecl->id); 
		break;
	default: throw _parser_error("invalid top level declaration: ", gettext()); 
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
	
	if (curtok != T_ID) throw _parser_error("declaration doesn't start with identifier, instead got: ", curtok); // parser error
	decl.lhs.id = gettext();
	nexttok();

	switch (curtok) {
	case T_COLON:
		decl.op = curtok;
		nexttok(); // eat ':'
		parse_type_specifier(decl);

		if (curtok == T_EQ) {
			decl.op = T_COLON_EQ;
			nexttok();
			parse_initializer(decl);
		} 

		if (curtok != T_SEMICOLON) {
			throw _parser_error("declaration doesn't end with ';'. instead got: ", curtok);
		}
		else {
			nexttok(); // eat ';'
		}
		break;
	case T_COLON_EQ:
		decl.op = curtok;
		nexttok(); // eat ':='
		parse_initializer(decl);
		break;
	case T_COLON_COLON:
		decl.op = curtok;
		nexttok(); // eat '::'
		parse_initializer(decl);
		break;
	default: throw _parser_error("declaration creation requires; ':', ':=', '::'. instead got: ", curtok);
	}
}

void _parser::parse_function_definition(_fndecl& fn)
{
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

	if (curtok != T_ARROW) throw _parser_error("argument list not followed by '->'. instead got: ", curtok); 
	nexttok();

	if (curtok == T_LPAREN)
		parse_return_list(fn.return_list);

}

void _parser::parse_lambda_body(_lambda& fn)
{
	parse_block(fn.body);
}

void _parser::parse_argument_list(vector<_arg>& args)
{
	/*<arg> := <identifier> (':' <type-specifier>)?*/
	auto build_arg = [this](_arg& a) {
		if (curtok != T_ID) throw;
		a.id = gettext();
		nexttok();

		if (curtok == T_COLON) { // ':'
			parse_type_specifier(a);
		}
	};
	/*
	<argument-list> := '(' (<arg> (',' <arg>)*)? ')'
	*/
	if (curtok != T_LPAREN) throw;
	nexttok();

	if (curtok == T_ID) {
		_arg arg;
		build_arg(arg);
		args.push_back(arg);
		while (curtok == T_COMMA) {
			nexttok(); // ','
			build_arg(arg);
			args.push_back(arg);
		}
	}

	if (curtok != T_RPAREN) throw;
	nexttok();
}

void _parser::parse_return_list(vector<_arg>& args)
{
	//<return-list> :='(' (<type-specifier> (',' <type-specifier>)*)? ')'
	auto build_rarg = [this](_arg& arg) {
		_lambda* l;
		switch (curtok) {
		case T_ID:
			arg.type = _DEF;
			arg.id = gettext();
			nexttok();
			return;
		case T_INT:
			arg.type = _INT;
			arg.value = new _int(stoi(gettext()));
			nexttok();
			return;
		case T_FLOAT:
			arg.type = _FLOAT;
			arg.value = new _float(stof(gettext()));
			nexttok();
			return;
		case T_TEXT:
			arg.type = _TEXT;
			arg.value = new _text(gettext());
			nexttok();
			return;
		case T_LPAREN:
			l = new _lambda;
			parse_lambda_header(*l);

			arg.type = _LAMBDA;
			arg.value = l;
			break;
		default: throw; // parser error
		}
	};
	
	if (curtok != T_LPAREN) throw;
	nexttok(); // eat '('

	_arg arg;

	if (curtok != T_RPAREN) {
		build_rarg(arg); // eat <type-specifier>
		args.push_back(arg);
		while (curtok == T_COMMA) {
			nexttok(); // eat ','
			build_rarg(arg);
			args.push_back(arg);
		}
	}

	if (curtok != T_RPAREN) throw;
	nexttok(); // eat ')'
}

void _parser::parse_expression(_ast* expr)
{
	expr = _parse_expression(_parse_primary_expr(), 0);
}

_ast* _parser::_parse_expression(_ast* lhs, int min_prec)
{
	auto lad = curtok; // lad - lookahead
	while (is_binop(lad) && ptable[lad] >= min_prec) {
		auto op = lad;
		nexttok();
		auto rhs = _parse_primary_expr();
		lad = curtok;
		while (is_binop(lad) && (ptable[lad] > ptable[op]))
		{
			rhs = _parse_expression(rhs, ptable[lad]);
			lad = curtok;
		}
		lhs = new _binop(op, lhs, rhs);
	}
	return lhs;
}

_ast* _parser::_parse_postop()
{
	_lambda* fn;
	switch (curtok) {
	case T_LPAREN:
		fn = _parse_function_call();
		return fn;
	/* TODO:
		case T_LBRACE:

		case T_PERIOD:
	*/
	default: throw; // parser error
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
	switch (curtok) {
	case T_ID:
		var = new _var;
		var->id = gettext();
		nexttok(); // eat <id>
		while (is_postop(curtok))
			var->postops.push_back(_parse_postop());
		return var;
	case T_LPAREN:
		nexttok(); // eat '('

		expr = _parse_expression(_parse_primary_expr(), 0);

		if (curtok != T_RPAREN) throw;
		nexttok(); // eat ')'
		return expr;
	case T_ADD: case T_SUB: case T_MULT:
	case T_LOG_AND: case T_LOG_NOT: case T_BIT_NOT:
		unop = new _unop;

		unop->op = curtok;
		nexttok();
		unop->rhs = _parse_primary_expr();

		return unop;
	case T_LITERAL_INT:
		value = gettext();
		nexttok();
		return new _int(stoi(value));
	case T_LITERAL_FLOAT:
		value = gettext();
		nexttok();
		return new _float(stof(value));
	case T_LITERAL_TEXT:
		value = gettext();
		nexttok();
		return new _text(value);
	case T_TRUE:
		nexttok();
		return new _bool(true);
	case T_FALSE:
		nexttok();
		return new _bool(false);
	default: throw;
	}
}

_lambda* _parser::_parse_function_call()
{
	// '(' (<carg> (',' <carg>)*)? ')'
	// <carg> := <id> | <literal> | <lambda-definition>
	auto parse_carg = [this](_arg& arg) {
		switch (curtok) {
		case T_ID:
			arg.id = gettext();
			arg.type = _VAR;
			break;
		case T_LITERAL_INT:
			arg.type = _INT;
			arg.value = new _int(stoi(gettext()));
			break;
		case T_LITERAL_FLOAT:
			arg.type = _FLOAT;
			arg.value = new _float(stof(gettext()));
			break;
		case T_LITERAL_TEXT:
			arg.type = _TEXT;
			arg.value = new _text(gettext());
			break;
		case T_LPAREN:
			arg.type = _LAMBDA;
			arg.value = new _lambda;
			parse_lambda(*((_lambda*)arg.value));
			return;
		}
		nexttok();
	};

	if (curtok != T_LPAREN) throw;
	nexttok(); // eat '('

	auto fn = new _lambda;

	if (curtok != T_RPAREN) { // empty arg list is semantically valid
		_arg arg;
		parse_carg(arg); // eat <arg>
		fn->argument_list.push_back(arg);

		while (curtok == T_COMMA) {
			nexttok(); // eat ','
			parse_carg(arg); // eat <arg>
			fn->argument_list.push_back(arg);
		}
	}

	if (curtok != T_RPAREN) throw;
	nexttok(); // eat ')'
}

void _parser::parse_conditional(_if& conditional)
{
}

void _parser::parse_iteration(_while& loop)
{
}

void _parser::parse_statement(_ast* expr)
{
}

void _parser::parse_type_specifier(_vardecl& decl)
{
	/*
	<type-specifier> := <lambda-header>
					  | <type-primitive>
					  | <identifier>
	*/
	switch (curtok) {
	case T_ID:
		decl.lhs.type = _DEF;
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
		cout << "error: type specified unknown { " << curtok << " }" << endl;
		throw;
	}
	nexttok(); // eat the single token type-specifier.
}

void _parser::parse_type_specifier(_arg& arg)
{
	// when this function is called the context is
	// <arg> := <identifier> (':' <type-specifier>)?
	// so curtok() is ':'
	nexttok();
	_lambda* lambda;
	switch (curtok) {
	case T_LPAREN: // it's a lambda arg
		lambda = new _lambda;
		parse_lambda_header(*lambda);
		arg.type = _LAMBDA;
		arg.value = lambda;
		break;
	case T_ID:
		arg.type = _DEF;
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
	default: throw; // parser error
	}
}

void _parser::parse_initializer(_vardecl& decl)
{
	/*
	<initializer>  := <lambda-definition>
					| <expression>
	*/
	// this grammer may need speculation........:(
	try {
		
	}
	catch (...) {

	}
}


void _parser::parse_block(_scope& body)
{
	/* <block> :=
			'{' (<declaration> | <statement>)* '}'
	*/
	if (curtok != T_LBRACKET) throw;
	nexttok();

	do {

	} while (curtok != T_RBRACKET); // there is a whole class of missing '}' errors we will want to report here
	nexttok();
}
