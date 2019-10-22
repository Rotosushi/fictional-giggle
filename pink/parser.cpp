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
			parse_module_declaration(*top);
		}
		catch (_parser_error err) {
			cout << err.what() << endl;
			exit(0);
		}
	}

	reset_internal_state();
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
	ptable[T_COMMA] = 1;

	ptable[T_EQ] = 2;

	ptable[T_LOG_EQUALS] = 3;
	ptable[T_LOG_NOT_EQUALS] = 3;

	ptable[T_LOG_LESS] = 4;
	ptable[T_LOG_GREATER] = 4;
	ptable[T_LOG_LESS_EQUALS] = 4;
	ptable[T_LOG_GREATER_EQUALS] = 4;

	ptable[T_LOG_OR] = 5;
	ptable[T_LOG_XOR] = 6;
	ptable[T_LOG_AND] = 7;

	ptable[T_BIT_OR] = 8;
	ptable[T_BIT_XOR] = 9;
	ptable[T_BIT_AND] = 10;

	ptable[T_BIT_LSHIFT] = 11;
	ptable[T_BIT_RSHIFT] = 11;

	ptable[T_ADD] = 12;
	ptable[T_SUB] = 12;

	ptable[T_MULT] = 13;
	ptable[T_DIV] = 13;
	ptable[T_MOD] = 13;
}

void _parser::reset_internal_state()
{
	while (parens.size() > 0) parens.pop();
	tokbuf.clear();
	texbuf.clear();
	tokidx = 0;
	while (marks.size() > 0) marks.pop();
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
	if (t == T_COMMA)				return true;
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
	if (t == T_LBRACKET)	 return true;
	if (t == T_PERIOD)	 return true;
	return false;
}

bool _parser::is_module_keyword(_token t)
{	
	if (curtok() == T_IMPORT) return true;
	if (curtok() == T_EXPORT) return true;
	if (curtok() == T_BEGIN) return true;
	return false;
}

void _parser::parse_module_declaration(_module& mdl)
{
	bool threw = false;
	_vardecl* vardecl;
	_fndecl* fndecl;
	_record* stctdecl;
	switch (curtok()) {
	case T_ID:
		vardecl = new _vardecl;
		parse_variable_declaration(*vardecl);

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
		else throw _parser_error(__FILE__, __LINE__, "variable already defined: ", vardecl->lhs.id);

		break;
	case T_FN:
		fndecl = new _fndecl;
		parse_function_definition(*fndecl);
		if (mdl.resolve_type(fndecl->id) == nullptr)
			mdl.define_type(fndecl);
		else
			throw _parser_error(__FILE__, __LINE__, "function already defined: ", fndecl->id); 
		break;

	case T_RECORD:
		stctdecl = new _record;
		parse_record(*stctdecl);
		if (mdl.resolve_type(stctdecl->id) == nullptr)
			mdl.define_type(stctdecl);
		else
			throw _parser_error(__FILE__, __LINE__,  "struct already defined: ", stctdecl->id);
		break;
	case T_MODULE:
		if (mdl.import_list.size() > 0 ||
			mdl.export_list.size() > 0 ||
			mdl.main_fn.size() > 0)
			throw _parser_error(__FILE__, __LINE__,  "module definition block already parsed", mdl.id);
		
		parse_module_definition(mdl);
		break;
	default: throw _parser_error(__FILE__, __LINE__,  "invalid top level declaration: ", curtext());
	}
}

void _parser::parse_module_definition(_module& mdl)
{
	/*
	<module-definition> := 'module' '::' <module-definition-block>

	<module-definition-block> := '{' (<module-keyword-expression>)* '}'

	<module-keyword-expression> := 'import' <id> (',' <id>)* ';'
								 | 'export' <id> (',' <id>)* ';'
								 | 'begin' <id> ';'

	*/
	if (curtok() != T_MODULE) throw;
	nexttok();

	if (curtok() != T_COLON_COLON) throw;
	nexttok();

	if (curtok() != T_LBRACE) throw;
	nexttok();

	while (is_module_keyword(curtok())) {
		parse_module_keyword(mdl);
	}

	if (curtok() != T_RBRACE) throw;
	nexttok();
}

void _parser::parse_module_keyword(_module& mdl)
{
	/*
	<module-keyword-expression> := 'import' <id> (',' <id>)* ';'
								 | 'export' <id> (',' <id>)* ';'
								 | 'begin' <id> ';'
	*/
	switch (curtok()) {
	case T_IMPORT:
		nexttok();

		if (curtok() != T_ID) throw;
		mdl.import_list.push_back(curtext());
		nexttok();

		while (curtok() == T_COMMA) {
			nexttok();

			if (curtok() != T_ID) throw;
			mdl.import_list.push_back(curtext());
			nexttok();
		}

		if (curtok() != T_SEMICOLON) throw;
		nexttok();

		break;
	case T_EXPORT:
		nexttok();

		if (curtok() != T_ID) throw;
		mdl.export_list.push_back(curtext());
		nexttok();

		while (curtok() == T_COMMA) {
			nexttok();

			if (curtok() != T_ID) throw;
			mdl.export_list.push_back(curtext());
			nexttok();
		}

		if (curtok() != T_SEMICOLON) throw;
		nexttok();
		break;
	case T_BEGIN:
		nexttok();

		if (curtok() != T_ID) throw;
		mdl.main_fn = curtext();
		nexttok();

		if (curtok() != T_SEMICOLON) throw;
		nexttok();
		
		break;
	default: throw;
	}
}

void _parser::parse_variable_declaration(_vardecl& decl)
{
	/*
<declaration>  := <identifier> ':' <type-specifier> ';'
				| <identifier> ':' <type-specifier> '=' <initializer> ';'
				| <identifier> '::' <initializer> ';'
				| <identifier> ':=' <initializer> ';'

	*/
	
	if (curtok() != T_ID) throw _parser_error(__FILE__, __LINE__, "declaration doesn't start with identifier, instead got: ", curtok()); // parser error
	decl.lhs.id = curtext();
	nexttok();

	switch (curtok()) {
	case T_COLON:
		decl.op = curtok();
		nexttok(); // eat ':'
		parse_type_specifier(decl.lhs.tspec);

		if (curtok() == T_EQ) {
			decl.op = T_COLON_EQ;
			nexttok();
			parse_initializer(decl);
		} 

		if (curtok() != T_SEMICOLON)
			throw _parser_error(__FILE__, __LINE__,  "declaration doesn't end with ';'. instead got: ", curtok());
		else nexttok(); // eat ';'

		break;
	case T_COLON_EQ: case T_COLON_COLON:
		decl.op = curtok();
		nexttok(); // eat ':=' | '::'

		parse_initializer(decl);

		if (curtok() != T_SEMICOLON) 
			throw _parser_error(__FILE__, __LINE__,  "declaration doesn't end with ';'. instead got: ", curtok());
		else nexttok();

		break;
	default: throw _parser_error(__FILE__, __LINE__, "declaration creation requires; ':', ':=', '::'. instead got: ", curtok());
	}
}

void _parser::parse_function_definition(_fndecl& fn)
{
	// 'fn' <identifier> '::' <lambda-definition>
	if (curtok() != T_FN) throw _parser_error(__FILE__, __LINE__, "invalid function definition, leading 'fn' missing, instead got: ", curtok());
	nexttok();

	if (curtok() != T_ID) throw _parser_error(__FILE__, __LINE__, "invalid function definition, missing identifier, instead got: ", curtok());
	fn.id = curtext();
	nexttok();

	if (curtok() != T_COLON_COLON) throw _parser_error(__FILE__, __LINE__, "invalid function definition, missing '::', instead got: ", curtok());
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

	if (curtok() != T_ARROW) throw _parser_error(__FILE__, __LINE__, "argument list not followed by '->'. instead got: ", curtok());
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
	if (curtok() != T_LPAREN) throw _parser_error(__FILE__, __LINE__, "invalid argument list, missing '(', instead got: ", curtok());
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

	if (curtok() != T_RPAREN) throw _parser_error(__FILE__, __LINE__, "invalid argument list, missing ')', instead got: ", curtok());
	nexttok();
}

void _parser::parse_arg(_arg& arg)
{
	/*
	<arg> := <identifier> ':' <type-specifier>
	*/
	if (speculate_type_annotated_arg()) {
		arg.id = curtext();
		nexttok();

		if (curtok() == T_COLON) {
			nexttok();
			parse_type_specifier(arg.tspec);
		}
		else {
			throw; // TODO: PARSER ERROR
		}
	}
	else {
		throw; // TODO: PARSER ERROR
	}
}

void _parser::parse_return_list(vector<_arg>& rargs)
{
	//<return-list> :='(' (<type-specifier> (',' <type-specifier>)*)? ')'
	
	if (curtok() != T_LPAREN) throw _parser_error(__FILE__, __LINE__, "invalid return list, missing '(', instead got: ", curtok());
	nexttok(); // eat '('

	_arg rarg;

	if (curtok() != T_RPAREN) {
		parse_type_specifier(rarg.tspec); // eat <type-specifier>
		rargs.push_back(rarg);
		while (curtok() == T_COMMA) {
			nexttok(); // eat ','
			parse_type_specifier(rarg.tspec);
			rargs.push_back(rarg);
		}
	}

	if (curtok() != T_RPAREN) throw _parser_error(__FILE__, __LINE__, "invalid return list, missing ')', instead got: ", curtok());
	nexttok(); // eat ')'
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
	_named_member_access* ma;
	_positional_member_access* aa;
	switch (curtok()) {
	case T_LPAREN: // '(' predicts a function call 
		fn = _parse_function_call();
		return fn;

	case T_LBRACKET: // '[' predicts positional access (tuple, array, etc) 
		aa = _parse_positional_access();
		return aa;

	case T_PERIOD: // '.' predicts named member access (struct, module)
		ma = _parse_named_access();
		return ma;
	
	default: throw _parser_error(__FILE__, __LINE__, "unrecognized postop token, expected '(' | '.' | '[' got: ", curtok()); // parser error
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
							  | <tuple>
							  | '(' <expression> ')'
							  | <unop> <primary-expression>
	*/
	_var* var;
	_ast* expr;
	_unop* unop;
	_tuple* tuple;
	string literal;
	switch (curtok()) {
	case T_ID: // variable name
		var = new _var;
		var->id = curtext();
		nexttok(); // eat <id>
		while (is_postop(curtok()))
			var->postops.push_back(_parse_postop());
		return var;
	case T_LPAREN: // sub-expression
		nexttok(); // eat '('

		expr = _parse_expression(_parse_primary_expr(), 0);

		if (curtok() != T_RPAREN) throw _parser_error(__FILE__, __LINE__, "invalid primary expression, while parsing parenthised expression expected closing ')', instead got: ", curtok());
		nexttok(); // eat ')'
		return expr;
	case T_LBRACE: // '{' predicts a tuple
		tuple = new _tuple;
		parse_tuple(*tuple);
		return tuple;
		break;  
	case T_ADD: case T_SUB: case T_MULT: // prefix expression (unop)
	case T_LOG_AND: case T_LOG_NOT: case T_BIT_NOT:
		unop = new _unop;

		unop->op = curtok();
		nexttok();
		unop->rhs = _parse_primary_expr();

		return unop;
	case T_LITERAL_INT:
		literal = curtext();
		nexttok();
		return new _int(stoi(literal));
	case T_LITERAL_FLOAT:
		literal = curtext();
		nexttok();
		return new _float(stof(literal));
	case T_LITERAL_TEXT:
		literal = curtext();
		nexttok();
		return new _text(literal);
	case T_TRUE:
		nexttok();
		return new _bool(true);
	case T_FALSE:
		nexttok();
		return new _bool(false);
	/*
		hypothesis ->
			if we are parsing a bracketed or parenthized
			expression, and there is no expression 
			curtok() will be ')' or ']'. meaning we can return
			a null value. 

		this might break the compiler...
	*/
	case T_RPAREN:
		return nullptr;
	case T_RBRACKET:
		return nullptr;
	case T_RBRACE:
		return nullptr;
	default: throw _parser_error(__FILE__, __LINE__, "invalid primary expression, expected: identifier or unary expression or parenthised expression or literal, instead got: ", curtok());
	}
}

_fcall* _parser::_parse_function_call()
{
	// '(' (<carg> (',' <carg>)*)? ')'

	if (curtok() != T_LPAREN) throw _parser_error(__FILE__, __LINE__, "invalid function call, expected '(', instead got: ", curtok());
	nexttok(); // eat '('

	auto fn = new _fcall;

	if (curtok() != T_RPAREN) { // empty arg list is semantically valid
		_arg carg;
		parse_carg(carg); // eat <arg>
		fn->argument_list.push_back(carg);

		while (curtok() == T_COMMA) {
			nexttok(); // eat ','
			parse_carg(carg); // eat <arg>
			fn->argument_list.push_back(carg);
		}
	}

	if (curtok() != T_RPAREN) throw _parser_error(__FILE__, __LINE__, "invalid function call, expected ')', instead got: ", curtok());
	nexttok(); // eat ')'

	return fn;
}

void _parser::parse_carg(_arg& carg)
{
	// <carg> := <expr> | <lambda-definition> 
	if (speculate_expression_and_unwind()) {
		carg.tspec.type = _DEDUCE;
		carg.tspec.expr = parse_expression();
	}
	else {
		auto lam = new _lambda;
		parse_lambda(*lam);
		carg.tspec.type = _LAMBDA;
		carg.tspec.expr = lam;
	}
}

_named_member_access* _parser::_parse_named_access()
{

	// <member-access> = '.' <identifier>
	if (curtok() != T_PERIOD) throw;
	nexttok();

	if (curtok() != T_ID) throw;
	auto memb = new _named_member_access;
	memb->member_id = curtext();

	nexttok();

	return memb;
}

_positional_member_access* _parser::_parse_positional_access()
{
	// '[' <expr> ']'
	_positional_member_access* aa;

	if (curtok() != T_LBRACKET) throw;
	nexttok();

	aa = new _positional_member_access;
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
		if (curtok() != T_LPAREN) throw _parser_error(__FILE__, __LINE__, "invalid if, expected '(', instead got: ", curtok());
		nexttok();
		conditional.cond = parse_expression();
		if (curtok() != T_RPAREN)  throw _parser_error(__FILE__, __LINE__, "invalid if, expected ')', instead got: ", curtok());
		nexttok();
		conditional.then = parse_statement();
		if (curtok() == T_ELSE) {
			nexttok();
			conditional.els = parse_statement();
		}
	}
	else  throw _parser_error(__FILE__, __LINE__, "invalid if, expected 'if', instead got: ", curtok());
}

void _parser::parse_iteration(_while& loop)
{
	if (curtok() == T_WHILE) {
		nexttok();
		if (curtok() != T_LPAREN)  throw _parser_error(__FILE__, __LINE__, "invalid while, expected '(', instead got: ", curtok());
		nexttok();
		loop.cond = parse_expression();
		if (curtok() != T_RPAREN) throw _parser_error(__FILE__, __LINE__, "invalid while, expected ')', instead got: ", curtok());;
		nexttok();
		loop.body = parse_statement();
	}
	else throw _parser_error(__FILE__, __LINE__, "invalid while, expected 'while', instead got: ", curtok());;
}

void _parser::parse_iteration(_dowhile& loop)
{
	if (curtok() == T_DO) {
		nexttok();
		loop.body = parse_statement();
		if (curtok() != T_WHILE) throw _parser_error(__FILE__, __LINE__, "invalid do while, expected 'while', instead got: ", curtok());;
		nexttok();
		if (curtok() != T_LPAREN) throw _parser_error(__FILE__, __LINE__, "invalid do while, expected '(', instead got: ", curtok());;
		nexttok();
		loop.cond = parse_expression();
		if (curtok() != T_RPAREN) throw _parser_error(__FILE__, __LINE__, "invalid do while, expected ')', instead got: ", curtok());;
		nexttok();
	}
	else throw _parser_error(__FILE__, __LINE__, "invalid do while, expected 'do', instead got: ", curtok());;
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

void _parser::parse_type_specifier(_type_specifier& tspec)
{
	_lambda* l;
	_array* a;
	_tuple* t;
	switch (curtok()) {
	case T_ID:
		tspec.type = _DEDUCE;
		tspec.name = curtext();
		nexttok();
		break;
	case T_INT:
		tspec.type = _INT;
		nexttok();
		break;
	case T_FLOAT:
		tspec.type = _FLOAT;
		nexttok();
		break;
	case T_TEXT:
		tspec.type = _TEXT;
		nexttok();
		break;
	case T_BOOL:
		tspec.type = _BOOL;
		nexttok();
		break;
	case T_LPAREN:
		tspec.type = _LAMBDA;
		l = new _lambda;
		parse_lambda_header(*l);
		tspec.expr = l;
		break;
	case T_LBRACE:
		tspec.type = _ARRAY;
		a = new _array;
		parse_array_type(*a);
		tspec.expr = a;
		break;
	case T_LBRACKET:
		tspec.type = _TUPLE;
		t = new _tuple;
		parse_type_tuple(*t);
		tspec.expr = t;
		break;
	default: throw;
	}
}

_ast* _parser::parse_initializer()
{
	/*
		<initializer>  := <lambda-definition>
						| <expression>
		*/
	if (speculate_lambda_and_unwind()) {
		auto lambda = new _lambda;
		parse_lambda(*lambda);
		return lambda;
	}
	else {
		return parse_expression();
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
		decl.rhs = lambda;
		if (decl.lhs.tspec.type == _ERR)
			decl.lhs.tspec.type = _LAMBDA;
	}
	else {
		decl.rhs = parse_expression();
		if (decl.lhs.tspec.type == _ERR)
			decl.lhs.tspec.type = _DEDUCE;
	}
}

void _parser::parse_record(_record& stct)
{
	if (curtok() != T_RECORD) throw;
	nexttok();

	if (curtok() != T_ID) throw;
	stct.id = curtext();
	nexttok();

	if (curtok() != T_COLON_COLON) throw;
	nexttok();

	parse_record_block(stct);
}

void _parser::parse_record_block(_record& stct)
{
	// '{' (<identifier> ':' <type-specifier> ( ';')* '}'
	if (curtok() != T_LBRACE) throw;
	nexttok();

	while (curtok() == T_ID) {
		parse_record_member(stct);
	}

	if (curtok() != T_RBRACE) throw;
	nexttok();
}


void _parser::parse_record_member(_record& stct)
{
	_member mem;
	// <identifier> ':' <type-specifier> ('=' <initializer>)? ';'
	if (curtok() != T_ID) throw;
	mem.id = curtext();
	nexttok();

	if (curtok() != T_COLON) throw;
	nexttok();

	parse_type_specifier(mem.tspec);

	if (curtok() == T_EQ) {
		nexttok();
		mem.initializer = parse_initializer();
	}

	if (curtok() != T_SEMICOLON) throw;
	nexttok();
	
	stct.members.push_back(mem);
}

void _parser::parse_tuple(_tuple& tpl)
{
	_member mem;

	// '{' <tuple-member> (';' <tuple-member>)* '}'
	if (curtok() != T_LBRACE) throw;
	nexttok();

	mem.tspec.type  = _DEDUCE;
	mem.initializer = parse_initializer();
	tpl.members.push_back(mem);

	while (curtok() == T_SEMICOLON) {
		nexttok();

		mem.tspec.type  = _DEDUCE;
		mem.initializer = parse_initializer();
		tpl.members.push_back(mem);
	}

	if (curtok() != T_RBRACE) throw;
	nexttok();
}

void _parser::parse_type_tuple(_tuple& tpl)
{
	_member mem;

	/*
	<type-tuple> := '{' <type-tuple-member> (';' <type-tuple-member>)* '}'
	*/
	if (curtok() != T_LBRACE) throw;
	nexttok();

	parse_type_specifier(mem.tspec);
	tpl.members.push_back(mem);

	while (curtok() == T_SEMICOLON) {
		nexttok();

		parse_type_specifier(mem.tspec);
		tpl.members.push_back(mem);
	}

	if (curtok() != T_RBRACE) throw;
	nexttok();
}

void _parser::parse_array_type(_array& arr)
{
	// '[' (<expr>)? ']' <type-specifier>
	if (curtok() != T_LBRACKET) throw;
	nexttok();

	arr.length_expr = parse_expression();

	if (curtok() != T_RBRACKET) throw;
	nexttok();

	parse_type_specifier(arr.tspec);
}

void _parser::parse_block(_scope& body)
{
	/* <block> :=
			'{' (<declaration> | <statement>)* '}'
	*/
	if (curtok() != T_LBRACE) throw _parser_error(__FILE__, __LINE__, "invalid block, expected '{', instead got: ", curtok());
	nexttok();

	_vardecl d;
	_ast* s = nullptr;
	int n = 0, m = 0;

	do {
		if (speculate_declaration_and_unwind()) {
			parse_variable_declaration(d);
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
			if (m > 1) throw _parser_error(__FILE__, __LINE__, "invalid block, expected: declaration or statement, instead got: ", curtok());
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
		if (speculate_expression()) {}

		if (speculate(T_SEMICOLON));
		else success = false;
	}
	else success = false;
	return success;
}

bool _parser::speculate_id_expr()
{
	// <id> can be followed by <binop>, <postop>,  or ';'
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
	if (curtok() == T_RBRACKET)
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
	// <binop> can be followed by <unop>, <id>, '(', '{', or <literal>
	nexttok(); // consume <binop>
	if (is_unop(curtok()))
		return speculate_unop_expr();
	if (is_literal(curtok()))
		return speculate_literal_expr();
	if (curtok() == T_ID)
		return speculate_id_expr();
	if (curtok() == T_LPAREN)
		return speculate_lparen_expr();
	if (curtok() == T_RBRACE)
		return speculate_tuple_in_expr();

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_postop_expr()
{
	// <postop> can be followed by <postop>, <binop>, or ';'
	// consume <postop>
	if (speculate_argument_list());
	else if (speculate_positional_access());
	else if (speculate_named_access());
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

bool _parser::speculate_tuple_in_expr()
{
	// a tuple in an expression is predicted with a
	// '{' following a <binop>, or starting an expression.
	nexttok(); // eat '{'

	if (speculate_initializer()) {
		while (curtok() == T_COMMA)
			speculate_initializer();
	}

	if (curtok() == T_RBRACE) {
		nexttok(); // eat '}'
		// a tuple can be followed by <binop>, or could end
		// the expression
		if (is_binop(curtok()))
			return speculate_binop_expr();
		if (curtok() == T_RPAREN) // ) ends the current expression or subexpression.
			return speculate_rparen_expr(); 
		if (curtok() == T_RBRACKET) // ] ends a well formed expression
			return true;
		if (curtok() == T_SEMICOLON) // ; ends a well formed expression
			return true;
		return false;
	}
	else {
		while (parens.size() > 0) parens.pop();
		return false;
	}
}

bool _parser::speculate_positional_access()
{
	bool success = true;
	// '[' <expr> ']'
	if (speculate(T_LBRACKET)) {
		if (speculate_expression()) {
			if (speculate(T_RBRACKET)) {

			}
			else success = false;
		}
		else success = false;
	}
	else success = false;
	return success;
}

bool _parser::speculate_named_access()
{
	bool success = true;
	// '.' <id>
	if (speculate(T_PERIOD)) {
		if (speculate(T_ID)) {

		}
		else success = false;
	}
	else success = false;
	return success;
}

bool _parser::speculate_expression()
{
	// an expression is started by <id>, <literal>, '{', <unop>, '('
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
	if (curtok() == T_LBRACE)
		return speculate_tuple_in_expr();
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

bool _parser::speculate_type_specifier_and_unwind()
{
	bool success = true;
	mark();
	success = speculate_type_specifier();
	release();
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
