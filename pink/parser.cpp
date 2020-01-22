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

	ptable[T_EQUALS] = 3;
	ptable[T_NOT_EQUALS] = 3;

	ptable[T_LESS] = 4;
	ptable[T_GREATER] = 4;
	ptable[T_LESS_EQUALS] = 4;
	ptable[T_GREATER_EQUALS] = 4;

	ptable[T_OR] = 5;
	ptable[T_XOR] = 6;
	ptable[T_AND] = 7;

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
	if (t == T_NOT)		return true;
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
	if (t == T_OR)				return true;
	if (t == T_XOR)				return true;
	if (t == T_AND)				return true;
	if (t == T_EQUALS)			return true;
	if (t == T_NOT_EQUALS)		return true;
	if (t == T_LESS)			return true;
	if (t == T_LESS_EQUALS)		return true;
	if (t == T_GREATER)			return true;
	if (t == T_GREATER_EQUALS)	return true;
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
	if (t == T_LPAREN)	 return true;
	return false;
}

bool _parser::is_module_keyword(_token t)
{	
	if (curtok() == T_IMPORT) return true;
	if (curtok() == T_EXPORT) return true;
	if (curtok() == T_ROOT) return true;
	return false;
}

void _parser::parse_module_declaration(_module& mdl)
{
	bool threw = false;
	_vardecl* vardecl;
	_fn* fndecl;
	switch (curtok()) {
	case T_VAR: {
		vardecl = new _vardecl;
		parse_variable_declaration(*vardecl);
		mdl.module_scope.local_symbols.bind(*vardecl);
		break;
	}
	case T_FN: {
		fndecl = new _fn;
		parse_function_declaration(*fndecl, mdl.module_scope);
		try {
			mdl.functions.at(fndecl->id);
		}
		catch (...) {
			threw = true;
		}
		auto overload_set = mdl.functions[fndecl->id];

		if (threw) overload_set.insert(overload_set.begin(), (*fndecl));
		else throw _parser_error(__FILE__, __LINE__, "function already defined: ", fndecl->id);

		break;
	}

	default: throw _parser_error(__FILE__, __LINE__,  "invalid top level declaration: ", curtext());
	}
}

void _parser::parse_context_statement(_module& mdl)
{
	if (curtok() == T_ROOT) {
		nexttok();
		if (curtok() != T_ID) throw _parser_error(__FILE__, __LINE__, "invalid root : expected: identifier instead got:", curtok());
		mdl.root = curtext();
		nexttok();

		if (curtok() != T_SEMICOLON) throw _parser_error(__FILE__, __LINE__, "invalid root: expected ';' instead got:", curtok());
		nexttok();
	}
	else if (curtok() == T_IMPORT) {
		nexttok();
		if (curtok() != T_ID) throw _parser_error(__FILE__, __LINE__, "invalid import list: expected: identifier instead got:", curtok());
		mdl.import_list.push_back(curtext());
		nexttok();
		
		while (curtok() == T_COMMA) {
			nexttok();

			if (curtok() != T_ID) throw _parser_error(__FILE__, __LINE__, "invalid import list: expected: identifier instead got:", curtok());
			mdl.import_list.push_back(curtext());
			nexttok();
		}

		if (curtok() != T_SEMICOLON) throw _parser_error(__FILE__, __LINE__, "invalid import list: expected ';' instead got:", curtok());
		nexttok();
	}
	else if (curtok() == T_EXPORT) {
		nexttok();
		if (curtok() != T_ID) throw _parser_error(__FILE__, __LINE__, "invalid export list: expected: identifier instead got:", curtok());
		mdl.export_list.push_back(curtext());
		nexttok();

		while (curtok() == T_COMMA) {
			nexttok();

			if (curtok() != T_ID) throw _parser_error(__FILE__, __LINE__, "invalid export list: expected: identifier instead got:", curtok());
			mdl.export_list.push_back(curtext());
			nexttok();
		}

		if (curtok() != T_SEMICOLON) throw _parser_error(__FILE__, __LINE__, "invalid export list: expected ';' instead got:", curtok());
		nexttok();
	}
	else throw _parser_error(__FILE__, __LINE__, "invalid context statement: expected 'root' | 'import' | 'export' instead got:", curtok());
}

void _parser::parse_variable_declaration(_vardecl& decl)
{
	/*
<declaration>  := 'var' <identifier> ':' <type-specifier> ';'
				| 'var' <identifier> ':' <type-specifier> '=' <initializer> ';'
				| 'var' <identifier> '::' <initializer> ';'
				| 'var' <identifier> ':=' <initializer> ';'

	*/

	if (curtok() != T_VAR) throw;
	nexttok();
	
	if (curtok() != T_ID) throw _parser_error(__FILE__, __LINE__, "declaration doesn't start with identifier, instead got: ", curtok()); // parser error
	decl.lhs.id = curtext();
	nexttok();

	switch (curtok()) {
	case T_COLON:
		nexttok(); // eat ':'
		parse_type(decl.lhs.type);

		if (curtok() == T_EQ) {
			nexttok();
			parse_initializer(decl);
		} 

		if (curtok() != T_SEMICOLON)
			throw _parser_error(__FILE__, __LINE__,  "declaration doesn't end with ';'. instead got: ", curtok());
		else nexttok(); // eat ';'

		break;
	case T_COLON_EQ: case T_COLON_COLON:
		nexttok(); // eat ':=' | '::'

		parse_initializer(decl);

		if (curtok() != T_SEMICOLON) 
			throw _parser_error(__FILE__, __LINE__,  "declaration doesn't end with ';'. instead got: ", curtok());
		else nexttok();

		break;
	default: throw _parser_error(__FILE__, __LINE__, "declaration creation requires; ':', ':=', '::'. instead got: ", curtok());
	}
}

void _parser::parse_function_declaration(_fn& fn, _scope& local_scope)
{
	// 'fn' <identifier> <function-type> <function-body>
	if (curtok() != T_FN) throw _parser_error(__FILE__, __LINE__, "invalid function definition, leading 'fn' missing, instead got: ", curtok());
	nexttok();

	if (curtok() != T_ID) throw _parser_error(__FILE__, __LINE__, "invalid function definition, missing identifier, instead got: ", curtok());
	fn.id = curtext();
	nexttok();

	parse_function_type(fn);
	parse_function_body(fn, local_scope);
}

void _parser::parse_function_type(_fn& fn)
{
	parse_argument_list(fn.argument_list);

	if (curtok() != T_ARROW) throw _parser_error(__FILE__, __LINE__, "invalid function declaration, missing '->', instead got: ", curtok());
	nexttok();

	if (curtok() == T_LPAREN)
		parse_return_type(fn.return_type);
}

void _parser::parse_function_body(_fn& fn, _scope& local_scope)
{
	parse_scope(fn.body, local_scope);
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
	<arg> := <identifier> ':' <type>
	*/
	if (curtok() == T_ID) {
		arg.id = curtext();
		nexttok();

		if (curtok() == T_COLON) {
			nexttok();
			parse_type(arg.type);
		}
		else {
			throw _parser_error(__FILE__, __LINE__, "invalid function arg, missing ':', instead got: ", curtok());
		}
	}
	else {
		throw _parser_error(__FILE__, __LINE__, "invalid function arg, missing <identifier>, instead got: ", curtok());
	}
}

void _parser::parse_return_type(_type & var)
{
	//<return-type> :='(' <type> ')'
	
	if (curtok() != T_LPAREN) throw _parser_error(__FILE__, __LINE__, "invalid return list, missing '(', instead got: ", curtok());
	nexttok(); // eat '('

	if (curtok() != T_RPAREN) {
		parse_type(var); // eat <type-specifier>
	}

	if (curtok() != T_RPAREN) throw _parser_error(__FILE__, __LINE__, "invalid return list, missing ')', instead got: ", curtok());
	nexttok(); // eat ')'
}

void _parser::parse_expression(_expr& expr)
{
	expr.expr = _parse_expression(_parse_primary_expr(), 0);

	/*
	10/31/2019: comma separated expressions are hard to parse,
		and will not be used in v1, their primary reason
		for existence is tuples, and tuples aren't until v2.
	expr.expr_list.push_back(_parse_expression(_parse_primary_expr(), 0));
	
	while (curtok() == T_COMMA) {
		nexttok();
		expr.expr_list.push_back(_parse_expression(_parse_primary_expr(), 0));
	}
	
	*/
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
	switch (curtok()) {
	case T_LPAREN: // '(' predicts a function call 
		fn = _parse_function_call();
		return fn;
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
	string literal;
	switch (curtok()) {
	case T_ID: { // variable name
		auto id = curtext();
		nexttok();
		if (is_postop(curtok())) {
			auto fcall = _parse_function_call();
			fcall->id = id;
			if (is_postop(curtok())) throw _parser_error(__FILE__, __LINE__, "function chaining isn't supported in v1.");
			return fcall;
		}
		else {
			auto var = new _var;
			var->id = id;
			return var;
		}
	}
	case T_LPAREN: // sub-expression
		nexttok(); // eat '('

		expr = parse_expression();

		if (curtok() != T_RPAREN) throw _parser_error(__FILE__, __LINE__, "invalid primary expression, while parsing parenthised expression expected closing ')', instead got: ", curtok());
		nexttok(); // eat ')'
		return expr; 
	case T_ADD: case T_SUB: case T_MULT: // prefix expression (unop)
	case T_AND: case T_NOT: case T_BIT_NOT:
		unop = new _unop;

		unop->op = curtok();
		nexttok();
		// unops bind to their immediate rhs
		unop->rhs = _parse_primary_expr();

		return unop;
	case T_LITERAL_INT:
		literal = curtext();
		nexttok();
		return new _int(stoi(literal));
	case T_LITERAL_FLOAT:
		literal = curtext();
		nexttok();
		return new _real(stof(literal));
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
			if we are parsing a parenthized
			expression or a comma separated list 
			of expressions, and there is no expression 
			curtok() will be ')' or ',' . meaning we can return
			a null value. 

		this might break the compiler...
	*/
	case T_RPAREN:
		return nullptr;
	case T_COMMA:
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
	// <carg> := <expr>
	carg.type.name = "";
	carg.type.expr = parse_expression();
}

void _parser::parse_if(_if& conditional, _scope& local_scope)
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
		conditional.then = parse_statement(local_scope);
		if (curtok() == T_ELSE) {
			nexttok();
			conditional.els = parse_statement(local_scope);
		}
	}
	else  throw _parser_error(__FILE__, __LINE__, "invalid if, expected 'if', instead got: ", curtok());
}

void _parser::parse_while(_while& loop, _scope& local_scope)
{
	if (curtok() == T_WHILE) {
		nexttok();
		if (curtok() != T_LPAREN)  throw _parser_error(__FILE__, __LINE__, "invalid while, expected '(', instead got: ", curtok());
		nexttok();
		loop.cond = parse_expression();
		if (curtok() != T_RPAREN) throw _parser_error(__FILE__, __LINE__, "invalid while, expected ')', instead got: ", curtok());;
		nexttok();
		loop.body = parse_statement(local_scope);
	}
	else throw _parser_error(__FILE__, __LINE__, "invalid while, expected 'while', instead got: ", curtok());;
}

_ast* _parser::parse_statement(_scope& local_scope)
{
	if (curtok() == T_LBRACE) {
		auto block = new _scope;
		parse_scope(*block, local_scope);
		return block;
	}
	else if (curtok() == T_IF) {
		auto cond = new _if;
		parse_if(*cond, local_scope);
		return cond;
	}
	else if (curtok() == T_WHILE) {
		auto loop = new _while;
		parse_while(*loop, local_scope);
		return loop;
	}
	else if (curtok() == T_RETURN) {
		nexttok();

		auto ret = new _return;
		ret->expr = parse_expression();
		
		if (curtok() != T_SEMICOLON) throw _parser_error(__FILE__, __LINE__, "invalid return statement: missing ';' instead got: ", curtok());
		nexttok();

		return ret;
	}
	else { // the only other choice is an expression
		auto expr = new _expr;

		parse_expression(*expr);
		
		if (curtok() != T_SEMICOLON) throw _parser_error(__FILE__, __LINE__, "invalid statement: missing ';' instead got: ", curtok());
		nexttok();

		return expr;
	}
}

void _parser::parse_initializer(_vardecl& decl)
{
	/*
	<initializer>  := <lambda-definition>
					| <expression>
	*/
	decl.init = parse_expression();
}

void _parser::parse_type(_type& t)
{
	switch (curtok()) {
	case T_INT:
		t.name = "int";
		break;
	case T_REAL:
		t.name = "real";
		break;
	case T_TEXT:
		t.name = "text";
		break;
	case T_BOOL:
		t.name = "bool";
		break;
	default: throw _parser_error(__FILE__,__LINE__, "invalid type: expected 'int' | 'real' | 'text' | 'bool' instead got: ", curtok());
	}
	nexttok();
}

void _parser::parse_scope(_scope& local_scope, _scope outer_scope)
{
	/* <block> :=
			'{' (<declaration> | <statement>)* '}'
	*/
	if (curtok() != T_LBRACE) throw _parser_error(__FILE__, __LINE__, "invalid block, expected '{', instead got: ", curtok());
	nexttok();

	_vardecl* d = nullptr;
	_ast* s = nullptr;
	short n = 0, m = 0;

	/*
	construct the scopes with name shadowing in mind.
	when a new scope is created, pass in the old scope.
	then when a variable decl is parsed that shares its name
	with a previous decl we overwrite the old decl in the
	local scope. since each scope has it's own decl table for locals,
	when we try to resolve names and search the local scope before
	the outer scope, by definition the algorithm shadows the name.
	
	note that "outer scope" refers to the next scope upwards which
	is dependant on the parse tree. if we are parsing a module then the outer scope
	is the global scope and if we are parsing a function body
	then the outer scope is initially the module scope. however
	the language as the grammar has been written supports nesting
	scopes as much as you want within functions.
	this is untimately why we coalesce visible symbols.
	recursively, each scope will build up with the full definition
	of visible symbols, and through shadowing will be able to redeclare
	a symbols name to a new type which is really only a syntactic
	convienence for programmers.
	*/
	for (auto&& bucket : outer_scope.local_symbols)
		for (auto&& decl : bucket)
			local_scope.local_symbols[decl.second.lhs.id] = decl.second;

	do {
		if (speculate_declaration()) {
			d = new _vardecl;
			parse_variable_declaration(*d);
			// rebind overwrites any previous bindings
			// of the symbol in the local_scope, instead
			// of throwing an error. this slight variation
			// is what allows name shadowing to work lexically.
			local_scope.local_symbols.rebind(*d);
			n++;
		}
		else {
			s = parse_statement(local_scope);
			local_scope.statements.push_back(s);
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

	// unbind the names that are declared only in the outer scope.
	// keeping bound the names which will be bound during the lifetime
	// of this scope.
	// this is to save storage of names, the only names a current scope needs
	// to know are the names who fall out of scope when this scope ends.
	// this set is the set of variables which were declared in this scope.
	// this has the added benefiet of making lifetime management of
	// local variables very straightforward.
	vector<_vardecl> will_outlive;
	for (auto&& bucket : local_scope.local_symbols)
		for (auto&& decl : bucket) {
			// if the name appears in the outer_scope.local_symbols then we know
			// it is visible after the end of the current scope. if the
			// name appears only in the local_scope.local_symbols then
			// it was declared in the current scope and we don't remove it.
			// if the name appears in both the current scope and
			// the outer_scope then the name was shadowed by a declaration
			// in the current scope. we only want to remove the declarations
			// from the current_scope that will outlive the current scope.
			for (auto buket : outer_scope.local_symbols)
				for (auto dec : buket) {
					if (dec.first == decl.first)
						will_outlive.push_back(dec.second);
				}
		}

	local_scope.local_symbols.unbind(will_outlive);
}

void _parser::parse_return(_return& ret)
{
	if (curtok() != T_RETURN) throw _parser_error(__FILE__, __LINE__, "invalid return, doesn't begin with 'return', instead parsed: ", curtok());
	nexttok();

	ret.expr = parse_expression();

	if (curtok() != T_SEMICOLON) throw _parser_error(__FILE__, __LINE__, "invalid return, doesn't end with ';', instead parsed: ", curtok());
	nexttok();
}


bool _parser::speculate_declaration()
{
	bool success = true;
	mark();
	if (speculate(T_ID)) {
		if (speculate(T_COLON)) {
			if (speculate_type()) {
				if (speculate(T_EQ)) {
					if (speculate_initializer());
					else success = false;
				}
			}
			else success = false;

		}
		else if (speculate(T_COLON_COLON) || speculate(T_COLON_EQ)) {
			if (speculate_initializer()) {
				if (speculate(T_SEMICOLON));
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

bool _parser::speculate_type()
{
	bool success = true;
	if (speculate(T_INT));
	else if (speculate(T_REAL));
	else if (speculate(T_TEXT));
	else if (speculate(T_BOOL));
	else success = false;
	return success;	
}
bool _parser::speculate_initializer()
{
	return speculate_expression();
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
	if (curtok() == T_LPAREN) // sub expressions are valid
		return speculate_lparen_expr();
	if (curtok() == T_RPAREN) // empty expressions are valid
		return speculate_rparen_expr();
	if (curtok() == T_SEMICOLON && parens.size() == 0) // empty expressions are valid
		return true;
	if (curtok() == T_COMMA && parens.size() == 0)
		return true;
	return false;
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
	// update 10/28/2019: not in v1
	//if (curtok() == T_RBRACKET)
	//	return true;
	if (curtok() == T_COMMA && parens.size() == 0)
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
	if (curtok() == T_COMMA && parens.size() == 0)
		return true;
	if (curtok() == T_SEMICOLON && parens.size() == 0)
		return true;

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
	if (curtok() == T_COMMA && parens.size() == 0)
		return true;
	if (curtok() == T_SEMICOLON && parens.size() == 0)
		return true;
	if (curtok() == T_RPAREN) {
		return speculate_rparen_expr();
	}

	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_postop_expr()
{
	// <postop> can be followed by <postop>, <binop>, or ';'
	// consume <postop>
	if (speculate_fcall());
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
	if (curtok() == T_COMMA && parens.size() == 0)
		return true;
	if (curtok() == T_SEMICOLON && parens.size() == 0)
		return true;
	
	while (parens.size() > 0) parens.pop();
	return false;
}

bool _parser::speculate_fcall()
{
	//'('
	nexttok();

	// <carg> *(',' <carg>)
	if (speculate_carg()) {
		while (speculate(T_COMMA))
			if (speculate_carg());
			else return false;
	}

	// ')'
	if (curtok() != T_RPAREN) {
		while (parens.size() > 0) parens.pop();
		return false;
	}
	nexttok();
	return true;
}
bool _parser::speculate_carg()
{
	return speculate_expression();
}
//bool _parser::speculate_declaration()
//{
//	bool success = true;
//	if (speculate(T_ID)) {
//		if (speculate(T_COLON)) {
//			if (speculate_type_specifier()) {
//				if (speculate(T_SEMICOLON)) {
//
//				}
//				else if (speculate(T_EQ)) {
//					if (speculate_initializer()) {
//						if (speculate(T_SEMICOLON)) {
//
//						}
//						else success = false;
//					}
//					else success = false;
//				}
//				else success = false;
//			}
//			else success = false;
//		}
//		else if (speculate(T_COLON_EQ) || speculate(T_COLON_COLON)) {
//			if (speculate_initializer()) {
//				if (speculate(T_SEMICOLON)) {
//
//				}
//				else success = false;
//			}
//			else success = false;
//		}
//		else success = false;
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_declaration_and_unwind()
//{
//	bool success = true;
//	mark();
//	success = speculate_declaration();
//	release();
//	return success;
//}
//
//bool _parser::speculate_statement()
//{
//	bool success = true;
//	if (speculate_block()) {
//
//	}
//	else if (speculate_conditional()) {
//
//	}
//	else if (speculate_iteration()) {
//
//	}
//	else if (speculate_return()) {
//
//	}
//	else if (speculate_expression()) {
//
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_statement_and_unwind()
//{
//	bool success = true;
//	mark();
//	success = speculate_statement();
//	release();
//	return success;
//}
//
//bool _parser::speculate_block()
//{
//	bool success = true;
//	if (speculate(T_LBRACE)) {
//		while (speculate_declaration() || speculate_statement());
//
//		if (speculate(T_RBRACE)) {
//
//		}
//		else success = false;
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_iteration()
//{
//	/*
//		<iteration> := 'while' '(' <expression> ')' <statement>
//					 | 'do' <statement> 'while' '(' <expression> ')'
//			// TODO: | 'for' <identifier> 'in' <iterable> <statement>
//
//	*/
//	bool success = true;
//	if (speculate(T_WHILE)) {
//		if (speculate(T_LPAREN)) {
//			if (speculate_expression()) {
//				if (speculate(T_RPAREN)) {
//					if (speculate_statement()) {
//
//					}
//					else success = false;
//				}
//				else success = false;
//			}
//			else success = false;
//		}
//		else success = false;
//	}
//	else success = false;
//
//	return success;
//}
//
//bool _parser::speculate_conditional()
//{
//	/*
//		<conditional>  := 'if' '(' <expression> ')' <statement> ('else' <statement>)?
//				// TODO:| 'switch' '(' <expression> ')' <switch-block>
//	*/
//	bool success = true;
//	if (speculate(T_IF)) {
//		if (speculate(T_LPAREN)) {
//			if (speculate_expression()) {
//				if (speculate(T_RPAREN)) {
//					if (speculate_statement()) {
//						if (speculate(T_ELSE)) {
//							if (speculate_statement()) {
//
//							}
//							else success = false;
//						}
//					}
//					else success = false;
//				}
//				else success = false;
//			}
//			else success = false;
//		}
//		else success = false;
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_return()
//{
//	bool success = true;
//	if (speculate(T_RETURN)) {
//		if (speculate_expression()) {}
//
//		if (speculate(T_SEMICOLON));
//		else success = false;
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_id_expr()
//{
//	// <id> can be followed by <binop>, <postop>,  or ';'
//	nexttok(); // consume <id>
//	if (is_binop(curtok()))
//		return speculate_binop_expr();
//	if (is_postop(curtok()))
//		return speculate_postop_expr();
//	if (curtok() == T_RPAREN)
//		return speculate_rparen_expr();
//	// this may be questionable, but when <id> is followed by ']', ']' is assumed to be a terminal character 
//	// for the case of array (pointer) math:
//	//		A[B + C] = D;
//	if (curtok() == T_RBRACKET)
//		return true;
//	if (curtok() == T_SEMICOLON && parens.size() == 0)
//		return true;
//
//	while (parens.size() > 0) parens.pop();
//	return false;
//}
//
//bool _parser::speculate_literal_expr()
//{
//	// <literal> can be followed by <binop>, ')' or ';'
//	nexttok();
//	if (is_binop(curtok()))
//		return speculate_binop_expr();
//	if (curtok() == T_RPAREN)
//		return speculate_rparen_expr();
//	if (curtok() == T_SEMICOLON && parens.size() == 0)
//		return true;
//
//	while (parens.size() > 0) parens.pop();
//	return false;
//}
//
//bool _parser::speculate_unop_expr()
//{
//	// <unop> can be followed by <unop>, <id>, or <literal>
//	nexttok(); // consume <unop>
//	if (is_unop(curtok()))
//		return speculate_unop_expr();
//	if (is_literal(curtok()))
//		return speculate_literal_expr();
//	if (curtok() == T_ID)
//		return speculate_id_expr();
//	if (curtok() == T_LPAREN)
//		return speculate_lparen_expr();
//
//	while (parens.size() > 0) parens.pop();
//	return false;
//}
//
//bool _parser::speculate_binop_expr()
//{
//	// <binop> can be followed by <unop>, <id>, '(', '{', or <literal>
//	nexttok(); // consume <binop>
//	if (is_unop(curtok()))
//		return speculate_unop_expr();
//	if (is_literal(curtok()))
//		return speculate_literal_expr();
//	if (curtok() == T_ID)
//		return speculate_id_expr();
//	if (curtok() == T_LPAREN)
//		return speculate_lparen_expr();
//	if (curtok() == T_RBRACE)
//		return speculate_tuple_in_expr();
//
//	while (parens.size() > 0) parens.pop();
//	return false;
//}
//
//bool _parser::speculate_postop_expr()
//{
//	// <postop> can be followed by <postop>, <binop>, or ';'
//	// consume <postop>
//	if (speculate_argument_list());
//	else if (speculate_positional_access());
//	else if (speculate_named_access());
//	else { // malformed postop == malformed expression
//		while (parens.size() > 0) parens.pop();
//		return false;
//	}
//
//	if (is_postop(curtok()))
//		return speculate_postop_expr();
//	if (is_binop(curtok()))
//		return speculate_binop_expr();
//	if (curtok() == T_RPAREN)
//		return speculate_rparen_expr();
//	if (curtok() == T_SEMICOLON && parens.size() == 0)
//		return true;
//
//	while (parens.size() > 0) parens.pop();
//	return false;
//}
//
//bool _parser::speculate_lparen_expr()
//{
//	// '(' can be followed by <expr>
//	nexttok();
//	parens.push(0); // push an open paren on the stack that needs closing
//	return speculate_expression();
//}
//
//bool _parser::speculate_rparen_expr()
//{
//	// ')' can be followed by ')', <binop>, ';' or be the terminal character
//	if (parens.size() > 0)
//		parens.pop();
//	else if (parens.size() == 0) // if we don't have any open sub-expressions
//								 // we assume that the rparen is terminating the expression
//		return true;
//
//	nexttok();
//
//	if (is_binop(curtok()))
//		return speculate_binop_expr();
//	if (curtok() == T_SEMICOLON && parens.size() == 0)
//		return true;
//	if (curtok() == T_RPAREN) {
//		return speculate_rparen_expr();
//	}
//
//	while (parens.size() > 0) parens.pop();
//	return false;
//}
//
//bool _parser::speculate_tuple_in_expr()
//{
//	// a tuple in an expression is predicted with a
//	// '{' following a <binop>, or starting an expression.
//	nexttok(); // eat '{'
//
//	if (speculate_initializer()) {
//		while (curtok() == T_COMMA)
//			speculate_initializer();
//	}
//
//	if (curtok() == T_RBRACE) {
//		nexttok(); // eat '}'
//		// a tuple can be followed by <binop>, or could end
//		// the expression
//		if (is_binop(curtok()))
//			return speculate_binop_expr();
//		if (curtok() == T_RPAREN) // ) ends the current expression or subexpression.
//			return speculate_rparen_expr(); 
//		if (curtok() == T_RBRACKET) // ] ends a well formed expression
//			return true;
//		if (curtok() == T_SEMICOLON) // ; ends a well formed expression
//			return true;
//		return false;
//	}
//	else {
//		while (parens.size() > 0) parens.pop();
//		return false;
//	}
//}
//
//bool _parser::speculate_positional_access()
//{
//	bool success = true;
//	// '[' <expr> ']'
//	if (speculate(T_LBRACKET)) {
//		if (speculate_expression()) {
//			if (speculate(T_RBRACKET)) {
//
//			}
//			else success = false;
//		}
//		else success = false;
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_named_access()
//{
//	bool success = true;
//	// '.' <id>
//	if (speculate(T_PERIOD)) {
//		if (speculate(T_ID)) {
//
//		}
//		else success = false;
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_expression()
//{
//	// an expression is started by <id>, <literal>, '{', <unop>, '('
//	//  or immediately ended by ')' or ';'
//	if (curtok() == T_ID)
//		return speculate_id_expr();
//	if (is_literal(curtok()))
//		return speculate_literal_expr();
//	if (is_unop(curtok()))
//		return speculate_unop_expr();
//	if (curtok() == T_LPAREN)
//		return speculate_lparen_expr();
//	if (curtok() == T_RPAREN) // empty expressions are valid
//		return speculate_rparen_expr();
//	if (curtok() == T_LBRACE)
//		return speculate_tuple_in_expr();
//	if (curtok() == T_SEMICOLON && parens.size() == 0) // empty expressions are valid
//		return true;
//	return false;
//}
//
//bool _parser::speculate_expression_and_unwind()
//{
//	bool success = true;
//	mark();
//	success = speculate_expression();
//	release();
//	return success;
//}
//
//bool _parser::speculate_type_annotated_arg()
//{
//	bool success = true;
//	mark();
//	if (speculate(T_ID)) {
//		if (speculate(T_COLON)) {
//			if (speculate_type_specifier()) {
//
//			}
//			else success = false;
//		}
//	}
//	else success = false;
//	release();
//	return success;
//}
//
//bool _parser::speculate_initializer()
//{
//	bool success = true;
//	if (speculate_lambda());
//	else if (speculate_expression());
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_literal()
//{
//	bool success = true;
//	if (speculate(T_LITERAL_INT));
//	else if (speculate(T_LITERAL_FLOAT));
//	else if (speculate(T_LITERAL_TEXT));
//	else if (speculate(T_TRUE));
//	else if (speculate(T_FALSE));
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_type_specifier()
//{
//	/*
//		<type-specifier>	:= <identifier>
//							 | <type-primitive>
//							 | <lambda-header>
//	*/
//	bool success = true;
//	if (speculate(T_ID));
//	else if (speculate_literal());
//	else if (speculate_type_primitive());
//	else if (speculate_argument_list()) {
//		if (speculate(T_ARROW)) {
//			if (speculate_return_list());
//			else success = false;
//		}
//		else success = false;
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_type_specifier_and_unwind()
//{
//	bool success = true;
//	mark();
//	success = speculate_type_specifier();
//	release();
//	return success;
//}
//
//bool _parser::speculate_type_primitive()
//{
//	bool success = true;
//	if (speculate(T_INT));
//	else if (speculate(T_FLOAT));
//	else if (speculate(T_TEXT));
//	else if (speculate(T_BOOL));
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_lambda()
//{
//	bool success = true;
//	if (speculate_lambda_header())
//		if (speculate_lambda_body());
//		else success = false;
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_lambda_and_unwind()
//{
//	bool success = true;
//	mark();
//	if (speculate_lambda_header())
//		if (speculate_lambda_body());
//		else success = false;
//	else success = false;
//	this->release();
//	return success;
//}
//
//bool _parser::speculate_lambda_header()
//{
//	/*
//		<lambda-header> := <argument-list> '->' (<return-list>)?
//	*/
//	bool success = true;
//	if (speculate_argument_list()) {
//		if (speculate(T_ARROW)) {
//			if (speculate_return_list()) {
//
//			}
//		}
//		else success = false;
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_lambda_body()
//{
//	return speculate_block();
//}
//
//bool _parser::speculate_argument_list()
//{
//	bool success = true;
//	if (speculate(T_LPAREN)) {
//		if (speculate_arg()) {
//
//		}
//		while (speculate(T_COMMA)) {
//			if (speculate_arg()) {
//
//			}
//			else {
//				success = false;
//				break;
//			}
//		}
//		if (speculate(T_RPAREN)) {
//
//		}
//		else success = false;
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_return_list()
//{
//	/*
//		<return-list> := '(' <type-specifier> (',' <type-specifier>)*')'
//	*/
//	bool success = true;
//	if (speculate(T_LPAREN)) {
//		if (speculate_type_specifier()) {
//
//		}
//		while (speculate(T_COMMA)) {
//			if (speculate_type_specifier()) {
//
//			}
//			else {
//				success = false;
//				break;
//			}
//		}
//		if (speculate(T_RPAREN)) {
//
//		}
//		else success = false;
//	}
//	else success = false;
//	return success;
//}
//
//bool _parser::speculate_arg()
//{
//	/*
//		<arg> := <identifier> (':' <type-specifier>)?
//				| <expr>
//	*/
//	bool success = true;
//	if (speculate_expression_and_unwind()) {
//		speculate_expression();
//	}
//	else if (speculate(T_ID)) {
//		if (speculate(T_COLON)) {
//			if (speculate_type_specifier()) {
//
//			}
//			else success = false;
//		}
//	}
//	else success = false;
//	return success;
//}
