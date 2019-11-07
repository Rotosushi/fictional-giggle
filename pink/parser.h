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

	/* Variable parsing 
		variables are a basic abstraction
		in any programming language. they give
		programmers a ergonomic, expressive way
		to talk about data\objects\atoms in
		the language. in other words, they 
		encapsulate state. variables *have* type.
		type encapsulates the set of valid operations (r: functions)
		on that given type.
		
		for instance, -> int
		int encapsulates a word of bytes, and treats
		that word as if it were an integer.
		the representation however is incomplete.
		since it occupies finite space it cannot 
		represent the full infinite series of
		integers. the set of representable integers is
		(commonly, in interval notation) [2^(64-1), 2^64]
		where 64 is derived from how many bits 
		are used in the representation. integers have a type
		within the language and, being a type primitive*, have
		many operations predefined. these being: '+', '-', '*',
		'/', '%', '==', '!=', '>', '<', '>=', '<=', '||', '&&',
		'^^', '!!', '|', '&', '^', '!', '='.
		

		*(it is further reinforced that int is a primitive type
		  dually because of its definition being contingent
		  upon the hardware, and of it's inclusion in every
		  programming language (even assembly))
		
	*/
	void parse_variable_declaration(_vardecl& decl);
	void parse_initializer(_vardecl& decl);
	void parse_type(_vardecl& decl);
	void parse_type(_var& var);
	void parse_type(_arg& arg);

	/* function parsing 
		functions are a basic abstraction
		in any programming language. they give 
		programmers a versatile, elegant way to
		talk about the flow of a program.

		fn add(x: int, y: int) -> (int) {
			return x + y;
		}

		functions encapsulate behavior. each call to a 
		given function, a programmer can expect something
		to happen, maybe a value is calculated, or
		some statefull effect is caused.

		each function definition has 
		a list of arguments, 
		a list of return values, 
		and a scope.
		
		functions in the future will be overloadable.
		sometimes the same operation is valid 
		upon a set of types, but will be implemented
		differently depending upon the type. function
		overloading allows programmers to more directly
		express their intent, and hopefully increases
		readability of the program as a result.

		sometimes a function is the same regardless of
		some aspect(s) of the type. we see this most clearly
		in data structures. the simplest of which is the
		array. an array is a composite structure which contains
		a number of contiguous objects of homogeneous type. the
		only thing that the array data structure need concern
		itself with is the size of the type it is to contain.
		every type has a size, it is essential, by the nature 
		of it's representation. so we need not even refrence
		it with syntax. a programmer should only be concerned
		with using the data structure, not maintaining it's integrity.
		therefore the programming language implements
		arrays. arrays, as they only care about the sizeof
		the type, can be more succinctly expressed polymorphically

		the type signature ( <*-type> ) is
		<array-type> := '[' <expression> ']' <type>

		these are the constraints upon it's construction:
		the type must have a sizeof().
		and the expression must resolve to something
		that behaves like an unsigned int.


		what is an operation? it is a function with an
		affix representation (pre-, in-, or post- fix).
		functions are sometimes so ubiquitous
		(r: used everytime you use the type)
		on a type that even a small name can make the
		function call format look clumsy.

		consider:

		assign(a, (add(x,mult(y,z)));

		vs.

		a = x + y * z;

		this special grammatic form, called an
		expression, is implemented by something called an
		operator precedence parser.

		it would be nice to have an easy way to define
		functions that defined new operation symbols,
		or overloaded existing operation symbols.

		idea 1: what c++ has
		// as part of a class definition
		-> classname operator+ (classname lhs, classname rhs) {
		 return memberfunc(lhs, rhs); // where the memberfunc can be called as a binop,
									  // or could be called by name directly.
		}

		in pink there will be no member functions. (there may be lambda's, and
		lambda's might be useful in a sum or product type, so there
		may inadvertently be a way to express something like members,
		but the intent is to have the function be something separate
		from the data that is passed.)

		so the data definition (encapsulated state) 
		of a new type, like say a cartesian vector, 
		would only be ->

		syntax 1:
		type vector :: (x: int + y: int)

		syntax 2:
		record vector :: {
			x: int;
			y: int;
		}

		and behavior would always be encapsulated in functions

		fn add (a: vector, b: vector) -> (vector) {
			return a.x + b.x, a.y + b.y;
		}

		we can extend the user interface of vectors by
		overloading the '+' binop function, this allows
		users to add two vectors using the infix '+'
		we can then construct expressions like
		a = b + c;
		where a, b, c have type vector


		idea 2: an idea i just had
		int add : binop '-' (x: int, y: int) -> (int) {
			return add(x, y); # 'add' is then implemented in assembly
		}

		int negate : unop '-' (x: int) -> (int) {

		}

	*/
	void parse_function_declaration(_fn& fn);
	void parse_function_type(_fn& fn);
	void parse_function_body(_fn& fn);
	void parse_argument_list(vector<_arg>& args);
	void parse_arg(_arg& arg);
	void parse_return_value(_var& var);
	
	/* statement parsing 
		statements are implemented
		with a basic multiplexer

		<statement> := <if>
					 | <while>
					 | <scope>
					 | <expression>

		by putting expressions last, speculation can
		be completely factored out of this segment 
		of the grammar. therefore this segment is LL(1)
	*/
	_ast* parse_statement();
	void parse_if(_if& conditional);
	void parse_while(_while& loop);
	void parse_scope(_scope& body);

	/* expression parsing
		expressions are their own sub-grammar within the
		parser. From my understanding an 'operator precedence
		parser' that parse_expression() implements, is a
		predicated parser. This is because it's parsing
		descisions, which ultimately affect the shape of
		the resulting parse tree, are affected by the
		'precedence' of each operator; a piece of information
		that is not a part of the input to the parser. 
		(lexically speaking)
	
	*/
	void parse_expression(_expr& expr);
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
	   A: 10/29/2019 -> so far, i don't see an argument for removing
			speculation from the parser it isn't used much, and performance
			isn't a concern for v1, v2, or v3.
			and from my understanding it may
			help with (or be required to properly implement) the CDL like
			language.
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