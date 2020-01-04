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
		are used in the representation, i.e. the word size of the
		current architecture. integers have a type
		within the language and, being a type primitive*, have
		many operations predefined. these being: '+', '-', '*',
		'/', '%', '==', '!=', '>', '<', '>=', '<=', '||', '&&',
		'^^', '!!', '|', '&', '^', '!', '='. all of these operations
		are either a binop or a unop. there will be a math library
		providing many supplemental functions like pow, and
		sqrt, and such.

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
		programmers a versatile, compositional way to
		talk about the flow of a program.

		fn add(x: int, y: int) -> (int) {
			return x + y;
		}

		functions encapsulate behavior. each call to a 
		given function, a programmer can expect something
		to happen, maybe a value is calculated, or
		some state is tested.

		each function definition has 
		a list of arguments, 
		a list of return values, 
		and a scope.
		
		functions in the future will be overloadable.
		sometimes the same operation is valid 
		upon a set of types, but will be implemented
		differently depending upon the type. function
		overloading allows programmers to directly
		express this intent, and increases
		readability of the program as a result.

		functions can also be polymorphic, that is will work
		regardless of type. this is common with data-structures.
		because every type has a size, and data-structures are
		only concerned with that aspect of the type. a polymorphic
		function is interesting, because it acts more like a template
		for a function with the types replaced with some placeholder name,
		which names all occurances of the type in the function. then when
		a programmer tries to call that function passing some type,
		a function definition is generated for the executable that is
		the exact function that was specified, only the type is replaced.
		(and all that implies for the generated code, which is a lot)

		functions arguments should be able to have default values,
		and the calling convention should be such that arguments 
		with default values can be elided from the function call.
		with default values having to be specified from the leftmost
		argument to the rightmost argument.

		idea: type aspects. 
			what is an aspect of a type?
			an aspect defines a 'kind' of type. 
			
			any one argument  function that
			takes a value of the type in question and returns a bool.
			signaling true if the type 'has' that aspect, 
			and false if the type does not. these are compile time functions
			(a.k.a macros) which are evaluated as part of macro expansion
			and parametric polymorphism. they are used as compile time
			constraints to help facilitate programmers to write generic code
			without having to independantly develop some kind of constraint
			schema over some generic type (like you have with void *'s in c).
			
			how/why do type aspects help programmers write generic code?

			in summation, type aspects allow the programmer to control 
			what 'kind's of types they allow the macro/template to
			be constructed with.

			if that didn't quite click:

			basically i don't want programmers to have to come up with
			their own way of saying, "if the type passed was ~this~ type
			(in a tagged union), then execute ~this~"
			or to come up with some way of saying "this macro only makes
			sense if the type implements ~this~ semantics, or makes sense
			in ~this~ position semantically" 
			because it is so simple, and is a kind of boilerplate
			function construction.

			a straightforward aspect is arithmetic?(), if the type makes sense
			anywhere one would see a number, then arithmetic? returns true.
			one would expect that a type that ~is~ arithmetic has functions
			like '+', '-', '*', '/', and '%' defined (as well as '<', '>',
			'<=', '>=', '==', and '!!=').

			one could imagine more aspects like:

			assignable?(), ordered?(), totally-ordered?() iterable?(), callable?(),
			equality-comparable?(), function?(), action?(),

			what about defining new aspects? what tools does this give us?
			consider a primitive type, int. this type has the previous
			two aspects defined, its typeof is 'int' and it's sizeof is
			one machine word. but what other 'aspects' does it have? well, it is also
			arithmetic, meaning the symbols '+', '-', '*', and '/', make
			sense on values of this type (we can add, subtract, multiply
			and divide integers). another aspect we could consider is
			assignable, meaning the symbol '=' makes sense on values
			of this type (you can assign a new integer value to a variable
			of type int right?). these specifically could make sense as kernel aspects.
			but if a user wanted to define an aspect, they would have to
			define a function, (actions don't make sense because aspects
			shouldn't modify anything; see, making the distinction is enforcing
			some semantics already!). what could aspects be used for?
			in my mind they work best with polymorphism and macros. because
			in polymorphic functions and macros, you care less what the actual
			type is, and more about what you can do with the type. which is to say
			you care about aspects of the type. that is to
			say which functions are defined upon the type. a function that
			could conceivably exist that would go a long ways to defining
			new aspects would be a function like 
			 fn exists(expr) -> bool
			where if the expression does not resolve the function
			returns false, and if the expression resolves the function
			returns true.  the exists function asks, could I call this function?
			then one could define the aspect like
			 aspect assignable(v1: mytype, v2: mytype) -> bool {
				if (exists(v1 = v2)) return true;
				else return false;
			 }
			 (because binops are 'just' functions, as are unops and postops)
			 names can contain special chars. then we can unfiy binops over
			 the string '+' vs 'some-other-name' and registering something
			 with binop says "this name is allowed in the first set
			 of the binop production" unop adds a new name to the unop production,
			 and postop adds a pair of names to the postop production,

			 an aspect could be defined as a subset of the complete set of
			 defined functions in the program. a type that can be said to 
			 'have' said aspect is a type that implements or overloads
			 all functions in the set of the aspect.

			 so a new macro or function can restrict what types can be used
			 to instanciate them (via macro expansion or polymorphic instancing) by
			 what actions they take on their arguments. which is to say,
			 which aspects they use. 

		what is an operation? it is a function with an
		affix representation (pre-, in-, or post-, -fix).
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
		so that the user could utilize this grammatic form
		easily.

		idea 1: what c++ has
		// as part of a class definition
		-> classname operator "" (classname lhs, classname rhs) {
		 return memberfunc(lhs, rhs); // where then memberfunc can be called as a binop,
									  // or could be called by name directly.
		}

		in pink there will be no member functions, because classes
		won't exist.

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
		// this of course means construct and return a new vector
		// whose components are the sum of the 
		// components in the passed vectors.
			return a.x + b.x, a.y + b.y;
		}

		we can extend the user interface of vectors by
		overloading the '+' binop function, this allows
		users to add two vectors using the infix '+'
		we can then construct expressions like
		a = b + c;
		where a, b, c have type vector


		idea 2: an idea i just had
		int-add : binop(+) (x: int, y: int) -> (int) {
			return add(x, y); # 'add' is then implemented in assembly
		}
		where the : symbol predicts some as-of-yet-unnamed-thing that
		adds int-add to the overload set of the '+' function. to be called
		when we encounter int types on either side of the '+' symbol in
		an expression context.

		int negate : unop '-' (x: int) -> (int) {
			return sub(0, x); # sub is then implemented in assembly
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
		of the grammar.
	*/
	_ast* parse_statement();
	void parse_if(_if& conditional);
	void parse_while(_while& loop);
	void parse_scope(_scope& body);

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