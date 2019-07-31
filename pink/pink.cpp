// pink.cpp : This file contains the 'main' function. Program execution begins and ends there.

#include <iostream>

/*
A program in pink is composed of declarations and statements
	Declarations:
		A declaration adds a new item to the current scope.
		This applies to
			function declarations
			variables
			unstructured scopes ( aka the unnamed scope { // } )

	Statements:
		A statement is something that has a side effect, either upon the state of a variable
			or the state of the program flow
		This applies to
			function calls
			operators (=, +, -, etc.)
			structured scopes (conditionals & loops)

Why make a new programming language?
	for fun?
	for profit?
	how about:
		There is a ton of unneccessary complexity in modern day languages
		c is in its verbosity (This is a result of being a small lang, 
			which is addmittedly a plus), pitfalls, and age.
		c++ fails in its verbosity (partly inherited, mostly added), 
			semantic overhead, and its plethora of features
		Dynamicly Typed and Garbage Collected languages fail
			in the computational overhead of garbage collection,
			the mental overhead of duck typing
			and the complexity of the languages implementation.
			if an advanced user wants to get in and tinker, they in theory can,
			but its always far more complex a design than should be required.

If these are the problems, then how is this language a solution?
	what are the design goals of pink?
		1) The language should have a small, focused feature set which can be learned
			quickly, and mastered slowly. Each feature should hold its own weight in the
			design space of the language. There shouldn't be 12 innate ways of doing one thing,
			the programming language should have a convienient built-in, with the ability for the 
			programmer to specialize should they have a unique set of constraints.

		2) The syntax of the language should make it easy to say what is easy to do.
			Taking a page out of the python school of thought, the language should be designed for
			_humans_ and not the computer. The syntax should help the programmer to understand what is happening
			not obfuscate even the simplest of ideas (looking at you c++, *cough* polymorphism)

		3) The syntax and the semantics of the language should help the programmer build
			the necessary abstractions for small and large programs in a way that is 
			computationally effecient for the machine, and cognitively efficient for the programmer.

		4) The language should help you with the hardest of problems! 
		~Multithreading & Parrellelism.
			This is something that will be built into the language, and will have low-level and high-level
			abstractions, plus the plumbing required to get between. 
		~Dynamic Memory management~.
			This is a major sticking point of the language, C did it 80% correctly, but the 20% they didn't do
			bit new programmers so much they wanted to do away with the problem entirely. hence, Garbage Collection.
			This is too much of a perfomance hit to be acceptable. We can build better, We deserve better.
		~Hardware interface~
			What major language gives you an easy way to inject native assembly into your program?
			c, c++, do have ways of interfacing. However asm("") is clumsy. What about nice interfaces around
			hardware devices like SPI, or USB? this is basically non-existant in a public way. C and Linux are the
			best about this. otherwise its up to the hardware vendor.
		~Interlanguage Interface~
			Should be able to talk to C

		5) The implementation of the language should make it easy to tinker with the specifics
			when you need to, but should have well thought out defaults that cover the most
			common use cases.

What is the Design Space of the language
	The C design space, plus a pinch of the functional and oop design spaces.

	feature set:
		operators:
			arithmetic: +, -, *, /, %
			bitwise: |, &, ^, !
			logical: ||, &&, ^^, !!
			programatic: ., (), *, !*, []

		intrinsic types:
			u8, u16, u32, u64,
			s8, s16, s32, s64,
			f32, f64,
			int,
			bool,
			char,
			string,
			enum,
			pointer,
			lambda

		composite types:
			struct,
			union,
			arrays

		scopes:
			lambdas (executable scope)
			condtionals (branching scope)
			loops (looping scope)
			the unnamed scope {} (this is for multiline statements)

		type aliasing & polymorphism

		out of order declaration and usage of symbols (Like python)

		type annotations are optional, but all typing is static

		reflection should be in the language. We should be able 
			to examine the types in a struct || union at runtime. We should be able
			to examine the code inside a function at runtime.

	syntax:
		
		scopes
			global: declared in the top level file, or exported as global from any file
			file: declared in the top level of the current file
			local: declared in a subscope (inside a function scope, or an unnamed scope)

			unnamed scope: {} This is a new scope without a name, program flow enters and then exits
				symbols declared in this scope are undefined after leaving, it works just like naked 
				scopes in c/c++

			loops:
				for, while, do .. while, are the same as c. foreach will derive iterators for walkable composite 
				objects.
				break, continue, will exist

			conditionals:
				if, else if, else 
				switch case:, case <opt1>, <opt2>, .. , <optn>; or case <opt1> .. <optn>
 
		
		variables
			a : b // a is a new variable of type b
			a = b // a was a variable and is being assigned the value of b
			a := b // a is a new variable whose type is deduced from b, and whose initial value is the value of b
			a : <type> = b // a is a new variable whose type is <type> and whose initial value is the value of b
			a :: b // a is a new constant whose type is derived from b and whose value is b

		functions
			f() // a call to function 'f'
			f() {} // a definition of function 'f'
			a = () {} // a was a lambda whose contents are that of the definiton provided, and whose type is deduced
			a := f // a is a new lambda whose type is 'f' and whose contents are the same as 'f'
			a := () {} // a is a new lambda whose type is deduced and whose body is the one provided
			a : f // a is a new lambda whose type is 'f', but who doesn't have a body yet; maybe an illegal statement?
			a :: f // a is a new constant lambda whose type is 'f', a will have the same body as f
*/

/*
	grammar:

	identifier = [a-zA-Z] ([0-9] | [a-zA-Z])* (_ | [0-9] | [a-zA-Z])*
	 // a string of chars, not beginning with a number, of arbitrary length

	declaration := 
		variable_definition ';' |
		function_definition ';'

	function_definition := return_value identifier '(' argument_list ')' '{' function_body '}'

	statement :=
		arithmetic_operation ';'	|
		logical_operation ';'		|
		bitwise_operation ';'		|
		intrinsic_operation ';'	|
		function_call ';'			|
		scope_statement ';'		|

	variable_definition :=
		identifier ;				| // create a new empty symbol with type None
		identifier : type_annotation ;	| // create a new empty symbol with type <type_annotation>
		identifier = initializer ;	| // fill an existing empty symbol with type derived from initializer 
									  // && filled with the contents of initializer
		identifier := initializer ;	| // create a new symbol with type derived from initializer 
									  // && filled with the contents of initializer
		identifeier : type_annotation = initializer ; | // create a new symbol with type <type_annotation>
													    // && filled with the contents of initializer
		identifier :: initializer ; // create a new constant symbol with type derived from initializer
									// && filled with the contents of initializer

	initializer := 
		identifier |
		function_definition |
		variable_definition |

*/

/*
	struct symbol = {
		String identifier; // the symbols name
		Type type; // any intrinsic type
		Void* value; // the space available for storing things
		int filename; // the file where the symbol declaration lives
		int line; // the line in the file where the symbol declaration lives
	}

	enum Type = {
		u8, u16, u32, u64,
		s8, s16, s32, s64,
		f32, f64,
		int,
		bool,
		char,
		string,
		enum,
		pointer,
		lambda,
		array,
		struct,
		union
	}

	struct Scope = {
		Context context; // Allocators, Runtime Info, Capture List
		SymbolTable symbols; // symbols visible to this scope
		ScopeType type; // are we a naked scope, loop, or conditional
		AST* body; // The body of executable code for this scope
	}
*/

/*
	Parsing

	Token := (<op1> | <op2> | .. | <opn> )
	-->
		if ( <<lookahead-predicts-op1>> ) { <<match-op1>> }
		else if ( <<lookahead-predicts-op2>> ) { <<match-op2>> }
		..
		else if ( <<lookahead-predicts-opn>> ) { <<match-opn>> }
		else <<unknown-token-error>> // no viable alternatives

	Token := (optional-opt)? (op1 | op2)
	-->
		if (<lookahead-predicts-optional-opt) { match-optional-opt }
		if (<lookahead-predicts-op1) {match-op1}
		else if (<lookahead-predicts-op2) {match-op2}
		else <unknown-token>

	Token := (one-or-more-opt)+ (op1 | op2)
	-->
		do { // ( .. )+
			<<code-matching-one-or-more-opts>>
		} while ( <<lookahead-predicts-an-alternative-of-the-one-or-more-opts>> )
		if (<lookahead-predicts-op1) {match-op1}
		else if (<lookahead-predicts-op2) {match-op2}
		else <unknown-token>

	Token := (zero-or-more-opts)* (opt1 | opt2)
	-->
		while (<<lookahead-predicts-an-alternative-of-the-zero-or-more-opts>> ) {
			<<code-matching-zero-or-more-opts>>
		}
		if (<lookahead-predicts-op1) {match-op1}
		else if (<lookahead-predicts-op2) {match-op2}
		else <unknown-token>
		
	top := (definition | expression)*

	definition := 
		variable_definition |
		function_definition 

	expression := 
		operation |
		function_call

	variable_definition :=
		identifier ; |
		identifier : type_expression ; |
		identifier : type_expression = initializer ; |
		identifier := initializer ;
*/

/*
	TODO: Write a lexer that can tokenize an input stream of chars

	Tokens:
		EOF  // TOKEN_EOF
		[a-zA-Z](_ | [a-zA-Z0-9])+   // TOKEN_IDENTIFIER
		(-)?[0-9](' | [0-9])+        // TOKEN_NUMBER
		0x([a-fA-F0-9])+             // TOKEN_HEX_NUMBER
		0b(0 | 1)+                   // TOKEN_BINARY_NUMBER
		0o([0-7])+                   // TOKEN_OCTAL_NUMBER
		+    // TOKEN_PLUS
		-    // TOKEN_MINUS
		*    // TOKEN_MULTIPLY
		/    // TOKEN_DIVIDE
		%    // TOKEN_MODULUS
		=    // TOKEN_ASSIGN
		!    // TOKEN_BITWISE_NOT
		&    // TOKEN_BITWISE_AND
		|    // TOKEN_BITWISE_OR
		^    // TOKEN_BITWISE_XOR
		{    // TOKEN_LEFT_BRACE
		}    // TOKEN_RIGHT_BRACE
		[    // TOKEN_LEFT_BRACKET
		]    // TOKEN_RIGHT_BRACKET
		(    // TOKEN_LEFT_PARENTHESIS
		)    // TOKEN_RIGHT_PARENTHESIS
		!!   // TOKEN_LOGICAL_NOT
		&&   // TOKEN_LOGICAL_AND
		||   // TOKEN_LOGICAL_OR
		^^   // TOKEN_LOGICAL_XOR
		==   // TOKEN_LOGICAL_EQUALITY
		u8   // TOKEN_UNSIGNED_8
		u16  // TOKEN_UNSIGNED_16
		u32  // TOKEN_UNSIGNED_32
		u64  // TOKEN_UNSIGNED_64
		s8   // TOKEN_SIGNED_8
		s16  // TOKEN_SIGNED_16
		s32  // TOKEN_SIGNED_32
		s64  // TOKEN_SIGNED_64
		f32  // TOKEN_FLOAT_32
		f64  // TOKEN_FLOAT_64
		int     // TOKEN_INT
		float   // TOKEN_FLOAT
		bool    // TOKEN_BOOL
		char    // TOKEN_CHAR
		string  // TOKEN_STRING
		enum    // TOKEN_ENUM
		pointer (s<x> *, u<x> *, f<x> *) // TOKEN_POINTER
		lambda  // TOKEN_LAMBDA
		array   // TOKEN_ARRAY
		struct  // TOKEN_STRUCT
		union   // TOKEN_UNION


*/

#include "pink_lexer.h"
#include <cassert>

void test_lexer() {
	int t;
	t = get_next_token();
	assert(current == "+");
	assert(t == T_ADD);
	current.clear();

	t = get_next_token();
	assert(current == "-");
	assert(t == T_SUB);
	current.clear();

	t = get_next_token();
	assert(current == "*");
	assert(t == T_MULT);
	current.clear();

	t = get_next_token();
	assert(current == "/");
	assert(t == T_DIV);
	current.clear();

	t = get_next_token();
	assert(current == "%");
	assert(t == T_MOD);
	current.clear();

	t = get_next_token();
	assert(current == "=");
	assert(t == T_ASSIGN_EQ);
	current.clear();

	t = get_next_token();
	assert(current == "==");
	assert(t == T_LOG_EQ);
	current.clear();

	t = get_next_token();
	assert(current == "!!=");
	assert(t == T_LOG_NEQ);
	current.clear();

	t = get_next_token();
	assert(current == ":");
	assert(t == T_ASSIGN_COLON);
	current.clear();

	t = get_next_token();
	assert(current == ":=");
	assert(t == T_ASSIGN_COLON_EQ);
	current.clear();

	t = get_next_token();
	assert(current == "::");
	assert(t == T_ASSIGN_COLON_COLON);
	current.clear();

	t = get_next_token();
	assert(current == "*=");
	assert(t == T_MULT_ASSIGN);
	current.clear();

	t = get_next_token();
	assert(current == "/=");
	assert(t == T_DIV_ASSIGN);
	current.clear();

	t = get_next_token();
	assert(current == "+=");
	assert(t == T_ADD_ASSIGN);
	current.clear();

	t = get_next_token();
	assert(current == "-=");
	assert(t == T_SUB_ASSIGN);
	current.clear();

	t = get_next_token();
	assert(current == "!");
	assert(t == T_BIT_NOT);
	current.clear();

	t = get_next_token();
	assert(current == "!!");
	assert(t == T_LOG_NOT);
	current.clear();

	t = get_next_token();
	assert(current == "^");
	assert(t == T_BIT_XOR);
	current.clear();

	t = get_next_token();
	assert(current == "^^");
	assert(t == T_LOG_XOR);
	current.clear();

	t = get_next_token();
	assert(current == "&");
	assert(t == T_BIT_AND);
	current.clear();

	t = get_next_token();
	assert(current == "&&");
	assert(t == T_LOG_AND);
	current.clear();

	t = get_next_token();
	assert(current == "[");
	assert(t == T_L_BRACE);
	current.clear();

	t = get_next_token();
	assert(current == "]");
	assert(t == T_R_BRACE);
	current.clear();

	t = get_next_token();
	assert(current == "{");
	assert(t == T_L_BRACKET);
	current.clear();

	t = get_next_token();
	assert(current == "}");
	assert(t == T_R_BRACKET);
	current.clear();

	t = get_next_token();
	assert(current == "(");
	assert(t == T_L_PAREN);
	current.clear();

	t = get_next_token();
	assert(current == ")");
	assert(t == T_R_PAREN);
	current.clear();

	t = get_next_token();
	assert(current == "<<");
	assert(t == T_BIT_LSHIFT);
	current.clear();

	t = get_next_token();
	assert(current == ">>");
	assert(t == T_BIT_RSHIFT);
	current.clear();

	t = get_next_token();
	assert(current == "<<=");
	assert(t == T_LSHIFT_ASSIGN);
	current.clear();

	t = get_next_token();
	assert(current == ">>=");
	assert(t == T_RSHIFT_ASSIGN);
	current.clear();

	t = get_next_token();
	assert(current == "<");
	assert(t == T_LOG_LESS);
	current.clear();

	t = get_next_token();
	assert(current == ">");
	assert(t == T_LOG_GREATER);
	current.clear();

	t = get_next_token();
	assert(current == "<=");
	assert(t == T_LOG_LEQ);
	current.clear();

	t = get_next_token();
	assert(current == ">=");
	assert(t == T_LOG_GEQ);
	current.clear();

	/*
	t = get_next_token();
	assert(current == "");
	assert(t == );
	current.clear();
	*/

	//

	t = get_next_token();
	assert(current == "hello");
	assert(t == T_ID);
	current.clear();

	t = get_next_token();
	assert(current == "for");
	assert(t == T_FOR);
	current.clear();

	t = get_next_token();
	assert(current == "while");
	assert(t == T_WHILE);
	current.clear();

	t = get_next_token();
	assert(current == "if");
	assert(t == T_IF);
	current.clear();

	t = get_next_token();
	assert(current == "else");
	assert(t == T_ELSE);
	current.clear();

	t = get_next_token();
	assert(current == "butts");
	assert(t == T_ID);
	current.clear();

	t = get_next_token();
	assert(current == "91");
	assert(t == T_INT_LITERAL);
	current.clear();

	t = get_next_token();
	assert(current == "9.1");
	assert(t == T_FLOAT_LITERAL);
	current.clear();

	t = get_next_token();
	assert(current == ".900");
	assert(t == T_FLOAT_LITERAL);
	current.clear();

	t = get_next_token();
	assert(current == "9.9.9");
	assert(t == T_ERR);
	current.clear();

	t = get_next_token();
	assert(current == "%%");
	assert(t == T_ERR);
	current.clear();

	t = get_next_token();
	assert(current == "\"Hello, World!\"");
	assert(t == T_STRING_LITERAL);
	current.clear();

	t = get_next_token();
	assert(current == "int");
	assert(t == T_INT);
	current.clear();

	t = get_next_token();
	assert(current == "float");
	assert(t == T_FLOAT);
	current.clear();

	t = get_next_token();
	assert(current == "bool");
	assert(t == T_BOOL);
	current.clear();

	t = get_next_token();
	assert(current == "true");
	assert(t == T_TRUE);
	current.clear();

	t = get_next_token();
	assert(current == "false");
	assert(t == T_FALSE);
	current.clear();

	t = get_next_token();
	assert(current == "string");
	assert(t == T_STRING);
	current.clear();

	t = get_next_token();
	assert(current == "struct");
	assert(t == T_STRUCT);
	current.clear();

	t = get_next_token();
	assert(current == "union");
	assert(t == T_UNION);
	current.clear();

	t = get_next_token();
	assert(current == "enum");
	assert(t == T_ENUM);
	current.clear();

	t = get_next_token();
	assert(current == "alias");
	assert(t == T_ALIAS);
	current.clear();

	t = get_next_token();
	assert(current == "typecast");
	assert(t == T_TYPECAST);
	current.clear();

	t = get_next_token();
	assert(current == "sizeof");
	assert(t == T_SIZEOF);
	current.clear();

	t = get_next_token();
	assert(current == "u8");
	assert(t == T_U8);
	current.clear();

	t = get_next_token();
	assert(current == "u16");
	assert(t == T_U16);
	current.clear();

	t = get_next_token();
	assert(current == "u32");
	assert(t == T_U32);
	current.clear();

	t = get_next_token();
	assert(current == "u64");
	assert(t == T_U64);
	current.clear();

	t = get_next_token();
	assert(current == "s8");
	assert(t == T_S8);
	current.clear();

	t = get_next_token();
	assert(current == "s16");
	assert(t == T_S16);
	current.clear();

	t = get_next_token();
	assert(current == "s32");
	assert(t == T_S32);
	current.clear();

	t = get_next_token();
	assert(current == "s64");
	assert(t == T_S64);
	current.clear();

	t = get_next_token();
	assert(current == "f32");
	assert(t == T_F32);
	current.clear();

	t = get_next_token();
	assert(current == "f64");
	assert(t == T_F64);
	current.clear();

	t = get_next_token();
	assert(current == "for");
	assert(t == T_FOR);
	current.clear();

	t = get_next_token();
	assert(current == "string");
	assert(t == T_STRING);
	current.clear();

	t = get_next_token();
	assert(current == "hello");
	assert(t == T_ID);
	current.clear();

	t = get_next_token();
	assert(current == "=");
	assert(t == T_ASSIGN_EQ);
	current.clear();

	t = get_next_token();
	assert(current == "\"Hello,\"");
	assert(t == T_STRING_LITERAL);
	current.clear();

	t = get_next_token();
	assert(current == "+");
	assert(t == T_ADD);
	current.clear();

	t = get_next_token();
	assert(current == "\"World!\"");
	assert(t == T_STRING_LITERAL);
	current.clear();

	t = get_next_token();
	assert(current == "EOF");
	assert(t == T_EOF);
	current.clear();
}

int main()
{
    std::cout << "Hello, Pink!\n";
	test_lexer();
	return 0;
}

