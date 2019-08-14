#include <string>

#include "pink_parser.h"
#include "pink_lexer.h"
#include <list>

using std::string;
using std::list;

struct variable {
	variable(int type, string name) : type(type), name(name) {}
 
	int type;
	string name;
};

struct var_int : public variable {
	var_int(int type, string name, int value) : variable(type, name), value(value){}

	int value;
};

struct var_float : public variable {
	var_float(int type, string name, float value) : variable(type, name), value(value){}

	float value;
};

struct var_string : public variable {
	var_string(int type, string name, string value) : variable(type, name), value(value){}

	string value;
};

list <string> symbol_table;
list <variable> variable_table;
int curtok = T_ERR;
variable* curvar = nullptr;

//constexpr auto k = 5;
//
//token tokbuf[k];
//int p = 0;
//
//void consume_token() {
//	tokbuf[p] = (token)gettok();
//	p = (p + 1) % k;
//}
//
//void match(int i) {
//
//}

// <program> := (<top-level-declaration>)* EOF
// <top-level-declaration>  := <context-declaration>
//							 | <type-definition>
//							 | <declaration>
//
// <context-declaration> := 'context' '::' <context-block>
// <type-definition>     := 'alias' <identifier> '::' <type-specifier> ';'
//						  | ('struct' | 'union') (<identifier>)? '::' <composite-type-block>
//						  | 'enum' (<identifier>) ? '::' <enumeration-block>
//						  | 'fn' <identifier> '::' <lambda-definition>
//
// <declaration>         := <identifier> <assignment-statement>
// 
// <context-block> := '{' (<declaration>)* '}' // future: (<declaration> | <compiler-directive>)*
//
// <composite-type-block> := '{' (<declaration>)+ '}'
//
// <enumeration-block> := '{' <enumeration-list> '}'
//
// <enumeration-list>  := <enum> (',' <enum>)*
//
// <enum> := <identifier> (':' <numeric-literal>)?
//
// <lambda-definition> := <argument-list> (<return-list>)? <lambda-block>
//
// <argument-list> := '(' <arg> (',' <arg>)* ')'
//
// <arg> := <identifier> (':' <type-specifier>)?
//
// <return-list> := '->' <argument-list> 
//
// <lambda-block> := '{' (<declaration> | <statement>)* '}'
//
// <type-specifier> := <identifier>
//					 | <type-primitive>
//					 | <lambda-definition>
//					 | <literal>
//
// <assignment-statement>  := <variable-declaration>
//							| <variable-assignment>
//							| <statement>
//
// <variable-declaration>  := ':' (<compiler-directive>)* <type-specifier> ';'
//							| ':' (<compiler-directive>)* '=' <type-specifier> ';'
//							| ':' (<compiler-directive>)* ':' <type-specifier> ';'
//
// <variable-assignment>   := '=' (<compiler-directive>)* <type-specifier> ';'
//
// TODO: <statement>
// TODO: <compiler-directive>


bool match_top_level() {
	while (curtok != T_EOF) {
		// prime our input
		curtok = gettok();

		if (speculate_context())
			match_context();
		else if (speculate_alias())
			match_alias();
		else if (speculate_struct())
			match_struct();
		else if (speculate_union())
			match_union();
		else if (speculate_)

		switch (curtok) {
			case T_CONTEXT: {
				if (speculate_context()) // TODO:
					match_context();
				else return false;
				break;
			}
			case T_ALIAS: {
				if (speculate_alias())   // TODO:
					match_alias();
				else return false;
				break;
			}
			case T_STRUCT: {
				if (speculate_struct())  // TODO:
					match_struct();
				else return false;
				break;
			}
			case T_UNION: {
				if (speculate_union())   // TODO:
					match_union();
				else return false;
				break;
			}
			case T_ENUM: {
				if (speculate_enum())    // TODO:
					match_enum();
				else return false;
				break;
			}
			case T_ID: {
				if (speculate_id())
					match_id();
				else return false;
				break;
			}

		}
	}
	return true;
}

bool speculate_context() {}

void match_context()
{
}

bool speculate_alias() {}

void match_alias()
{
}

bool speculate_struct() {}

void match_struct()
{
}

bool speculate_union() {}

void match_union()
{
}

bool speculate_enum() {}

void match_enum()
{
}

bool speculate_id() {
	// add the identifier to the symbol table if it doesn't already exist.
	// aside: why not symbol_table.find(current) ??
	//	or even find current : symbol_table 
	//	essentially, refrencing the iterator object explicitly is tedious, and
	//	the machinery is simple enough that a more concise syntax would be helpful.
	//	"what is easy to do should be easy to say"
	auto it = std::find(symbol_table.begin(), symbol_table.end(), current);
	if (it == symbol_table.end()) {
		symbol_table.push_front(current);
	}

	auto id = current;

	// if we parse an identifier, then we can be sure this is a variable declaration.
	// for this first pass at a parser, the language will support initialization via
	// literals only.
	// variables look like:
	// --- <identifier> (<assignment-operator <initializer>)? ';'
	//     <initializer> := <literal>
	//					  | <lambda>
	//					  | <identifier>

	// we now need to see what the next token is and make a decision.
	curtok = gettok();
	switch (curtok) {
	case T_SEMICOLON:
		// 
		break;
	case T_ASSIGN_EQ:    case T_ASSIGN_COLON_EQ:
	case T_ASSIGN_COLON: case T_ASSIGN_COLON_COLON: {
		speculate_initializer();
		// we know this is an assignment operation, 
		// so now we need to look at what is being assigned.
		auto op = current;
		curtok = gettok();
		switch (curtok) { 
		case T_INT_LITERAL: 
			curvar = new variable(T_INT_LITERAL, )
		case T_FLOAT_LITERAL:
			auto val = atof(current.c_str());
		case T_STRING_LITERAL: // literal assignment
			auto val = current;
			break;
		case T_L_PAREN: { // function assignment
			speculate_function();
			break;
		}
		}
		break;
	}

	}
}

void match_id()
{
}

bool speculate_function()
{
	return false;
}

void match_function()
{
}

bool speculate_initializer() {}

void match_initializer()
{
}

bool match_token(token tok, token cur) {
	return tok == cur ? true : false;
}

//
//typedef struct {
//	int type;
//	void* value;
//} node;
//
//typedef struct {
//	char* name;
//	int argument_count;
//	int return_count;
//	void** argument_vector;
//	void** return_vector;
//	void* body; 
//} lambda;
//
//typedef struct {
//	char* name;
//	int length;
//} string;
//
//typedef struct {
//	void* lhs;
//	void* rhs;
//	int op;
//} operaton;
//
//typedef struct {
//	void* expression;
//	void* body;
//} if_conditional;
//
//typedef struct {
//	void* expression;
//	void* body;
//} while_loop;
//
//typedef struct {
//	void* expression;
//	void* body;
//} do_while_loop;
//
//typedef struct {
//	void* prefix_expression;
//	void* conditional_expression;
//	void* postfix_expression;
//	void* body;
//} for_loop;
//
//typedef struct {
//	void* body;
//} scope;


