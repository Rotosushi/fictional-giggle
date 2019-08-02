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
//							 | <type-declaration>
//							 | <variable-declaration>

// <context-declaration> := 'context' '::' <context-block>
// <type-declaration>    := 'alias' <identifier> '::' <type-specifier> ';'
//						  | ('struct' | 'union') (<identifier>)? '::' '{' (<declaration>)* '}'
//						  | 'enum' (<identifier>) ? '::' '{' <enumeration-list> '}'
// <variable-declaration> := <identifier> <assignment-op> <type-specifier> ';'

int parse_top_level() {
	int failed = 0;
	
	while (curtok != T_EOF) {
		// prime our input
		curtok = gettok();

		switch (curtok) {
			case T_CONTEXT: {
				parse_context(); // TODO:
				break;
			}
			case T_ALIAS: {
				parse_alias();   // TODO:
				break;
			}
			case T_STRUCT: {
				parse_struct();  // TODO:
				break;
			}
			case T_UNION: {
				parse_union();   // TODO:
				break;
			}
			case T_ENUM: {
				parse_enum();    // TODO:
				break;
			}
			case T_ID: {
				parse_id();
				break;
			}

		}
	}
	return failed;
}

void parse_context() {}

void parse_alias() {}

void parse_struct() {}

void parse_union() {}

void parse_enum() {}

void parse_id() {
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

	// if we parse an identifier, then we can be sure this is a variable declaration.
	// for this first pass at a parser, the language will support initialization via
	// literals only.
	// variables look like:
	// --- <identifier> (<assignment-operator <initializer>)? ';'
	//     <initializer> := <literal>
	//					  | <function>
	//					  | <identifier>

	// we now need to see what the next token is and make a decision.
	curtok = gettok();
	switch (curtok) {
	case T_ASSIGN_EQ:    case T_ASSIGN_COLON_EQ:
	case T_ASSIGN_COLON: case T_ASSIGN_COLON_COLON: {
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
			parse_function();
			break;
		}
		}
		break;
	}

	}
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


