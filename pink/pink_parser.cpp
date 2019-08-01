#include <string>

#include "pink_parser.h"
#include "pink_lexer.h"

using std::string;

constexpr auto k = 5;

token tokbuf[k];
int p = 0;

void consume_token() {
	tokbuf[p] = (token)gettok();
	p = (p + 1) % k;
}

// <program> := (<top-level-declaration>)* EOF

int parse_top_level() {
	int failed = 0;
	

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


