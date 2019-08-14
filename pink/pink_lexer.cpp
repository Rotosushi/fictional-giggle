#include <string>

using std::string;

#include "pink_lexer.h"

/*
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
*/

/* 
 top level parsing 
	<program> := (<header>)? (<declaration> | <statement>)*
	
	# TODO: <header> :=

	<declaration> :=  <variable-declaration>
					| <type-alias>

	<statement> := <expression-statement>
				 | <compound-statement>
				 | <conditional-statement>
				 | <iteration-statement>

*/



int isidentifier(int c) {
	if (isalnum(c) || c == '_' || c == '-') {
		return 1;
	}
	return 0;
}

int isoperator(int c) {
	switch (c) {
	case '=': case ':': case '*': case '/':
	case '%': case '+': case '-': case '<':
	case '>': case '!': case '^':
	case '|': case '&': case '[': case ']':
	case '{': case '}': case '(': case ')':
		return 1;
	default:
		return 0;
	}
}

string current;
int integer_value;
float float_value;

int gettok() {
	static int c = ' ';

	while (isspace(c)) { // ignore whitespace
		c = getchar();
	}

	if (c == EOF) {
		current = "EOF";
		return T_EOF;
	}

	if (isalpha(c)) { // could be an identifier, a type-primitive, or a keyword
		current += c;
		c = getchar();
		while (isidentifier(c)) { // consume the next word
			current += c;
			c = getchar();
		}

		if (current == "int")		return T_INT;
		if (current == "float")		return T_FLOAT;
		if (current == "string")	return T_STRING;
		if (current == "bool")		return T_BOOL;
		if (current == "maybe")		return T_MAYBE;
		if (current == "none")		return T_NONE;
		if (current == "true")		return T_TRUE;
		if (current == "false")		return T_FALSE;
		if (current == "if")		return T_IF;
		if (current == "else")		return T_ELSE;
		if (current == "do")		return T_DO;
		if (current == "while")		return T_WHILE;
		if (current == "for")		return T_FOR;
		if (current == "alias")		return T_ALIAS;
		if (current == "sizeof")	return T_SIZEOF;
		if (current == "typecast")	return T_TYPECAST;
		if (current == "struct")	return T_STRUCT;
		if (current == "union")		return T_UNION;
		if (current == "enum")		return T_ENUM;
		if (current == "context")	return T_CONTEXT;
		if (current == "fn")		return T_FUNCTION;
		if (current == "u8")		return T_U8;
		if (current == "u16")		return T_U16;
		if (current == "u32")		return T_U32;
		if (current == "u64")		return T_U64;
		if (current == "s8")		return T_S8;
		if (current == "s16")		return T_S16;
		if (current == "s32")		return T_S32;
		if (current == "s64")		return T_S64;
		if (current == "f32")		return T_F32;
		if (current == "f64")		return T_F64;
		// else it's an identifier
		return T_ID;
	}

	if (isdigit(c) || c == '.') { // could be an int or a float literal
		bool has_fractional = c == '.' ? true : false;

		current += c;
		c = getchar();

		while (isdigit(c) || c == '.') { // consume the whole number
			if (c == '.') has_fractional = true;
			current += c;
			c = getchar();

			if (c == '.' && has_fractional) {    // this is a malformed numer
				while (c != '\n' && c != '\r') { // grab the rest of the line
					current += c;
					c = getchar();
				}
				return T_ERR;
			}
		}

		if (has_fractional) {
			float_value = atof(current.c_str());
			return T_FLOAT_LITERAL;
		} else {
			integer_value = atof(current.c_str());
			return T_INT_LITERAL;
		}
	}

	if (c == '\'' || c == '\"') { // it's a string literal
		current += c;
		c = getchar();

		while (c != '\'' && c != '\"') {
			current += c;
			c = getchar();
			if (c == EOF) return T_ERR; // This is probably because of no ending ' or "
		}

		current += c; // store the trailing ' or "
		c = getchar(); // prime the next char
		return T_STRING_LITERAL;
	}

	if (isoperator(c)) { // it's operator or grouping symbol
		current += c;
		c = getchar();

		while (isoperator(c)) {
			current += c;
			c = getchar();
		}

		if (current == "=") return T_ASSIGN_EQ;
		if (current == ":") return T_ASSIGN_COLON;
		if (current == ":=") return T_ASSIGN_COLON_EQ;
		if (current == "::") return T_ASSIGN_COLON_COLON;
		if (current == "*=") return T_MULT_ASSIGN;
		if (current == "/=") return T_DIV_ASSIGN;
		if (current == "%=") return T_MOD_ASSIGN;
		if (current == "+=") return T_ADD_ASSIGN;
		if (current == "-=") return T_SUB_ASSIGN;
		if (current == "&=") return T_BIT_AND_ASSIGN;
		if (current == "^=") return T_BIT_XOR_ASSIGN;
		if (current == "|=") return T_BIT_OR_ASSIGN;
		if (current == "<<=") return T_LSHIFT_ASSIGN;
		if (current == ">>=") return T_RSHIFT_ASSIGN;
		if (current == "+") return T_ADD;
		if (current == "-") return T_SUB;
		if (current == "*") return T_MULT;
		if (current == "/") return T_DIV;
		if (current == "%") return T_MOD;
		if (current == "&") return T_BIT_AND;
		if (current == "|") return T_BIT_OR;
		if (current == "^") return T_BIT_XOR;
		if (current == "!") return T_BIT_NOT;
		if (current == "!!") return T_LOG_NOT;
		if (current == "&&") return T_LOG_AND;
		if (current == "||") return T_LOG_OR;
		if (current == "^^") return T_LOG_XOR;
		if (current == "==") return T_LOG_EQ;
		if (current == "!!=") return T_LOG_NEQ;
		if (current == "<") return T_LOG_LESS;
		if (current == ">") return T_LOG_GREATER;
		if (current == "<=") return T_LOG_LEQ;
		if (current == ">=") return T_LOG_GEQ;
		if (current == "<<") return T_BIT_LSHIFT;
		if (current == ">>") return T_BIT_RSHIFT;
		if (current == "[") return T_L_BRACE;
		if (current == "]") return T_R_BRACE;
		if (current == "{") return T_L_BRACKET;
		if (current == "}") return T_R_BRACKET;
		if (current == "(") return T_L_PAREN;
		if (current == ")") return T_R_PAREN;
		if (current == ".") return T_PERIOD;
		if (current == ",") return T_COMMA;
		return T_ERR; // This is an unrecognized token
	}
	return T_ERR; // if it's not an identifier, keyword,
				  // numeric literal, string literal, or
				  // operator, it's unrecognized
}
