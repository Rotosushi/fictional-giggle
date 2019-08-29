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
	case '>': case '!': case '^': case ';':
	case '|': case '&': case '[': case ']':
	case '{': case '}': case '(': case ')':
	case ',':
		return 1;
	default:
		return 0;
	}
}

Token gettok() {
	static int c = ' '; // prime the input
	Token tok;

	while (isspace(c)) { // ignore whitespace
		c = getchar();
	}

	if (c == EOF) {
		tok.value = "EOF";
		tok.type = T_EOF;
		return tok;
	}

	if (isalpha(c)) { // could be an identifier, a type-primitive, or a keyword
		tok.value += c;
		c = getchar();
		while (isidentifier(c)) { // consume the next word
			tok.value += c;
			c = getchar();
		}

		if (tok.value == "int")			  tok.type = T_INT;
		else if (tok.value == "float")	  tok.type = T_FLOAT;
		else if (tok.value == "string")   tok.type = T_STRING;
		else if (tok.value == "bool")	  tok.type = T_BOOL;
		else if (tok.value == "maybe")	  tok.type = T_MAYBE;
		else if (tok.value == "none")	  tok.type = T_NONE;
		else if (tok.value == "true")	  tok.type = T_TRUE;
		else if (tok.value == "false")    tok.type = T_FALSE;
		else if (tok.value == "if")		  tok.type = T_IF;
		else if (tok.value == "else")	  tok.type = T_ELSE;
		else if (tok.value == "do")		  tok.type = T_DO;
		else if (tok.value == "while")    tok.type = T_WHILE;
		else if (tok.value == "for")	  tok.type = T_FOR;
		else if (tok.value == "alias")    tok.type = T_ALIAS;
		else if (tok.value == "sizeof")   tok.type = T_SIZEOF;
		else if (tok.value == "typecast") tok.type = T_TYPECAST;
		else if (tok.value == "struct")   tok.type = T_STRUCT;
		else if (tok.value == "union")	  tok.type = T_UNION;
		else if (tok.value == "enum")	  tok.type = T_ENUM;
		else if (tok.value == "context")  tok.type = T_CONTEXT;
		else if (tok.value == "fn")  tok.type = T_FUNCTION;
		else if (tok.value == "u8")  tok.type = T_U8;
		else if (tok.value == "u16") tok.type = T_U16;
		else if (tok.value == "u32") tok.type = T_U32;
		else if (tok.value == "u64") tok.type = T_U64;
		else if (tok.value == "s8")  tok.type = T_S8;
		else if (tok.value == "s16") tok.type = T_S16;
		else if (tok.value == "s32") tok.type = T_S32;
		else if (tok.value == "s64") tok.type = T_S64;
		else if (tok.value == "f32") tok.type = T_F32;
		else if (tok.value == "f64") tok.type = T_F64;
		else tok.type = T_ID;
		return tok;
	}

	if (isdigit(c) || c == '.') { // could be an int or float literal
		bool has_fractional = c == '.' ? true : false;

		tok.value += c;
		c = getchar();

		while (isdigit(c) || c == '.') { // consume the whole number
			if (c == '.') has_fractional = true;
			tok.value += c;
			c = getchar();

			if (c == '.' && has_fractional) {    // this is a malformed numer
				while (c != '\n' && c != '\r') { // grab the rest of the line
					tok.value += c;
					c = getchar();
				}
				tok.type = T_ERR;
				return tok;
			}
		}

		if (has_fractional) {
			tok.type = T_FLOAT_LITERAL;
			return tok;
		} else {
			tok.type = T_INT_LITERAL;
			return tok;
		}
	}

	if (c == '\'' || c == '\"') { // it's a string literal
		tok.value += c;
		c = getchar();

		while (c != '\'' && c != '\"') {
			tok.value += c;
			c = getchar();
		}
		// ensure we don't consume the EOF
		if (c == EOF) { // This is probably because of no ending ' or "
			tok.type = T_ERR;
			return tok;
		}

		tok.value += c; // store the trailing ' or "
		c = getchar();  // prime the next char

		tok.type = T_STRING_LITERAL;
		return tok;
		
	}

	if (isoperator(c)) { // else it's an operator or grouping symbol
		// operators are all parsed one symbol at a time
		tok.value += c;
		c = getchar();

		if (tok.value == "[") {
			tok.type = T_LBRACE;
			return tok;
		}
		else if (tok.value == "]") { 
			tok.type = T_RBRACE;
			return tok;
		}
		else if (tok.value == "{") { 
			tok.type = T_LBRACKET;
			return tok;
		}
		else if (tok.value == "}") {
			tok.type = T_RBRACKET;
			return tok;
		}
		else if (tok.value == "(") { 
			tok.type = T_LPAREN;
			return tok;
		}
		else if (tok.value == ")") { 
			tok.type = T_RPAREN;
			return tok;
		}
		else if (tok.value == ".") { 
			tok.type = T_PERIOD;
			return tok;
		}
		else if (tok.value == ";") { 
			tok.type = T_SEMICOLON;
			return tok;
		}
		else if (tok.value == ",") { 
			tok.type = T_COMMA;
			return tok;
		}
		else {
			while (isoperator(c)) {
				tok.value += c;
				c = getchar();
			}

			/* assignment operators */
			if (tok.value == "=")  tok.type = T_EQUALS;
			else if (tok.value == ":")  tok.type = T_COLON;
			else if (tok.value == "::") tok.type = T_CONST_ASSIGN;
			else if (tok.value == ":=") tok.type = T_DYNAMIC_ASSIGN;
			/* arithmetic operators */
			else if (tok.value == "+")  tok.type = T_ADD;
			else if (tok.value == "-")  tok.type = T_SUB;
			else if (tok.value == "*")  tok.type = T_MULT;
			else if (tok.value == "/")  tok.type = T_DIV;
			else if (tok.value == "%")  tok.type = T_MOD;
			else if (tok.value == "+=") tok.type = T_ADD_ASSIGN;
			else if (tok.value == "-=") tok.type = T_SUB_ASSIGN;
			else if (tok.value == "*=") tok.type = T_MULT_ASSIGN;
			else if (tok.value == "/=") tok.type = T_DIV_ASSIGN;
			else if (tok.value == "%=") tok.type = T_MOD_ASSIGN;
			/* bitwise operators */
			else if (tok.value == "&")  tok.type = T_AND;
			else if (tok.value == "|")  tok.type = T_OR;
			else if (tok.value == "^")  tok.type = T_XOR;
			else if (tok.value == "!")  tok.type = T_NOT;
			else if (tok.value == "<")  tok.type = T_LSHIFT;
			else if (tok.value == ">")  tok.type = T_RSHIFT;
			else if (tok.value == "|=") tok.type = T_OR_ASSIGN;
			else if (tok.value == "&=") tok.type = T_AND_ASSIGN;
			else if (tok.value == "^=") tok.type = T_XOR_ASSIGN;
			else if (tok.value == "<=") tok.type = T_LSHIFT_ASSIGN;
			else if (tok.value == ">=") tok.type = T_RSHIFT_ASSIGN;
			/* logic operators */
			else if (tok.value == "&&")  tok.type = T_LOG_AND;
			else if (tok.value == "||")  tok.type = T_LOG_OR;
			else if (tok.value == "^^")  tok.type = T_LOG_XOR;
			else if (tok.value == "!!")  tok.type = T_LOG_NOT;
			else if (tok.value == "!!=") tok.type = T_LOG_NOT_EQUALS;
			else if (tok.value == "==")  tok.type = T_LOG_EQUALS;
			/* relational operators */
			else if (tok.value == "<<")  tok.type = T_LOG_LESS;
			else if (tok.value == ">>")  tok.type = T_LOG_GREATER;
			else if (tok.value == "<<=") tok.type = T_LOG_LESS_EQUAL;
			else if (tok.value == ">>=") tok.type = T_LOG_GREATER_EQUAL;
			/* language symbols */
			else if (tok.value == "->") tok.type = T_ARROW;
			else tok.type = T_ERR; // This is an unrecognized token
			return tok;
		}
	}
	
	if (c == '#') { // it's a compiler directive
		tok.value += c;
		c = getchar();

		while (isidentifier(c)) {
			tok.value += c;
			c = getchar();
		}
		tok.type = T_COMPILER_DIRECTIVE;
		return tok;
	}

	tok.type = T_ERR;
	return tok; // if it's not an identifier, keyword,
				  // numeric literal, string literal, or
				  // operator, it's unrecognized
}
