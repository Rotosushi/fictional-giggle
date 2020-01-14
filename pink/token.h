#pragma once
#include <string>
using std::string;

enum _token {
	/* internal */
	T_ERR,
	T_EOF,
	/* operators */
	T_EQ,
	T_COLON,
	T_COLON_EQ,
	T_COLON_COLON,
	T_ADD,
	T_SUB,
	T_MULT,
	T_DIV,
	T_MOD,
	T_BIT_NOT,
	T_BIT_AND,
	T_BIT_XOR,
	T_BIT_OR,
	T_BIT_LSHIFT,
	T_BIT_RSHIFT,
	T_NOT,
	T_AND,
	T_XOR,
	T_OR,
	T_EQUALS,
	T_NOT_EQUALS,
	T_LESS,
	T_GREATER,
	T_LESS_EQUALS,
	T_GREATER_EQUALS,
	/* language symbols */
	T_DOLLAR,
	T_COMMA,
	T_SEMICOLON,
	T_LBRACE,
	T_RBRACE,
	T_LPAREN,
	T_RPAREN,
	/* keywords */
	T_VAR,
	T_FN,
	T_ARROW,
	T_IF,
	T_ELSE,
	T_WHILE,
	T_ID,
	T_RETURN,
	T_IMPORT,
	T_EXPORT,
	T_ROOT,
	/* primitive type tokens */
	T_INT,
	T_REAL,
	T_TEXT,
	T_BOOL,
	/* literal tokens */
	T_LITERAL_INT,
	T_LITERAL_FLOAT,
	T_LITERAL_TEXT,
	T_TRUE,
	T_FALSE,
};

string token_to_string(_token t);