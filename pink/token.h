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
	T_ADD_EQ,
	T_SUB_EQ,
	T_MULT_EQ,
	T_DIV_EQ,
	T_MOD_EQ,
	T_BIT_OR_EQ,
	T_BIT_AND_EQ,
	T_BIT_XOR_EQ,
	T_BIT_NOT_EQ,
	T_BIT_LSHIFT_EQ,
	T_BIT_RSHIFT_EQ,
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
	T_LOG_NOT,
	T_LOG_AND,
	T_LOG_XOR,
	T_LOG_OR,
	T_LOG_EQUALS,
	T_LOG_NOT_EQUALS,
	T_LOG_LESS,
	T_LOG_GREATER,
	T_LOG_LESS_EQUALS,
	T_LOG_GREATER_EQUALS,
	/* language */
	T_PERIOD,
	T_COMMA,
	T_SEMICOLON,
	T_LBRACE,
	T_RBRACE,
	T_LBRACKET,
	T_RBRACKET,
	T_LPAREN,
	T_RPAREN,
	T_ARROW,
	T_IF,
	T_ELSE,
	T_DO,
	T_WHILE,

	T_ID,
	T_RETURN,
	T_MODULE,
	T_IMPORT,
	T_EXPORT,
	T_BEGIN,
	/* types */
	T_INT,
	T_FLOAT,
	T_TEXT,
	T_BOOL,
	T_LITERAL_INT,
	T_LITERAL_FLOAT,
	T_LITERAL_TEXT,
	T_TRUE,
	T_FALSE,
	T_FN,
	T_RECORD,
};

string token_to_string(_token t);