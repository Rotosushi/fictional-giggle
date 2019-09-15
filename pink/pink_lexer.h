#pragma once
#include <string>
using std::string;
#include <iostream>
#include <fstream>
using std::ifstream;

enum _tok {
	T_EOF,
	T_ERR,
	T_MAYBE,
	T_NONE,
	T_ID,
	T_ADD,
	T_SUB,
	T_MULT,
	T_DIV,
	T_MOD,
	T_EQUALS,
	T_COLON,
	T_BITWISE_NOT,
	T_BITWISE_AND,
	T_BITWISE_XOR,
	T_BITWISE_OR,
	T_BITWISE_LSHIFT, 
	T_BITWISE_RSHIFT, 
	T_LBRACE,
	T_RBRACE,
	T_LBRACKET,
	T_RBRACKET,
	T_LPAREN,
	T_RPAREN,
	T_PERIOD,
	T_COMMA,
	T_CONST_ASSIGN,
	T_DYNAMIC_ASSIGN,
	T_ADD_ASSIGN, 
	T_SUB_ASSIGN,  
	T_MULT_ASSIGN,
	T_DIV_ASSIGN, 
	T_MOD_ASSIGN, 
	T_OR_ASSIGN,
	T_AND_ASSIGN,
	T_XOR_ASSIGN,
	T_LSHIFT_ASSIGN,
	T_RSHIFT_ASSIGN,
	T_LOG_AND,
	T_LOG_OR,
	T_LOG_XOR,
	T_LOG_NOT,
	T_LOG_NOT_EQUALS,
	T_LOG_EQUALS,
	T_LOG_LESS,
	T_LOG_GREATER,
	T_LOG_LESS_EQUALS,
	T_LOG_GREATER_EQUALS,
	T_QUESTION,
	T_SEMICOLON,
	T_ARROW,
	T_INT,
	T_FLOAT,
	T_BOOL,
	T_STRING,
	T_INT_LITERAL,
	T_FLOAT_LITERAL,
	T_TRUE,
	T_FALSE,
	T_STRING_LITERAL,
	T_STRUCT,
	T_UNION,
	T_ENUM,
	T_ALIAS,
	T_U8,
	T_U16,
	T_U32,
	T_U64,
	T_S8,
	T_S16,
	T_S32,
	T_S64,
	T_F32,
	T_F64,
	T_FUNCTION,
	T_TYPECAST,
	T_SIZEOF,
	T_IF,
	T_ELSE,
	T_WHILE,
	T_DO,
	T_FOR,
	T_CONTEXT,
	T_COMPILER_DIRECTIVE,
};

typedef struct {
	_tok type;
	std::string value;
} _token;

class _lexer {
public:
	_token gettok();
	void set_infile(char* filename);
	void set_instring(string input);
private:
	ifstream infile;
	string instring;
	int input_state = 0;

	int get_char();
	int isidentifier(int c);
	int isoperator(int c);
};


