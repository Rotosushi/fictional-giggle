// pink.cpp : This file contains the 'main' function. Program execution begins and ends there.

#include <iostream>
#include "pink_lexer.h"
#include "pink_parser.h"
#include <cassert>

/*
void test_lexer() {
	int t;
	t = gettok();
	assert(current == "+");
	assert(t == T_ADD);
	current.clear();

	t = gettok();
	assert(current == "-");
	assert(t == T_SUB);
	current.clear();

	t = gettok();
	assert(current == "*");
	assert(t == T_MULT);
	current.clear();

	t = gettok();
	assert(current == "/");
	assert(t == T_DIV);
	current.clear();

	t = gettok();
	assert(current == "%");
	assert(t == T_MOD);
	current.clear();

	t = gettok();
	assert(current == "=");
	assert(t == T_EQUALS);
	current.clear();

	t = gettok();
	assert(current == "==");
	assert(t == T_LOG_EQ);
	current.clear();

	t = gettok();
	assert(current == "!!=");
	assert(t == T_LOG_NEQ);
	current.clear();

	t = gettok();
	assert(current == ":");
	assert(t == T_COLON);
	current.clear();

	t = gettok();
	assert(current == ":=");
	assert(t == T_ASSIGN_COLON_EQ);
	current.clear();

	t = gettok();
	assert(current == "::");
	assert(t == T_ASSIGN_COLON_COLON);
	current.clear();

	t = gettok();
	assert(current == "*=");
	assert(t == T_MULT_ASSIGN);
	current.clear();

	t = gettok();
	assert(current == "/=");
	assert(t == T_DIV_ASSIGN);
	current.clear();

	t = gettok();
	assert(current == "+=");
	assert(t == T_ADD_ASSIGN);
	current.clear();

	t = gettok();
	assert(current == "-=");
	assert(t == T_SUB_ASSIGN);
	current.clear();

	t = gettok();
	assert(current == "!");
	assert(t == T_NOT);
	current.clear();

	t = gettok();
	assert(current == "!!");
	assert(t == T_LOG_NOT);
	current.clear();

	t = gettok();
	assert(current == "^");
	assert(t == T_XOR);
	current.clear();

	t = gettok();
	assert(current == "^^");
	assert(t == T_LOG_XOR);
	current.clear();

	t = gettok();
	assert(current == "&");
	assert(t == T_AND);
	current.clear();

	t = gettok();
	assert(current == "&&");
	assert(t == T_LOG_AND);
	current.clear();

	t = gettok();
	assert(current == "[");
	assert(t == T_LBRACE);
	current.clear();

	t = gettok();
	assert(current == "]");
	assert(t == T_RBRACE);
	current.clear();

	t = gettok();
	assert(current == "{");
	assert(t == T_LBRACKET);
	current.clear();

	t = gettok();
	assert(current == "}");
	assert(t == T_RBRACKET);
	current.clear();

	t = gettok();
	assert(current == "(");
	assert(t == T_LPAREN);
	current.clear();

	t = gettok();
	assert(current == ")");
	assert(t == T_RPAREN);
	current.clear();

	t = gettok();
	assert(current == "<<");
	assert(t == T_BIT_LSHIFT);
	current.clear();

	t = gettok();
	assert(current == ">>");
	assert(t == T_BIT_RSHIFT);
	current.clear();

	t = gettok();
	assert(current == "<<=");
	assert(t == T_LSHIFT_ASSIGN);
	current.clear();

	t = gettok();
	assert(current == ">>=");
	assert(t == T_RSHIFT_ASSIGN);
	current.clear();

	t = gettok();
	assert(current == "<");
	assert(t == T_LESS);
	current.clear();

	t = gettok();
	assert(current == ">");
	assert(t == T_GREATER);
	current.clear();

	t = gettok();
	assert(current == "<=");
	assert(t == T_LOG_LEQ);
	current.clear();

	t = gettok();
	assert(current == ">=");
	assert(t == T_LOG_GEQ);
	current.clear();

	
	t = get_next_token();
	assert(current == "");
	assert(t == );
	current.clear();
	
	t = gettok();
	assert(current == "hello");
	assert(t == T_ID);
	current.clear();

	t = gettok();
	assert(current == "for");
	assert(t == T_FOR);
	current.clear();

	t = gettok();
	assert(current == "while");
	assert(t == T_WHILE);
	current.clear();

	t = gettok();
	assert(current == "if");
	assert(t == T_IF);
	current.clear();

	t = gettok();
	assert(current == "else");
	assert(t == T_ELSE);
	current.clear();

	t = gettok();
	assert(current == "pink");
	assert(t == T_ID);
	current.clear();

	t = gettok();
	assert(current == "91");
	assert(t == T_INT_LITERAL);
	current.clear();

	t = gettok();
	assert(current == "9.1");
	assert(t == T_FLOAT_LITERAL);
	current.clear();

	t = gettok();
	assert(current == ".900");
	assert(t == T_FLOAT_LITERAL);
	current.clear();

	t = gettok();
	assert(current == "9.9.9");
	assert(t == T_ERR);
	current.clear();

	t = gettok();
	assert(current == "%%");
	assert(t == T_ERR);
	current.clear();

	t = gettok();
	assert(current == "\"Hello, World!\"");
	assert(t == T_STRING_LITERAL);
	current.clear();

	t = gettok();
	assert(current == "int");
	assert(t == T_INT);
	current.clear();

	t = gettok();
	assert(current == "float");
	assert(t == T_FLOAT);
	current.clear();

	t = gettok();
	assert(current == "bool");
	assert(t == T_BOOL);
	current.clear();

	t = gettok();
	assert(current == "true");
	assert(t == T_TRUE);
	current.clear();

	t = gettok();
	assert(current == "false");
	assert(t == T_FALSE);
	current.clear();

	t = gettok();
	assert(current == "string");
	assert(t == T_STRING);
	current.clear();

	t = gettok();
	assert(current == "struct");
	assert(t == T_STRUCT);
	current.clear();

	t = gettok();
	assert(current == "union");
	assert(t == T_UNION);
	current.clear();

	t = gettok();
	assert(current == "enum");
	assert(t == T_ENUM);
	current.clear();

	t = gettok();
	assert(current == "alias");
	assert(t == T_ALIAS);
	current.clear();

	t = gettok();
	assert(current == "typecast");
	assert(t == T_TYPECAST);
	current.clear();

	t = gettok();
	assert(current == "sizeof");
	assert(t == T_SIZEOF);
	current.clear();

	t = gettok();
	assert(current == "u8");
	assert(t == T_U8);
	current.clear();

	t = gettok();
	assert(current == "u16");
	assert(t == T_U16);
	current.clear();

	t = gettok();
	assert(current == "u32");
	assert(t == T_U32);
	current.clear();

	t = gettok();
	assert(current == "u64");
	assert(t == T_U64);
	current.clear();

	t = gettok();
	assert(current == "s8");
	assert(t == T_S8);
	current.clear();

	t = gettok();
	assert(current == "s16");
	assert(t == T_S16);
	current.clear();

	t = gettok();
	assert(current == "s32");
	assert(t == T_S32);
	current.clear();

	t = gettok();
	assert(current == "s64");
	assert(t == T_S64);
	current.clear();

	t = gettok();
	assert(current == "f32");
	assert(t == T_F32);
	current.clear();

	t = gettok();
	assert(current == "f64");
	assert(t == T_F64);
	current.clear();

	t = gettok();
	assert(current == "for");
	assert(t == T_FOR);
	current.clear();

	t = gettok();
	assert(current == "string");
	assert(t == T_STRING);
	current.clear();

	t = gettok();
	assert(current == "hello");
	assert(t == T_ID);
	current.clear();

	t = gettok();
	assert(current == "=");
	assert(t == T_EQUALS);
	current.clear();

	t = gettok();
	assert(current == "\"Hello,\"");
	assert(t == T_STRING_LITERAL);
	current.clear();

	t = gettok();
	assert(current == "+");
	assert(t == T_ADD);
	current.clear();

	t = gettok();
	assert(current == "\"World!\"");
	assert(t == T_STRING_LITERAL);
	current.clear();

	t = gettok();
	assert(current == "EOF");
	assert(t == T_EOF);
	current.clear();
}
*/

void test_parser() {
	build_module();
}

int main()
{
    std::cout << "Hello, Pink!\n";
	//test_lexer();
	test_parser();
	return 0;
}

