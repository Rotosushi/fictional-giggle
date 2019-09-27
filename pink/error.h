#pragma once
#include <string>
using std::string;
using std::to_string;
#include <exception>
using std::exception;

#include "token.h"

class _error : public exception {
public:
	virtual const char* what() { return contents.c_str(); }

	_error() : contents() {}
	_error(string s) : contents(s) {}
private:
	string contents;
};

class _parser_error : public _error {
public:
	virtual const char* what() { return contents.c_str(); }

	_parser_error(string s, _token t, string id = "") : _error() {
		contents = "parser error : ";
		contents += s;
		if (t == T_ID) contents += id;
		else contents += token_to_string(t);
	}
	
	_parser_error(string s, int i) : _error() {
		contents = "parser error : ";
		contents += s;
		contents += to_string(i);
	}

	_parser_error(string s, float f) : _error() {
		contents = "parser error : ";
		contents += s;
		contents += to_string(f);
	}

	_parser_error(string s, string t) : _error() {
		contents = "parser error : ";
		contents += s;
		contents += t;
	}

	_parser_error(string s, bool b) : _error() {
		contents = "parser error : ";
		contents += s;
		contents += to_string(b);
	}

private:
	string contents;
};