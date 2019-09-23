#pragma once
#include "token.h"
#include <fstream>
using std::ifstream;
#include <string>
using std::string;

class _lexer {
public:	
	_token gettok();
	string gettext();
	void set_infile(char* filename);
	void set_instring(string input);

private:
	int input_state = 0;
	ifstream infile;
	string instring;
	string text;

	int _getchar();
	int _isidentifier(int c);
	int _isoperator(int c);
};