#pragma once
#include <string>
using std::string;
#include <fstream>
using std::ifstream;

#include "token.h"

class Lexer {
public:
	void set_stdio();
	void set_infile(ifstream& f);
	void set_instring(string& s);

	Token  gettok();
	string gettext();

	// default initialization assumes reading from stdin
	Lexer() : infile(), instring(), curtext() {
		idx = 0;
		input_state = 0;
		c = ' ';
	}

	// passing in a file sets the lexer to read from the file
	Lexer(ifstream& f) : instring(), curtext() {
		idx = 0;
		infile.swap(f);
		input_state = 1;
		c = ' ';
	}

	// passing in a string sets the lexer to read from the string
	Lexer(string& s) : infile(), instring(s), curtext() {
		idx = 0;
		input_state = 2;
		c = ' ';
	}

private:
	/* lexer input management */
	unsigned int idx;
	int input_state;
	ifstream infile;
	string   instring;

	int _getchar();
	/* the character buffer 
		this is treated semantically
		as equivalent to a 
		static int c;
		decl inside the gettok() function.
		it remains filled between calls to 
		gettok, so this lexer in not reentrant.
		this pushes the sense of multithreading
		to the parser, where if each parser
		has it's own lexer, then we could easily
		imagine each parser living in it's own thread.


		the curtext gets reset on each call to
		gettok, it holds the text that lexed
		to the token returned.
	*/
	int c; 
	string curtext;

	//bool is_identifier(Token t);
	bool issymbol(int c);
};



//#include "token.h"
//#include <fstream>
//using std::ifstream;
//#include <string>
//using std::string;
//
//class _lexer { 
//public:	
//	_token gettok();
//	string gettext();
//	void set_infile(ifstream& input);
//	void set_instring(string input);
//
//	_lexer() {
//		c = ' ';
//		i = 0;
//	}
//private:
//	int c;
//	unsigned int i;
//	int input_state = 0;
//	ifstream infile;
//	string instring;
//	string text;
//
//	int _getchar();
//	int _isidentifier(int c);
//	int _isoperator(int c);
//};