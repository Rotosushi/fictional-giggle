#include "token.h"
#include <fstream>
using std::ifstream;
#include <string>
#include "lexer.h"
using std::string;

_token _lexer::gettok()
{
	text.clear(); // reset text buffer

	while (isspace(c)) { // ignore whitespace
		c = _getchar();
	}

	if (c == EOF) { // end of file
		return T_EOF;
	}

	if (c == '$') { // one line comments
		while (c != '\n' && c != '\r' && c != EOF)
			c = _getchar();
	}

	if (isalpha(c)) { // could be an id, or reserved word
		text += c;
		c = _getchar();

		while (_isidentifier(c)) { // read the entire word
			text += c;
			c = _getchar();
		}

		if (text == "int")   return T_INT;
		if (text == "float") return T_FLOAT;
		if (text == "text")  return T_TEXT;
		if (text == "bool")  return T_BOOL;
		if (text == "true")  return T_TRUE;
		if (text == "false") return T_FALSE;
		if (text == "if")	 return T_IF;
		if (text == "else")  return T_ELSE;
		if (text == "while") return T_WHILE;
		if (text == "fn")	 return T_FN;
		if (text == "return") return T_RETURN;
		if (text == "import") return T_IMPORT;
		if (text == "export") return T_EXPORT;
		if (text == "begin") return T_ROOT;
		return T_ID;
	}

	if (isdigit(c) || c == '.') { // could be a literal int/float
		bool has_fractional = c == '.' ? true : false;

		text += c;
		c = _getchar();

		while (isdigit(c) || c == '.') { // consume the whole number
			if (c == '.') has_fractional = true;
			text += c;
			c = _getchar();

			if (c == '.' && has_fractional) { // check for multiple '.'
				while (isdigit(c) || c == '.') {
					text += c;
					c = _getchar();
				}
				return T_ERR;
			}
		}
		
		// edge case: the number lexing loop
		// appears before the operator lexing loop
		// so this loop will falsely trigger on
		// a lone '.'. this is the current fix
		// update 10/28/2019: no longer needed as v1 doesn't have composite types
		//if (text == ".") return T_PERIOD;

		if (has_fractional) return T_LITERAL_FLOAT;
		else return T_LITERAL_INT;
	}

	if (c == '\'') { // literal text
		c = _getchar();

		while (c != '\'') {
			text += c;
			c = _getchar();

			// ensure we don't eat EOF
			// also this is an error.
			if (c == EOF) return T_ERR;
		}

		// eat the trailing '
		c = _getchar(); // prime the next char
		return T_LITERAL_TEXT;
	}

	if (c == '\"') { // literal text
		c = _getchar();

		while (c != '\"') {
			text += c;
			c = _getchar();

			// ensure we don't eat EOF
			// also this is an error.
			if (c == EOF) return T_ERR;
		}
		
		// eat the trailing '
		c = _getchar(); // prime the next char
		return T_LITERAL_TEXT;
	}

	if (_isoperator(c)) {
		text += c;
		c = _getchar();

		// single char operators
		if (text == "{") return T_LBRACE;
		if (text == "}") return T_RBRACE;
		if (text == "(") return T_LPAREN;
		if (text == ")") return T_RPAREN;
		if (text == ",") return T_COMMA;
		if (text == ";") return T_SEMICOLON;

		while (_isoperator(c)) { // eat the rest of the op
			text += c;
			c = _getchar();
		}

		// multichar ops and composable ops
		if (text == "->") return T_ARROW;
		if (text == "=") return T_EQ;
		if (text == ":") return T_COLON;
		if (text == ":=") return T_COLON_EQ;
		if (text == "::") return T_COLON_COLON;
		if (text == "+") return T_ADD;
		if (text == "-") return T_SUB;
		if (text == "*") return T_MULT;
		if (text == "/") return T_DIV;
		if (text == "%") return T_MOD;
		if (text == "&&") return T_BIT_AND;
		if (text == "||") return T_BIT_OR;
		if (text == "^^") return T_BIT_XOR;
		if (text == "!!") return T_BIT_NOT;
		if (text == "<<") return T_BIT_LSHIFT;
		if (text == ">>") return T_BIT_RSHIFT;
		if (text == "&") return T_AND;
		if (text == "|") return T_OR;
		if (text == "^") return T_XOR;
		if (text == "!") return T_NOT;
		if (text == "<") return T_LESS;
		if (text == ">") return T_GREATER;
		if (text == "<=") return T_LESS_EQUALS;
		if (text == ">=") return T_GREATER_EQUALS;
		if (text == "==") return T_EQUALS;
		if (text == "!=") return T_NOT_EQUALS;
		
		return T_ERR;
	}

	// if it's none of the above its an error
	return T_ERR;
}

string _lexer::gettext()
{
	return text;
}

void _lexer::set_infile(ifstream& input)
{
	i = 0;
	c = ' ';
	infile.close();
	infile.swap(input);
	input_state = 1;
}

void _lexer::set_instring(string input)
{
	i = 0;
	c = ' ';
	instring.clear();
	instring = input;
	input_state = 2;
}

int _lexer::_getchar()
{
	auto string_getchar = [this](string s) -> int {
		if (i < s.size()) return s[i++];
		else return EOF;
	};
	switch (input_state) {
	case 0: return getchar();
	case 1: return infile.get();
	case 2: return string_getchar(instring);
	default: throw;
	}
}

int _lexer::_isidentifier(int c)
{
	if (isalnum(c) || c == '_' || c == '-') {
		return 1;
	}
	return 0;
}

int _lexer::_isoperator(int c)
{
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
