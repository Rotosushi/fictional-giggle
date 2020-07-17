

#include <string>
using std::string;
#include <iostream>
using std::cin;
using std::cout;
using std::istream;
using std::endl;

#include "Ast.hh"
#include "Lexer.hh"
#include "Parser.hh"

int main()
{
  Parser parser;
  string input;

  cout << "Welcome to Pink v0.0.2\npress Ctrl+C or Ctrl+D to quit.\n";

  while (1) {
    cout << ":> ";
    cin  >> input;
    auto ast = parser.parse(input);

    if (ast) {
      cout << (*ast)->to_string() << endl ;
    } else {
      cout << "failed to parse input: " << input << "\n";
    }
  }
  return 0;
}
