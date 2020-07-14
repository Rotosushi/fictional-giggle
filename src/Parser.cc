#include <string>
using std::string;
#include <stack>
using std::stack;
#include <vector>
using std::vector;
#include <set>
using std::set;
#include <memory>
using std::unique_ptr;
#include <utility>
using std::optional;
using std::pair;
using std::get;

#include "Parser.hh"
#include "Ast.hh"
#include "Lexer.hh"
#include "OperatorTable.hh"
#include "Kernel.hh"

Parser::Parser()
{
  init_binops(binops);
  init_unops(unops);
}

void Parser::reset()
{
  marks.clear();
  tokbuf.clear();
  txtbuf.clear();
  locbuf.clear();
}

int Parser::mark()
{
  marks.push(curidx);
  return curidx;
}

void Parser::release()
{
  marks.pop();
}

bool Parser::speculating()
{

}

unique_ptr<Ast> Parser::parse(const string& text);
