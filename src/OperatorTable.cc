

#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::pair;
using std::make_pair;
using std::get;
#include <optional>
using std::optional;
#include <tuple>
using std::tuple;
using std::make_tuple;


#include "OperatorTable.hh"
#include "Ast.hh"

void OperatorTable::insert(const string& op, int precedence, Assoc associativity, unique_ptr<Ast> body)
{
  ops.insert(make_pair(op, make_tuple(precedence, associativity, move(body))));
}

optional<pair<int, Assoc>> OperatorTable::find(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end()) {
    return optional<pair<int, Assoc>>();
  } else {
    return get<1>(*cursor);
  }
}

optional<int>  OperatorTable::findPrecedenceOf(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end())
  {
    return optional<int>();
  }
  else
  {
    /* (operator, (precedence, associativity)) */
    return optional<int>(get<int>(get<1>(*cursor)));
  }
}

optional<Assoc> OperatorTable::findAssociativityOf(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end())
  {
    return optional<Assoc>();
  }
  else
  {
    /* (operator, (precedence, associativity)) */
    return optional<Assoc>(get<Assoc>(get<1>(*cursor)));
  }
}

optional<unique_ptr<Ast>> OperatorTable::findBodyOf(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end())
  {
    return optional<unique_ptr<Ast>>();
  }
  else
  {
    return optional<unique_ptr<Ast>>(get<unique_ptr<Ast>>((get<1>(*cursor))->clone()));
  }
}
