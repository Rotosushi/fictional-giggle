

#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::optional;
using std::pair;
using std::make_pair;
using std::get;

#include "OperatorTable.hh"

void OperatorTable::insert(const string& op, int precedence, Assoc associativity)
{
  ops.insert(make_pair(op, make_pair(precedence, associativity)));
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

optional<int>  OperatorTable::getPrecedence(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end()) {
    return optional<int>();
  } else {
    /* (operator, (precedence, associativity)) */
    return optional<int>(get<int>(get<1>(*cursor)));
  }
}

optional<Assoc> OperatorTable::getAssociativity(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end()) {
    return optional<Assoc>();
  } else {
    /* (operator, (precedence, associativity)) */
    return optional<Assoc>(get<Assoc>(get<1>(*cursor)));
  }
}
