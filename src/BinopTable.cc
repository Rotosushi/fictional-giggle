

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


#include "BinopTable.hh"
#include "Ast.hh"

void BinopTable::insert(const string& op, Binop binop)
{
  ops.insert(make_pair(op, binop));
}

optional<Binop> BinopTable::find(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end())
  {
    return optional<Binop>();
  }
  else
  {
    // construct an optional using a copy of the
    // unique_ptr containing the body.
    // without calling the copy constructor of the
    // unique_ptr, as that is deleted.
    int prec    = get<int>(get<1>(*cursor));
    Assoc assoc = get<Assoc>(get<1>(*cursor));
    auto&& body   = get<unique_ptr<Ast>>(get<1>(*cursor));
    return optional<tuple<int, Assoc, unique_ptr<Ast>>>(make_tuple(prec, assoc, body->clone()));
  }
}

optional<int>  BinopTable::findPrecedenceOf(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end())
  {
    return optional<int>();
  }
  else
  {
    /* (operator, (precedence, associativity, body)) */
    return optional<int>(get<int>(get<1>(*cursor)));
  }
}

optional<Assoc> BinopTable::findAssociativityOf(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end())
  {
    return optional<Assoc>();
  }
  else
  {
    /* (operator, (precedence, associativity, body)) */
    return optional<Assoc>(get<Assoc>(get<1>(*cursor)));
  }
}

optional<unique_ptr<Ast>> BinopTable::findBodyOf(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end())
  {
    return optional<unique_ptr<Ast>>();
  }
  else
  {
    return optional<unique_ptr<Ast>>(get<unique_ptr<Ast>>(get<1>(*cursor))->clone());
  }
}
