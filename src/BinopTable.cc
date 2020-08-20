

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

optional<Binop&> BinopTable::find(const string& op)
{
  auto cursor = ops.find(op);
  if (cursor == ops.end())
  {
    return optional<Binop&>();
  }
  else
  {
    return optional<Binop&>(get<1>(*cursor))
  }
}
