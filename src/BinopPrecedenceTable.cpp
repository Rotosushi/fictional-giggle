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
using std::make_optional;

#include "Associativity.hpp"
#include "BinopPrecedenceTable.hpp"

BinopPrecedenceTable::BinopPrecedenceTable()
{}

void BinopPrecedenceTable::RegisterBinopPrecAndAssoc(const string& op, int precedence, Associativity associativity)
{
  precedenceTable.insert(make_pair(op, make_pair(precedence, associativity)));
}

optional<pair<int,Associativity>> BinopPrecedenceTable::FindPrecAndAssoc(const string& key)
{
  auto binop = precedenceTable.find(key);
  if (binop != precedenceTable.end())
  {
    return make_optional(get<pair<int,Associativity>>(*binop));
  }
  else
  {
    return optional<pair<int, Associativity>>();
  }
}
