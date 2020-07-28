
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::optional;
using std::pair;

#include "Ast.hh"
#include "SymbolTable.hh"

optional<unique_ptr<Ast>> SymbolTable::operator[](const string& key)
{
  auto symbol = symbs.find(key);
  if (symbol == symbs.end())
  {
    return optional<unique_ptr<Ast>>();
  }
  else
  {
    return optional<unique_ptr<Ast>>(get<1>(*symbol)->clone());
  }
}

void SymbolTable::bind(const string& key, const unique_ptr<Ast> value)
{
  symbs.insert(make_pair(key, value->clone()));
}

void SymbolTable::unbind(const string& key)
{
  symbs.erase(key);
}
