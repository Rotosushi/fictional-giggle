
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::optional;

#include "Ast.hpp"
#include "SymbolTable.hpp"

SymbolTable::SymbolTable(const SymbolTable& other)
  : enclosing_scope(other.enclosing_scope)
{
  for (auto&& sym : other.symbs)
  {
    symbs.insert(make_pair(get<0>(sym), get<1>(sym)->clone()));
  }
}

optional<shared_ptr<Ast>> SymbolTable::lookupInLocalScopeOnly(const string& key)
{
  unordered_map::iterator sym = symbs.find(key);
  if (sym == symbs.end())
  {
    return optional<shared_ptr<Ast>>();
  }
  else
  {
    return optional<shared_ptr<Ast>>((get<shared_ptr<Ast>>(*sym))->clone());
  }
}

optional<shared_ptr<Ast>> SymbolTable::operator[](const string& key)
{
  unordered_map::iterator sym = symbs.find(key);
  if (sym = symbs.end())
  {
    if (enclosing_scope != nullptr)
    {
      return (*enclosing_scope)[key];
    }
    else
    {
      return optional<shared_ptr<Ast>>();
    }
  }
  else
  {
    return optional<shared_ptr<Ast>>((get<shared_ptr<Ast>>(*sym))->clone());
  }
}

SymbolTable& SymbolTable::operator=(const SymbolTable& rhs)
{
  symbs = rhs.symbs;
  enclosing_scope = rhs.enclosing_scope;
}

void SymbolTable::register_enclosing_scope(SymbolTable* scope)
{
  enclosing_scope = scope;
}

void SymbolTable::bind(const string& key, const shared_ptr<Ast>& value)
{
  symbs.insert(make_pair(key, value));
}

void SymbolTable::unbind(const string& key)
{
  symbs.erase(key);
}
