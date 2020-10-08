
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

optional<shared_ptr<Ast>> SymbolTable::lookupInLocalScopeOnly(const string& key)
{
  unordered_map::iterator sym = symbs.find(key);
  if (sym == symbs.end())
  {
    return optional<shared_ptr<Ast>>();
  }
  else
  {
    return optional<shared_ptr<Ast>>(get<shared_ptr<Ast>>(*sym));
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
    return optional<shared_ptr<Ast>>(get<shared_ptr<Ast>>(*sym));
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
