
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

SymbolTable& SymbolTable::operator=(const SymbolTable& rhs)
{
  // we cannot call the std::unordered_map::operator=
  // as that calls the implicitly deleted copy constructor
  // of unique_ptr<Ast>, so instead, hopefully, we can
  // simply construct a new entry with a clone of the
  // unique_ptr, and looping through all entries in the
  // rhs gives us an identical symboltable!
  // albeit using a very greedy algorithm.
  symbs.clear();
  for (auto&& sym : rhs.symbs)
  {
    symbs->insert(make_pair(get<string>(sym), get<unique_ptr<Ast>>(sym)->clone()));
  }
  return *this;
}

optional<unique_ptr<Ast>> SymbolTable::operator[](const string& key)
{
  unordered_map::iterator symbol = symbs.find(key);
  if (symbol == symbs.end())
  {
    // given that all scopes form a weird sort of
    // tree, we want to traverse that tree to find
    // the symbol being referred to. we start with
    // the innermost scope, as part of the reason
    // that we have name shadowing semantics.
    // the innermost scope is a scope in it's own
    // right. which means that it can 'redefine'
    // a symbol which appears in an outer scope.
    // and this algorithm will by definition find the
    // inner symbol before the outer symbol.
    if (symbs.enclosing_scope != nullptr)
    {
      optional<unique_ptr<Ast>> symbol = (*symbs.enclosing_scope)[key];

      if (symbol)
        return optional<unique_ptr<Ast>>(get<1>(*symbol)->clone());
      else
        return optional<unique_ptr<Ast>>();
    }
    else
    {
      return optional<unique_ptr<Ast>>();
    }
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
