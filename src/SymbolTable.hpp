#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::optional;

class Ast;
#include "BinopEliminators.hpp"

class SymbolTable {
private:
  unordered_map<string, shared_ptr<Ast>> symbs;
  // all scopes are either the top scope
  // or a subscope of another scope.
  // essentially all scopes form a funny sort of n-ary tree.
  // or an elaborate linked-list if you like.

  // in the same way that a static variable is
  // semantically a module local variable with
  // restricted visibility. a procedure local
  // definition is a module procedure definition
  // with restricted visibility.
  SymbolTable* enclosing_scope;

  SymbolTable(SymbolTable* scope) : symbs(), enclosing_scope(scope) {}
  SymbolTable(const SybolTable& other)
    : symbs(other.symbs), enclosing_scope(other.enclosing_scope) {}

public:
  optional<shared_ptr<Ast>> lookupInLocalScopeOnly(const string& key);
  optional<shared_ptr<Ast>> operator[](const string& key);
  SymbolTable& operator=(const SymbolTable& rhs);

  void register_enclosing_scope(SymbolTable* scope);
  void bind (const string& key, const shared_ptr<Ast>& value);
  void unbind (const string& key);
};
