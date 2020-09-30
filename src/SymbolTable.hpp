#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::optional;

class Entity;

class SymbolTable {
private:
  unordered_map<string, shared_ptr<Entity>> symbs;
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
public:
  optional<shared_ptr<Entity>> lookupInLocalScopeOnly(const string& key);
  optional<shared_ptr<Entity>> operator[](const string& key);
  SymbolTable& operator=(const SymbolTable& rhs);

  void register_enclosing_scope(SymbolTable* scope);
  void bind (const string& key, const shared_ptr<Entity> value);
  void unbind (const string& key);
};
