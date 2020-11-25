#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::optional;
using std::get;

class Ast;
#include "BinopEliminators.hpp"

class SymbolTable {
private:
  unordered_map<string, shared_ptr<Ast>> symbs;
  // all scopes are either the top scope
  // or a subscope of another scope.
  // essentially all scopes form a funny sort of n-ary tree.
  // more like a list of tables, but because any table entry
  // could itself contain another table, it's a tree right?
  // so, "funny sort of tree"

  // in the same way that a static variable is
  // semantically a module local variable with
  // restricted visibility. a procedure local
  // definition is a module procedure definition
  // with restricted visibility.
  SymbolTable* enclosing_scope;

public:
  SymbolTable() : symbs(), enclosing_scope(nullptr) {}
  SymbolTable(SymbolTable* scope) : symbs(), enclosing_scope(scope) {}
  SymbolTable(const SymbolTable& other);

  optional<shared_ptr<Ast>> lookupInLocalScopeOnly(const string& key);
  optional<shared_ptr<Ast>> operator[](const string& key);
  SymbolTable& operator=(const SymbolTable& rhs);

  void register_enclosing_scope(SymbolTable* scope);
  void bind (const string& key, const shared_ptr<Ast>& value);
  void unbind (const string& key);
};
