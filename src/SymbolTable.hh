#pragma once
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::optional;

class Ast;

class SymbolTable {
private:
  unordered_map<string, unique_ptr<Ast>> symbs;
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
  optional<unique_ptr<Ast>> operator[](const string& key);
  SymbolTable& operator=(const SymbolTable& rhs);

  void register_enclosing_scope(SymbolTable* scope);
  void bind (const string& key, const unique_ptr<Ast> value);
  void unbind (const string& key);
};
