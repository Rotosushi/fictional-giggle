#pragma once
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::optional;

#include "Ast.hh"

class SymbolTable {
private:
  unordered_map<string, unique_ptr<Ast>> symbs;

public:
  optional<unique_ptr<Ast>> operator[](const string& key);

  void bind (const string& key, const unique_ptr<Ast> value);
  void unbind (const string& key);
};
