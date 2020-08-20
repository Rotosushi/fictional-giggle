#pragma once
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::optional;
using std::pair;
using std::get;
#include <list>
using std::list;

#include "Ast.hh"

typedef unique_ptr<Ast> (*unop_eliminator)(const Ast* const, const Ast* const);

class Unop {
public:
  list<pair<unique_ptr<TypeNode>, unop_eliminator>> primitive_eliminators;
  Procedure composite_eliminators;
};

class UnopTable {
  unordered_map<string, Unop> ops;

public:
  void insert(const string& op, Unop unop);
  optional<Unop> find(const string& op);
};
