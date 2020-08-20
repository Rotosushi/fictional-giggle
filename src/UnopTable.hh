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
#include <vector>
using std::vector;

#include "Ast.hh"
#include "Unop.hh"

class UnopTable {
  unordered_map<string, Unop> ops;

public:
  void insert(const string& op, Unop unop);
  optional<Unop> find(const string& op);
};
