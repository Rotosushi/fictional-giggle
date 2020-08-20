
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
#include <tuple>
using std::tuple;
using std::make_tuple;
#include <vector>
using std::vector;

#include "Ast.hh"
#include "Binop.hh"


class BinopTable {
  unordered_map<string, Binop> ops;

public:
  void insert(const string& op, Binop binop);
  optional<Binop> find(const string& op);
};
