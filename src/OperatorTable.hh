
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

class Ast;

/* Associativity */
enum class Assoc {
  Left,
  Right,
  None,
};

class OperatorTable {
  unordered_map<string, tuple<int, Assoc, unique_ptr<Ast>>> ops;

public:
  void insert(const string& op, int precedence, Assoc associativity, unique_ptr<Ast> body);
  optional<tuple<int, Assoc, unique_ptr<Ast>>> find(const string& op);
  optional<int>   findPrecedenceOf(const string& op);
  optional<Assoc> findAssociativityOf(const string& op);
  optional<unique_ptr<Ast>> findBodyOf(const string& op);
};
