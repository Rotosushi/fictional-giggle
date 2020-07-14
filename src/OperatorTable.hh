
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

/* Associativity */
enum class Assoc {
  Left,
  Right,
  None,
};

class OperatorTable {
  unordered_map<string, pair<int, Assoc>> ops;

public:
  void insert(const string& op, int precedence, Assoc associativity);
  optional<pair<int, Assoc>> exists(const string& op);
  optional<int>   getPrecedence(const string& op);
  optional<Assoc> getAssociativity(const string& op);
};
