#pragma once
#include <string>
using std::string;
#include <memory>
using std::unique_ptr;
#include <unordered_map>
using std::unordered_map;
#include <utility>
using std::pair;
using std::make_pair;
using std::get;
#include <optional>
using std::optional;

#include "Associativity.hpp"

class BinopPrecedenceTable
{
  unordered_map<string, pair<int, Associativity>> precedenceTable;
public:
  BinopPrecedenceTable();

  void RegisterBinopPrecAndAssoc(const string& op, int prec, Associativity assoc);
  optional<pair<int,int>> FindPrecAndAssoc(const string& key);
};
