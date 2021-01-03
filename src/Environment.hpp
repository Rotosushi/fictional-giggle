#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <memory>
using std::shared_ptr;

#include "Parser.hpp"
#include "BinopEliminators.hpp"
#include "BinopPrecedenceTable.hpp"
#include "UnopEliminators.hpp"
#include "SymbolTable.hpp"

class Environment
{
public:
  shared_ptr<Parser>               parser;
  shared_ptr<SymbolTable>          scope;
  shared_ptr<BinopPrecedenceTable> precedences;
  shared_ptr<BinopSet>             binops;
  shared_ptr<UnopSet>              unops;
  shared_ptr<vector<string>>       cleanup_list;

  Environment(shared_ptr<Parser> p, shared_ptr<SymbolTable> s, shared_ptr<BinopPrecedenceTable> bp, shared_ptr<BinopSet> bs, shared_ptr<UnopSet> us, shared_ptr<vector<string>> cl);
};
