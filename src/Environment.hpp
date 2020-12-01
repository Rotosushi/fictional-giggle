#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;
#include <list>
using std::list;
#include <memory>
using std::shared_ptr;

#include "BinopEliminators.hpp"
#include "BinopPrecedenceTable.hpp"
#include "UnopEliminators.hpp"
#include "SymbolTable.hpp"

class Environment
{
public:
  shared_ptr<SymbolTable>          scope;
  shared_ptr<BinopPrecedenceTable> precedences;
  shared_ptr<BinopSet>             binops;
  shared_ptr<UnopSet>              unops;
  shared_ptr<list<string>>         cleanup_list;

  Environment(shared_ptr<SymbolTable> s, shared_ptr<BinopPrecedenceTable> bp, shared_ptr<BinopSet> bs, shared_ptr<UnopSet> us, shared_ptr<list<string>> cl);
};
