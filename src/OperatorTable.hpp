#pragma once
#include <string>
using std::string;
#include <memory>
using std::shared_ptr;
#include <list>
using std::list;
#include <pair>
using std::pair;

#include "BinopEliminators.hpp"
#include "BinopPrecedenceTable.hpp"
#include "UnopEliminators.hpp"


class OperatorTable
{
public:
  BinopPrecedenceTable precedences;
  BinopSet             binops;
  UnopSet              unops;
}
