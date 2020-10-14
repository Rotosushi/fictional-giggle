#pragma once

#include "BinopEliminators.hpp"
#include "BinopPrecedenceTable.hpp"
#include "UnopEliminators.hpp"


class OperatorTable
{
public:
  BinopPrecedenceTable precedences;
  BinopSet             binops;
  UnopSet              unops;
};
