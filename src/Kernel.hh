#pragma once

#include <string>
using std::string;
#include <set>
using std::set;

#include "OperatorTable.hh"

void init_typeops(OperatorTable& typeops);
void init_binops(OperatorTable& binops);
void init_unops(set<string>& unops);
