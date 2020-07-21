
#include <string>
using std::string;
#include <set>
using std::set;
#include <utility>
using std::optional;
using std::pair;
using std::make_pair;
using std::get;



#include "OperatorTable.hh"
#include "Kernel.hh"

void init_typeops(OperatorTable& typeops)
{
  typeops.insert("->", 5, Assoc::Right);
}

void init_binops(OperatorTable& binops)
{
  binops.insert("+", 5, Assoc::Left);
  binops.insert("-", 5, Assoc::Left);
  binops.insert("*", 6, Assoc::Left);
  binops.insert("/", 6, Assoc::Left);
  binops.insert("%", 6, Assoc::Left);
}

void init_unops(set<string>& unops)
{
  unops.insert("-");
}
