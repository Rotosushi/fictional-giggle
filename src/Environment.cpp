#include <memory>
using std::shared_ptr;

#include "BinopEliminators.hpp"
#include "BinopPrecedenceTable.hpp"
#include "UnopEliminators.hpp"
#include "SymbolTable.hpp"
#include "Environment.hpp"

Environment::Environment(shared_ptr<SymbolTable> s, shared_ptr<BinopPrecedenceTable> bp, shared_ptr<BinopSet> bs, shared_ptr<UnopSet> us)
  : scope(s), precedences(bp), binops(bp), unops(us)
{

}
