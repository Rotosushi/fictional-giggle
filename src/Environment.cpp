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
#include "Environment.hpp"


Environment::Environment(shared_ptr<Parser> p, shared_ptr<SymbolTable> s, shared_ptr<BinopPrecedenceTable> bp, shared_ptr<BinopSet> bs, shared_ptr<UnopSet> us, shared_ptr<vector<string>> cl)
  : parser(p), scope(s), precedences(bp), binops(bs), unops(us), cleanup_list(cl)
{

}
