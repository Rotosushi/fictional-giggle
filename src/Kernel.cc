
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

void init_binops(OperatorTable& binops)
{
  binops.insert("->", 5, Assoc::Right);
  binops.insert("+", 5, Assoc::Left);
  binops.insert("-", 5, Assoc::Left);
  binops.insert("*", 6, Assoc::Left);
  binops.insert("/", 6, Assoc::Left);
  binops.insert("%", 6, Assoc::Left);
  /*
  primitive binary operations:

  comparison operators: < > <= >= =

  boolean operators: & | ^

  arithmetic operators: + - * / %

  bitwise operators: && || ^^ >> <<

  language operators: -> . o
  */
}

void init_unops(set<string>& unops)
{
  /*
  note: unary operators should avoid symbolically
        intersecting with binary operators.
        if they do, be aware the grammar rules will select
        the binary form of the operator every time.
        this is to avoid the reverse case of parsing
        [3 - 4] as [3 (-4)]
        and yeah, you are reading that correctly,
        it's a call expression...
        and hence a way worse alternative.
        it would be so much more confusing,
        and by being such a simple lexical mistake,
        would make coding in the langauge
        like trying to wade through a syntax minefield.

  primitive unary operations:

  boolean operator: !

  arithmetic operator: ~

  bitwise operator: !!

  language operators: & *
  */
  unops.insert("-");
}
