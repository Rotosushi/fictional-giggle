
#include <string>
using std::string;
#include <set>
using std::set;
#include <memory>
using std::make_unique;
#include <utility>
using std::optional;
using std::pair;
using std::make_pair;
using std::get;

#include "Ast.hh"
#include "BinopTable.hh"
#include "Kernel.hh"

unique_ptr<Ast> TypeArrow(const Ast* const lhs, const Ast* const rhs);

unique_ptr<Ast> IntAddition(const Ast* const lhs, const Ast* const rhs);
unique_ptr<Ast> IntSubtraction(const Ast* const lhs, const Ast* const rhs);
unique_ptr<Ast> IntMultiplication(const Ast* const lhs, const Ast* const rhs);
unique_ptr<Ast> IntDivision(const Ast* const lhs, const Ast* const rhs);
unique_ptr<Ast> IntModulus(const Ast* const lhs, const Ast* const rhs);

void init_binops(BinopTable& binops)
{
  vector<pair<unique_ptr<TypeNode>, binop_eliminator>> rarrow_prim_elims;
  rarrow_prim_elims.push_back();
  binops.insert("->", Binop("->", 5, Assoc::Right, ));


  binops.insert("+", 5, Assoc::Left, dummy_body->clone());
  binops.insert("-", 5, Assoc::Left, dummy_body->clone());
  binops.insert("*", 6, Assoc::Left, dummy_body->clone());
  binops.insert("/", 6, Assoc::Left, dummy_body->clone());
  binops.insert("%", 6, Assoc::Left, dummy_body->clone());
  /*
  primitive binary operations:

  comparison operators: < > <= >= =

  boolean operators: & | ^

  arithmetic operators: + - * / %

  bitwise operators: && || ^^ >> <<

  language operators: -> . o
  */
}

void init_unops(UnopTable& unops)
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
  auto dummy_body = unique_ptr<EntityNode>();
  unops.insert("-", 1, Assoc::None, dummy_body->clone());
}
