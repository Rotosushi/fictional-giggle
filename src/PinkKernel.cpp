
#include <memory>
using std::shared_ptr;
using std::make_shared;

#include "Ast.hpp"
#include "Entity.hpp"
#include "Environment.hpp"
#include "PinkKernel.hpp"


shared_ptr<Ast> PlusBinopAddInts(const Ast* const lhs, const Ast* const rhs)
{
  /*
  this could be done without the dynamic casts
  with simple c-style casts and this would be
  faster, and less safe.
  although, by design of
  the interpreter we don't feed any type but
  what has been asked for into these procedures.
  so maybe it isn't -that- unsafe?

  const Entity* lentity = (Entity*)lhs;
  const Entity* rentity = (Entity*)rhs;

  const Integer* lint = (Integer*)lentity->literal.get();
  const Integer* rint = (Integer*)rentity->literal.get();

  return make_shared(Entity(lint->value  rint->value, Location()));
  */
  const Entity* lentity = dynamic_cast<const Entity*>(lhs);
  const Entity* rentity = dynamic_cast<const Entity*>(rhs);

  if (!lentity)
    throw "Bad Lhs Ast";

  if (!rentity)
    throw "Bad Rhs Ast";

  const Integer* lint = dynamic_cast<Integer*>(lentity->literal.get());
  const Integer* rint = dynamic_cast<Integer*>(rentity->literal.get());

  if (!lint)
    throw "Bad Object Type";

  if (!rint)
    throw "Bad Object Type";

    /*
    eventually the returned location should be
    the location of the entire expression, except
    modified to reflect the textual length of the
    result value. it could start (i.e. first_line, first_column)
    at the same place as the binop expression we are reducing,
    and it's length is the length of the resulting value.
    (i.e. last_line, last_column)
    */
  return shared_ptr<Ast>(new Entity(lint->value + rint->value, Location()));
}

shared_ptr<Ast> HyphenBinopSubtractInts(const Ast* const lhs, const Ast* const rhs)
{
  const Entity* lentity = dynamic_cast<const Entity*>(lhs);
  const Entity* rentity = dynamic_cast<const Entity*>(rhs);

  if (!lentity)
    throw "Bad Lhs Ast";

  if (!rentity)
    throw "Bad Rhs Ast";

  const Integer* lint = dynamic_cast<Integer*>(lentity->literal.get());
  const Integer* rint = dynamic_cast<Integer*>(rentity->literal.get());

  if (!lint)
    throw "Bad Object Type";

  if (!rint)
    throw "Bad Object Type";

  return shared_ptr<Ast>(new Entity(lint->value - rint->value, Location()));
}

shared_ptr<Ast> FSlashBinopDivideInts(const Ast* const lhs, const Ast* const rhs)
{
  const Entity* lentity = dynamic_cast<const Entity*>(lhs);
  const Entity* rentity = dynamic_cast<const Entity*>(rhs);

  if (!lentity)
    throw "Bad Lhs Ast";

  if (!rentity)
    throw "Bad Rhs Ast";

  const Integer* lint = dynamic_cast<Integer*>(lentity->literal.get());
  const Integer* rint = dynamic_cast<Integer*>(rentity->literal.get());

  if (!lint)
    throw "Bad Object Type";

  if (!rint)
    throw "Bad Object Type";

  return shared_ptr<Ast>(new Entity(lint->value / rint->value, Location()));
}

shared_ptr<Ast> StarBinopMultiplyInts(const Ast* lhs, const Ast* rhs)
{
  const Entity* lentity = dynamic_cast<const Entity*>(lhs);
  const Entity* rentity = dynamic_cast<const Entity*>(rhs);

  if (!lentity)
    throw "Bad Lhs Ast";

  if (!rentity)
    throw "Bad Rhs Ast";

  const Integer* lint = dynamic_cast<Integer*>(lentity->literal.get());
  const Integer* rint = dynamic_cast<Integer*>(rentity->literal.get());

  if (!lint)
    throw "Bad Object Type";

  if (!rint)
    throw "Bad Object Type";

  return shared_ptr<Ast>(new Entity(lint->value * rint->value, Location()));
}

shared_ptr<Ast> PercentBinopModulusInts(const Ast* lhs, const Ast* rhs)
{
  const Entity* lentity = dynamic_cast<const Entity*>(lhs);
  const Entity* rentity = dynamic_cast<const Entity*>(rhs);

  if (!lentity)
    throw "Bad Lhs Ast";

  if (!rentity)
    throw "Bad Rhs Ast";

  const Integer* lint = dynamic_cast<Integer*>(lentity->literal.get());
  const Integer* rint = dynamic_cast<Integer*>(rentity->literal.get());

  if (!lint)
    throw "Bad Object Type";

  if (!rint)
    throw "Bad Object Type";

  return shared_ptr<Ast>(new Entity(lint->value % rint->value, Location()));
}

shared_ptr<Ast> EqualsBinopEquivalentInts(const Ast* lhs, const Ast* rhs)
{
  const Entity* lentity = dynamic_cast<const Entity*>(lhs);
  const Entity* rentity = dynamic_cast<const Entity*>(rhs);

  if (!lentity)
    throw "Bad Lhs Ast";

  if (!rentity)
    throw "Bad Rhs Ast";

  const Integer* lint = dynamic_cast<Integer*>(lentity->literal.get());
  const Integer* rint = dynamic_cast<Integer*>(rentity->literal.get());

  if (!lint)
    throw "Bad Object Type";

  if (!rint)
    throw "Bad Object Type";

  return shared_ptr<Ast>(new Entity(lint->value == rint->value, Location()));
}

void RegisterPrimitiveBinops(Environment env)
{
  auto IntegerType = shared_ptr<Type>(new MonoType(AtomicType::Int, Location()));
  auto BooleanType = shared_ptr<Type>(new MonoType(AtomicType::Bool, Location()));

  auto EqualsBinop = shared_ptr<BinopEliminatorSet>(new BinopEliminatorSet());
  // the formal argument list reflects the
  // function type, as in, the type of the elimination
  // procedure is:
  // arg1 -> arg2 -> arg3
  EqualsBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, BooleanType, EqualsBinopEquivalentInts);
  env.binops->RegisterBinop("=", EqualsBinop);
  env.precedences->RegisterBinopPrecAndAssoc("=", 1, Associativity::Left);

  auto PlusBinop = shared_ptr<BinopEliminatorSet>(new BinopEliminatorSet());
  PlusBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, IntegerType, PlusBinopAddInts);
  env.binops->RegisterBinop("+", PlusBinop);
  env.precedences->RegisterBinopPrecAndAssoc("+", 5, Associativity::Left);

  auto HyphenBinop = shared_ptr<BinopEliminatorSet>(new BinopEliminatorSet());
  HyphenBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, IntegerType, HyphenBinopSubtractInts);
  env.binops->RegisterBinop("-", HyphenBinop);
  env.precedences->RegisterBinopPrecAndAssoc("-", 5, Associativity::Left);

  auto FSlashBinop = shared_ptr<BinopEliminatorSet>(new BinopEliminatorSet());
  FSlashBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, IntegerType, FSlashBinopDivideInts);
  env.binops->RegisterBinop("/", FSlashBinop);
  env.precedences->RegisterBinopPrecAndAssoc("/", 6, Associativity::Left);

  auto StarBinop = shared_ptr<BinopEliminatorSet>(new BinopEliminatorSet());
  StarBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, IntegerType, StarBinopMultiplyInts);
  env.binops->RegisterBinop("*", StarBinop);
  env.precedences->RegisterBinopPrecAndAssoc("*", 6, Associativity::Left);

  auto PercentBinop = shared_ptr<BinopEliminatorSet>(new BinopEliminatorSet());
  PercentBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, IntegerType, PercentBinopModulusInts);
  env.binops->RegisterBinop("%", PercentBinop);
  env.precedences->RegisterBinopPrecAndAssoc("%", 6, Associativity::Left);
}

void RegisterPrimitiveUnops(Environment ops)
{

}
