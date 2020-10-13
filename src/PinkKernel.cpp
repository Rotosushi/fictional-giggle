

#include "Ast.hpp"
#include "Entity.hpp"
#include "SymbolTable.hpp"
#include "OperatorTable.hpp"
#include "PinkKernel.hpp"


shared_ptr<Ast> PlusBinopAddInts(const Ast* const lhs, const Ast* const rhs)
{
  Entity* lentity = dynamic_cast<Entity*>(lhs);
  Entity* rentity = dynamic_cast<Entity*>(rhs);

  if (!lentity)
    throw "Bad Lhs Ast";

  if (!rentity)
    throw "Bad Rhs Ast";

  return make_shared(Entity(lentity->literal->value + rentity->literal->value));
}

shared_ptr<Ast> HyphenBinopSubtractInts(const Ast* const lhs, const Ast* const rhs)
{
  Entity* lentity = dynamic_cast<Entity*>(lhs);
  Entity* rentity = dynamic_cast<Entity*>(rhs);

  if (!lentity)
    throw "Bad Lhs Ast";

  if (!rentity)
    throw "Bad Rhs Ast";

  return make_shared(Entity(lentity->literal->value - rentity->literal->value));
}

shared_ptr<Ast> FSlashBinopDivideInts(const Ast* const lhs, const Ast* const rhs)
{
  Entity* lentity = dynamic_cast<Entity*>(lhs);
  Entity* rentity = dynamic_cast<Entity*>(rhs);

  if (!lentity)
    throw "Bad Lhs Ast";

  if (!rentity)
    throw "Bad Rhs Ast";

  return make_shared(Entity(lentity->literal->value / rentity->literal->value));
}

shared_ptr<Ast> StarBinopMultiplyInts(const Ast* lhs, const Ast* rhs)
{
  Entity* lentity = dynamic_cast<Entity*>(lhs);
  Entity* rentity = dynamic_cast<Entity*>(rhs);

  if (!lentity)
    throw "Bad Lhs Ast\n";

  if (!rentity)
    throw "Bad Rhs Ast\n";

  return make_shared(Entity(lentity->literal->value * rentity->literal->value));
}

shared_ptr<Ast> PercentBinopModulusInts(const Ast* lhs, const Ast* rhs)
{
  Entity* lent = dynamic_cast<Entity*>(lhs);
  Entity* rent = dynamic_cast<Entity*>(rhs);

  if (!lent)
    throw "Bad Lhs Ast\n";

  if (!rent)
    throw "Bad Rhs Ast\n";

  return make_shared(Entity(lent->literal->value % rent->literal->value));
}

void RegisterPrimitiveBinops(OperatorTable* ops)
{
  auto IntegerType = make_shared(MonoType(AtomicType::Int));

  auto EqualsBinop = make_shared(BinopEliminatorSet());
  EqualsBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, EqualsBinopEquivalentInts);
  ops->binops.RegisterBinopEliminatorSet("=", EqualsBinop);
  ops->precedences.RegisterBinopPrecAndAssoc("=", 1, Associativity::Left);

  auto PlusBinop = make_shared(BinopEliminatorSet());
  PlusBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, PlusBinopAddInts);
  ops->binops.RegisterBinopEliminatorSet("+", PlusBinop);
  ops->precedences.RegisterBinopPrecAndAssoc("+", 5, Associativity::Left);

  auto HyphenBinop = make_shared(BinopEliminatorSet());
  HyphenBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, HyphenBinopSubtractInts);
  ops->binops.RegisterBinopEliminatorSet("-", HyphenBinop);
  ops->precedences.RegisterBinopPrecAndAssoc("-", 5, Associativity::Left);

  auto FSlashBinop = make_shared(BinopEliminatorSet());
  FSlashBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, FSlashBinopDivideInts);
  ops->binops.RegisterBinopEliminatorSet("/", FSlashBinop);
  ops->precedences.RegisterBinopPrecAndAssoc("/", 6, Associativity::Left);

  auto StarBinop = make_shared(BinopEliminatorSet());
  StarBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, StarBinopMultiplyInts);
  ops->binops.RegisterBinopEliminatorSet("*", StarBinop);
  ops->precedences.RegisterBinopPrecAndAssoc("*", 6, Associativity::Left);

  auto PercentBinop = make_shared(BinopEliminatorSet());
  PercentBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, );
}

void RegisterPrimitiveUnops(OperatorTable* ops)
{

}
