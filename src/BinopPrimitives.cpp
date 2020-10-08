
#include "Ast.hpp"
#include "Entity.hpp"
#include "SymbolTable.hpp"
#include "BinopEliminators.hpp
#include "BinopPrecedenceTable.hpp"



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
    throw "Bad Lhs Ast";

  if (!rentity)
    throw "Bad Rhs Ast";

  return make_shared(Entity(lentity->literal->value * rentity->literal->value));
}

void RegisterPrimitiveBinops(BinopSet* Binops, BinopPrecedenceTable* BinopPrecTable)
{
  auto IntegerType = make_shared(MonoType(AtomicType::Int));

  auto EqualsBinop = make_shared(BinopEliminatorSet());
  EqualsBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, EqualsBinopEquivalentInts);
  Binops->RegisterBinopEliminatorSet("=", EqualsBinop);
  BinopPrecTable->RegisterBinopPrecAndAssoc("=", 1, Associativity::Left);

  auto PlusBinop = make_shared(BinopEliminatorSet());
  PlusBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, PlusBinopAddInts);
  Binops->RegisterBinopEliminatorSet("+", PlusBinop);
  BinopPrecTable->RegisterBinopPrecAndAssoc("+", 5, Associativity::Left);

  auto HyphenBinop = make_shared(BinopEliminatorSet());
  HyphenBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, HyphenBinopSubtractInts);
  Binops->RegisterBinopEliminatorSet("-", HyphenBinop);
  BinopPrecTable->RegisterBinopPrecAndAssoc("-", 5, Associativity::Left);

  auto FSlashBinop = make_shared(BinopEliminatorSet());
  FSlashBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, FSlashBinopDivideInts);
  Binops->RegisterBinopEliminatorSet("/", FSlashBinop);
  BinopPrecTable->RegisterBinopPrecAndAssoc("/", 6, Associativity::Left);

  auto StarBinop = make_shared(BinopEliminatorSet());
  StarBinop->RegisterPrimitiveEliminator(IntegerType, IntegerType, StarBinopMultiplyInts);
  Binops->RegisterBinopEliminatorSet("*", StarBinop);
  BinopPrecTable->RegisterBinopPrecAndAssoc("*", 6, Associativity::Left);
}

void RegisterPrimitiveUnops(SymbolTable* top)
{

}
