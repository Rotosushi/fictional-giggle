

#include "pink_kernel.h"
#include "ast.h"
#include "symboltable.h"

void InitEnvWithKernel (Symboltable* env)
{
  /*
    bind can theoretically be considered a binary operator,
    but then what is the type of an id? (the lhs of the := binop)
     if we take the
    obvious route and give it some type say T_ID.
    then we are letting an id have type id
    and be assigned a term, which could itself have
    type T_ID. well, if id's have type ID, then what
    does it mean to type an expression containing ID's?
    even simpler, what does it mean to type an id?
    do we return the type of the bound term or just T_ID?
    so much of the usefullness of binding comes directly
    from the fact that we assume replacement.
    this doesn't seem like an intractable problem, it just
    muddies the clarity of the design. so, ultimately that
    is why binds are being considered a grammar construct
    instead of a primary construct. is that they deal directly
    with information relevant to the grammar and execution
    (but, purely combinatorial languages like SKI, point out
     that the names are really only truly needed by the grammar.)
  */

}

void InitializePrecedenceTable(PrecedenceTable* pTable)
{
  /*
  my previous attempt at the precedence table
  obviously we want to maintain the same precedence
  relations between the common math symbols. and also
  understanding that for many, the c precedence table
  is de-facto standard for programming languages.

  so, starting from the perspective of emulating mathematics
  we want to preform basic actions on numbers/entities.
  3 + 4 * 5 HAS TO parse to (* 5 (+ 3 4))
  when a programmer wants to preform bitwise operations
  on numbers, they probably want the bitwise operations to
  operate on numbers which have already been manipulated
  to some final value.
  x || y * z,
  likewise when we go up a step to the logical connectives,
  we want to test for truth and falsehood upon values which
  have been fully operated upon. a + b > c * d.

  the same argument is made when we take another step up to
  equality comparison.
  say to the case of
  a + b > c * d == e - f < g \ h

  we want to test for equality between the largest lhs and right we can
  group together. because we are assuming the programmer wants to compare
  between fully evaluated terms, which implies gathering as many operations
  into the evaluation tree before we insert the comparison operation.

  and, the same argument extends to the programming language specific operators
  ',', ';', ':=', '[', ']', '(', ')', '{', '}'
  and the type connectives '->', '+', '|', '&', '!'

  why do I invert the logical symbols and the bitwise symbols?
  well, for logical consistency, and the fact that logical connectives
  are more common than bitwise operations, by a wide-margin.
  (and now, LSHIFT <<, and RSHIFT >>, align with the rest of the
   bitwise operators OR ||, and &&, and xor ^^, in being two symbol operators.
  )
  when we consider equals and not equals, =, ~= resp. one can probably intuit
  the meaning of the compound symbol ~= just from knowing that = means equal-to
  and ~ means logical-negation. this also aligns with ~ being the logical not.
  instead of ! as in c. why is that? well, the operator *, is used to represent
  type kinds in the theory, and it would be nice to align the language to some
  theory symbolically. since we are deciding to reapropriate * for types, then
  it could create confusion to also use it as the indirection unop. so we will
  pay some homage to ML by taking ! to be the indirection operator. which takes
  ! away from the logical commectives, because again too much overloading
  increases the cognitive complexity of the kernel, and so we must select
  a new symbol for logical-negation.
  hence, ~ for logical-negation, and ~~ for bitwise negation. notice how
  this aligns with every bitwise operator, being composed of a repetition
  of some other operator. when a programmer sees a || instead of a | it should
  always be able to be read as a bitwise operation. this at least holds for the
  kernel, obviously if we give programmers the tools to both overload existing
  operators, and define new operators, they can define new operators which
  negate the truth of the above statement quite easily. but that is separate
  from the logical consistency of the kernel.

  (normally in ML ~ is the unop minus to the binop minus -, we just use the
   fact that unary minus always appears in prefix position and disambiguate
   by the position instead of symbol the operation to be carried out. meaning
   both forms of minus are symbolically stated by the - symbol, this is in
   some sense of the word, overloading the symbol with two meanings, however
   since the two meanings are entirely disambiguated by the grammar there is no
   need to consider any special logic to support the two definitions, we can
   )


    ptable[T_COMMA] = 1;
  	ptable[T_EQ] = 2;

  	ptable[T_EQUALS] = 3;
  	ptable[T_NOT_EQUALS] = 3;

  	ptable[T_LESS] = 4;
  	ptable[T_GREATER] = 4;
  	ptable[T_LESS_EQUALS] = 4;
  	ptable[T_GREATER_EQUALS] = 4;

  	ptable[T_OR] = 5;
  	ptable[T_XOR] = 6;
  	ptable[T_AND] = 7;

  	ptable[T_BIT_OR] = 8;
  	ptable[T_BIT_XOR] = 9;
  	ptable[T_BIT_AND] = 10;

  	ptable[T_BIT_LSHIFT] = 11;
  	ptable[T_BIT_RSHIFT] = 11;

  	ptable[T_ADD] = 12;
  	ptable[T_SUB] = 12;

  	ptable[T_MULT] = 13;
  	ptable[T_DIV] = 13;
  	ptable[T_MOD] = 13;
  */
  InsertOpPrec(pTable, "->", 5, A_RIGHT);
}

void InitializeBinops(StringSet* binopSet)
{
  appendStr(binopSet, "->");
}

void InitializeUnops(StringSet* unopSet)
{

}
