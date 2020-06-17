

#include "pink_kernel.h"
#include "ast.h"
#include "symboltable.h"
#include "stringset.h"

void InitEnvWithKernel (Symboltable* env)
{
  /*
    bind can theoretically be considered a binary operator,
    but then what is the type of an id? (the lhs of the := binop)
     if we take the
    obvious route and give id's some type, say T_ID.
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
     that the names are really only truly needed by the grammar.
    they are very useful for humans however.)
    again, not an intractible problem, it's just not a problem
    that needs to be solved right now.
  */

   /*
   the only binop in v0.0.1 is the type operator '->'
   at first, bind was going to be a binop as well but
   it isn't in the end (see above).

   the type operator '->' takes two types as argument
   T1, T2, and returns the type T1 -> T2
   which is itself a type, we could name it T3 if
   we wanted.
   which would give the operator '->' the type
   T1 -> T2 -> T3
   or equivalently
   T1 -> T2 -> (T1 -> T2)?

   this makes sense given the straightforward type
   of any given binop, which is also T1 -> T2 -> T3
   */

}

void InitializePrecedenceTable(PrecedenceTable* pTable)
{
  InsertOpPrec(pTable, "->", 5, A_RIGHT);
}

void InitializeBinops(StringSet* binopSet)
{
  appendStr("->", binopSet);
}

void InitializeUnops(StringSet* unopSet)
{

}
