/* Pink: Type */

#ifndef TYPE_H
#define TYPE_H

/*
  A type has a name, but there are other 'kinds'
  of type as well. in this simple system,
  we only consider the single datatype 'Nil'
  and the functiontype type -> type
  the type definition mechanism will have
  the ability to name new datatypes. which
  can all be considered kind *
  as opposed to the functiontype which has
  kind * -> *.

*/

typedef enum TypeKind {
  K_UNDEF,
  K_FUNC,
  K_DATA
} TypeKind;

typedef enum BaseTypeTag {
  T_UNDEF,
  T_NIL
  /*
  Here is where Int, Real, Char, Text, and Bool will go.
  as well as s8/16/32/64 and u8/16/32/64.
  */
} BaseTypeTag;

struct Type;

typedef struct FuncType {
  Type T1;
  Type T2;
} FuncType;

typedef struct BaseType {
  BaseTypeTag tag;
} BaseType;

typedef struct Type {
  TypeKind kind;
  union {
    FuncType ft;
    BaseType bt;
  } b;
} Type;

void InitFuncType (Type* type, Type T1, Type T2);
void InitBaseType (Type* type, BaseTypeTag type_tag);

#endif /* !TYPE_H */
