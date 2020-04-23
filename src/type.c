#include "type.h"

void InitFuncType (Type* type, Type T1, Type T2)
{
  type->kind = K_FUNC;
  type->b.ft.T1 = T1;
  type->b.ft.T2 = T2;
}

void InitBaseType (Type* type, BaseTypeTag type_tag)
{
  type->kind = K_DATA;
  type->b.bt.tag = type_tag;
}
