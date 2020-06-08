#ifndef STRINGSET_H
#define STRINGSET_H
#include <stdbool.h>

typedef struct Elem {
  char* str;
  struct Elem* next;
} Elem;

typedef struct StringSet {
  Elem* root;
  Elem* end;
  int len;
} StringSet;

StringSet* createStringSet();
StringSet* copyStringSet(StringSet* s);

void appendStr(char* s, StringSet* strset);
void removeStr(char* s, StringSet* strset);

StringSet* unionStrSets (StringSet* s1, StringSet* s2);
StringSet* intersectionStrSets(StringSet* s1, StringSet* s2);
StringSet* complementStrSets  (StringSet* s1, StringSet* s2);

bool isMember(char* s, StringSet* strset);
bool isSubset(StringSet* s1, StringSet* s2);
bool isEquvalentSet(StringSet* s1, StringSet* s2);

int  lengthOfStrset(StringSet* s);

void DestroyStringSet(StringSet* s);

#endif
