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

void append(char* s, StringSet* strset);
void remove(char* s, StringSet* strset);

StringSet* union       (StringSet* s1, StringSet* s2);
StringSet* intersection(StringSet* s1, StringSet* s2);
StringSet* complement  (StringSet* s1, StringSet* s2);

bool isMember(char* s, StringSet* strset);
bool isSubset(StringSet* s1, StringSet* s2);
bool isEquvalentSet(StringSet* s1, StringSet* s2);

int  lengthOfStrset(StringSet* s);

void destroyStringSet(StringSet* s);

#endif
