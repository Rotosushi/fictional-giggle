#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "stringset.h"

StringSet* createStringSet()
{
  StringSet* res = (StringSet*)malloc(sizeof(StringSet));
  res->root = NULL;
  res->end  = NULL;
  res->len  = 0;
  return res;
}

StringSet* copyStringSet(StringSet* s)
{
  StringSet* res = createStringSet();
  if (s == NULL) return res;
  Elem* elem = s->root;
  while (elem != NULL) {
    appendStr(elem->str, res);
    elem = elem->next;
  }
  return res;
}

void appendStr(char* s, StringSet* strset)
{
  Elem* elem = (Elem*)malloc(sizeof(Elem));
  elem->str  = strdup(s);
  elem->next = NULL;

  if (strset->root == NULL) {
    strset->root = elem;
    strset->end  = elem;
    strset->len  = 1;
  }
  else {
    strset->end->next = elem;
    strset->end = elem;
    strset->len += 1;
  }
}

void removeStr(char* s, StringSet* strset)
{
  Elem* prv = NULL, *ths = strset->root;
  int cmp   = strcmp(s, ths->str);

  if (cmp == 0) {
    strset->root = strset->root->next;
    free(ths->str);
    free(ths);
    strset->len -= 1;
  }
  else while (ths != NULL) {
    if (cmp == 0) {
      if (ths->next == NULL) {
        strset->end = prv;
        prv->next   = NULL;
        free(ths->str);
        free(ths);
        strset->len -= 1;
      } else {
        prv->next = ths->next;
        free(ths->str);
        free(ths);
        strset->len -= 1;
      }
      break;
    }
    prv = ths;
    ths = ths->next;
    cmp = strcmp(s, ths->str);
  }
}

StringSet* unionStrSets (StringSet* s1, StringSet* s2)
{
  /* the elements of both sets (no repeats) */
  if (s1 == NULL) {
    if (s2 == NULL)
      return NULL;
    else
      return s2;
  }
  else if (s2 == NULL)
    return s1;

  StringSet* sr = createStringSet();
  Elem *s1e = s1->root, *s2e = s2->root;

  /* is the first list populated? */
  if (s1e == NULL) {
    /* is either list populated? */
    if (s2e == NULL) {
      return sr;
    }
    /* the first list is empty, the second isn't
       so we can just copy the second list */
    else
      while (s2e != NULL)
        appendStr(s2e->str, sr);
  }
  /* the first list has elems, does the second? */
  else if (s2e == NULL)
    /* the first list isn't empty, but the second
       list is, so we copy the first list */
    while (s1e != NULL)
      appendStr(s1e->str, sr);
  /* both lists have elems */
  else {
    /* appendStr all members of the first list */
    while (s1e != NULL) {
      appendStr(s1e->str, sr);
      s1e = s1e->next;
    }

    /* appendStr every elem not already a member from the second list */
    while (s2e != NULL) {
      if (!isMember(s2e->str, sr))
        appendStr(s2e->str, sr);

      s2e = s2e->next;
    }
  }
  return sr;
}

StringSet* intersectionStrSets (StringSet* s1, StringSet* s2)
{
  /* the elements that are in both lists are in the result */

  if (s1 == NULL) {
    if (s2 == NULL)
      return NULL;
    else
      return createStringSet();
  }
  else
    return createStringSet();

    StringSet* sr = createStringSet();
    Elem *s1e = s1->root, *s2e = s2->root;

    /* is the first list populated? */
    if (s1e == NULL) {
      /* is either list populated? */
      if (s2e == NULL) {
        return sr;
      }
      /* the first list is empty, the intersection
         is empty
      */
      else
        return createStringSet();
    }
    /* the first list has elems, does the second? */
    else if (s2e == NULL)
      /* the first list isn't empty, but the second
         list is, so the result is empty */
      return createStringSet();
    /* both lists have elems */
    else {
      /* appendStr all members of the first list
         which are members of the second list */
      while (s1e != NULL) {
        if (isMember(s1e->str, s2))
          appendStr(s1e->str, sr);

        s1e = s1e->next;
      }
    }
    return sr;

}

StringSet* complementStrSets (StringSet* s1, StringSet* s2)
{
  /* s1 but remove all elements of s1 that are in s2 */

  if (s1 == NULL) {
    if (s2 == NULL)
      return NULL;
    else
      return createStringSet();
  }
  else if (s2 == NULL)
    return copyStringSet(s1);

  StringSet* sr = createStringSet();
  Elem *s1e = s1->root, *s2e = s2->root;

  /* is the first list populated? */
  if (s1e == NULL) {
    /* is either list populated? */
    if (s2e == NULL) {
      return sr;
    }
    /* the first list is empty,
       the compliment is empty
    */
    else
      return sr;
  }
  /* the first list has elems, does the second? */
  else if (s2e == NULL) {
    /* the first list isn't empty, but the second
       list is, so the result is the first list */
    while(s1e != NULL) {
      appendStr(s1e->str, sr);

      s1e = s1e->next;
    }
  }
  /* both lists have elems */
  else {

    /* appendStr all members of the first list
       which are not members of the second list */
    while (s1e != NULL) {
      if (!isMember(s1e->str, s2))
        appendStr(s1e->str, sr);

      s1e = s1e->next;
    }
  }
  return sr;
}

bool isMember (char* s, StringSet* strset)
{
  if (strset == NULL)
    return false;
  if (strset->root == NULL)
    return false;
  if (s == NULL)
    return false;

  Elem* elem = strset->root;
  if (elem->str != NULL) {
    int cmp = strcmp(s, elem->str);
    if (cmp == 0) {
      return true;
    }
    else {
        elem = elem->next;
        while (elem != NULL) {
          if (elem->str != NULL) {
            cmp  = strcmp(s, elem->str);
            if (cmp == 0) return true;
            elem = elem->next;
          }
        }
    }
  }
  return false;
}

bool is_subset (StringSet* s1, StringSet* s2)
{
  /* if every element of s1 can be found in s1
     s1 is a proper subset of s2. */
     return false;
}

void DestroyStringSet (StringSet* s)
{
  Elem *prv = NULL, *ths = s->root;
  while (ths != NULL) {
    prv = ths;
    ths = ths->next;
    free(prv->str);
    free(prv);
  }
  s->root = NULL;
  s->end  = NULL;
  s->len  = 0;
}
