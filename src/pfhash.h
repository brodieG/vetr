/*
copyright paxdiablo
lifted directly from: http://powerfield-software.com/?p=615 under license:

> Brodie, the code I hold copyright for in this article (which is the bulk of
> it) is covered by the “do whatever the heck you want with it” licence, the
> official text of which is:
>
> 1/ You are hereby permitted to do whatever the heck you want with it.
>
> I make no representations about the actual hashing functions themselves,
> defaultFnKnR and defaultFnBJ. If you want to ensure you’re safe in respect to
> those, either consult a lawyer, write your own, or see the addendum to this
> article.
*/

#include <R.h>
#include <Rinternals.h>
#include <ctype.h>
#include <stdint.h>

#ifndef _PFHASH_H
#define _PFHASH_H


    typedef struct sPfHashNode {
        const char *key;
        const char *data;
        struct sPfHashNode *next;
    } pfHashNode;

    typedef struct {
        uint32_t (*fn) (const char *);
        pfHashNode *lookup[];
    } pfHashTable;

    pfHashTable *pfHashCreate (uint32_t(*)(const char*));
    // void pfHashDestroy (pfHashTable*);
    int pfHashSet (pfHashTable*,const char*,const char*);
    // int pfHashDel (pfHashTable*,char*);
    const char *pfHashFind (pfHashTable*,const char*);
    void pfHashDebug (pfHashTable*,char*);
    SEXP pfHashTest(SEXP keys, SEXP values);

#endif
