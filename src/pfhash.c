/*
copyright paxdiablo
lifted directly from: http://powerfield-software.com/?p=615
*/

#include "pfhash.h"

// Simple hash function from K&R.

// static uint32_t defaultFnKnR (char *key) {
//     static int HASHSZ = 101;

//     // Return table size if no data to hash.

//     if (key == NULL) return HASHSZ;

//     // Otherwise hash data.

//     uint32_t hashval;
//     for (hashval = 0; *key != '\0'; key++)
//         hashval = (uint32_t)(*key) + 31 * hashval;
//     return hashval % HASHSZ;
// }

// Bob Jenkins' hashing function.
// This has a good distribution and doesn't need to
//   be modded with a prime. It's enough just to use
//   bitwise operations, so this one allows for a
//   table size of 256 (by bitwise-and with 0xff).

#define mixBits(a,b,c) { \
    a -= b; a -= c; a ^= (c >> 13); \
    b -= c; b -= a; b ^= (a <<  8); \
    c -= a; c -= b; c ^= (b >> 13); \
    a -= b; a -= c; a ^= (c >> 12); \
    b -= c; b -= a; b ^= (a << 16); \
    c -= a; c -= b; c ^= (b >>  5); \
    a -= b; a -= c; a ^= (c >>  3); \
    b -= c; b -= a; b ^= (a << 10); \
    c -= a; c -= b; c ^= (b >> 15); \
}

static uint32_t defaultFnBJ (const char *key) {
    uint32_t a = 0x9e3779b9;
    uint32_t b = 0x9e3779b9;
    uint32_t c = 0;

    // Return table size if no data to hash.

    if (key == NULL) return 256;

    // Otherwise hash data.

    uint32_t sz = strlen (key);
    size_t left = sz;

    while (left >= 12) {
        a +=   (uint32_t)key[ 0]
            + ((uint32_t)key[ 1] <<  8)
            + ((uint32_t)key[ 2] << 16)
            + ((uint32_t)key[ 3] << 24);
        b +=   (uint32_t)key[ 4]
            + ((uint32_t)key[ 5] <<  8)
            + ((uint32_t)key[ 6] << 16)
            + ((uint32_t)key[ 7] << 24);
        c +=   (uint32_t)key[ 8]
            + ((uint32_t)key[ 9] <<  8)
            + ((uint32_t)key[10] << 16)
            + ((uint32_t)key[11] << 24);
        mixBits (a, b, c);
        key += 12;
        left -= 12;
    }

    c += sz;
    switch (left) {
        case 11 : c += ((uint32_t)key[10] << 24);
        case 10 : c += ((uint32_t)key[ 9] << 16);
        case  9 : c += ((uint32_t)key[ 8] <<  8);
        case  8 : b += ((uint32_t)key[ 7] << 24);
        case  7 : b += ((uint32_t)key[ 6] << 16);
        case  6 : b += ((uint32_t)key[ 5] <<  8);
        case  5 : b +=  (uint32_t)key[ 4];
        case  4 : a += ((uint32_t)key[ 3] << 24);
        case  3 : a += ((uint32_t)key[ 2] << 16);
        case  2 : a += ((uint32_t)key[ 1] <<  8);
        case  1 : a +=  (uint32_t)key[ 0];
    }
    mixBits (a, b, c);

    // Return hash, adjusted for table size.

    return c & 0xff;
}
// Local function to locate a key, will populate
//   hash entry, node before and node matching.
// If node matching is null, it wasn't found.
// If node before is null, it was the first at that
//   entry.

static void locate (pfHashTable *tbl, const char *key,
    int *pEntry, pfHashNode **pPrev,
    pfHashNode **pNode)
{
    // Get the hash entry as first step.

    *pEntry = tbl->fn (key);

    // Iterate through list at that entry until
    // you find key, or reach end.

    *pPrev = NULL;
    *pNode = tbl->lookup[*pEntry];
    while (*pNode != NULL) {
        if (strcmp (key, (*pNode)->key) == 0)
            break;
        *pPrev = *pNode;
        *pNode = (*pNode)->next;
    }
}

// In case your C implementation doesn't have strdup,
//   we use this one.

static char *dupstr (const char *str) {
    char *newstr = R_alloc (strlen (str) + 1, sizeof(char));
    if (newstr != NULL)
        strcpy (newstr, str);
    return newstr;
}

// Create a hash table, giving only the hashing
//   function.

pfHashTable *pfHashCreate (uint32_t (*fn)(const char *)) {
    // Use default if none given, and get number
    //   of entries allowed.

    if (fn == NULL)
        fn = defaultFnBJ;
    uint32_t numEntries = fn (NULL);

    // Allocate the hash table, including entries
    //   for lists of nodes.

    pfHashTable *tbl = (void *) R_alloc (1, sizeof (pfHashTable)
        + numEntries * sizeof (pfHashNode*));
    if (tbl == NULL) return NULL;  // nocov

    // Store function and set hash entries to empty.

    tbl->fn = fn;

    for (uint32_t i = 0; i < numEntries; i++)
        tbl->lookup[i] = NULL;

    return tbl;
}

// Destroys a hash table, freeing all data.
/*
void pfHashDestroy (pfHashTable *tbl) {
    error("hash table destroy disabled as it should be handled by R");

    // Get size first.

    uint32_t numEntries = tbl->fn (NULL);

    // For each lookup entry, free its node list.

    for (uint32_t i = 0; i < numEntries; i++) {
        // Iterate through the linked list,
        //   freeing one node at a time.

        pfHashNode *node = tbl->lookup[i];
        while (node != NULL) {
            pfHashNode *next = node->next;
            // free (node->key);
            // free (node->data);
            // free (node);
            node = next;
        }
    }
}
*/

// Set a hash value (key/data), creating it if it doesn't
//   already exist.

int pfHashSet (pfHashTable *tbl, const char *key, const char *data) {
    int entry;
    pfHashNode *prev, *node;

    locate (tbl, key, &entry, &prev, &node);

    if (node != NULL) {
        char *newdata = dupstr (data);
        if (newdata == NULL)
            return -1;  // nocov
        // free (node->data);
        node->data = newdata;
        return 0;
    }

    node = (void *) R_alloc (1, sizeof (pfHashNode));
    if (node == NULL)
        return -1;  // nocov

    node->key = dupstr (key);
    node->data = dupstr (data);
    if ((node->key == NULL) || (node->data == NULL)) {
        // free (node->key);
        // free (node->data);
        // free (node);
        return -1;  // nocov
    }

    node->next = tbl->lookup[entry];
    tbl->lookup[entry] = node;

    return 0;
}

// Delete a hash entry, returning error if not found.

/*
int pfHashDel (pfHashTable *tbl, char *key) {
    int entry;
    pfHashNode *prev, *node;

    locate (tbl, key, &entry, &prev, &node);

    if (node == NULL)
        return -1;

    if (prev != NULL)
        prev->next = node->next;
    else
        tbl->lookup[entry] = node->next;

    // Relying on R to clear this up at tend of fun call

    // free (node->key);
    // free (node->data);
    // free (node);

    return 0;
}
*/

// Find a hash entry, and return the data. If not found,
//   returns NULL.

const char *pfHashFind (pfHashTable *tbl, const char *key) {
    int entry;
    pfHashNode *prev, *node;

    locate (tbl, key, &entry, &prev, &node);

    if (node == NULL)
        return NULL;

    return node->data;
}

// Output debugging info about the hash table.
/*
void pfHashDebug (pfHashTable *tbl, char *desc) {
    // Get size first.

    uint32_t numEntries = tbl->fn (NULL);

    Rprintf ("=====: %s %d entries\n", desc, numEntries);

    // For each lookup entry, free the list.

    for (uint32_t i = 0; i < numEntries; i++) {
        // Only output if there are nodes.

        if (tbl->lookup[i] != NULL) {
            int sz = 0;
            Rprintf ("Entry #%3d:", i);
            pfHashNode *node = tbl->lookup[i];
            while (node != NULL) {
                Rprintf (" ['%s' = '%s']", node->key, node->data);
                node = node->next;
                sz++;
            }
            Rprintf (", size=%d\n", sz);
        }
    }
    Rprintf ("\n");
}
*/
/*
 * Test out the hash scripts
 */

SEXP pfHashTest(SEXP keys, SEXP values) {
  pfHashTable * hash = pfHashCreate(NULL);

  if(TYPEOF(keys) != STRSXP) error("Argument `keys` must be a string");
  if(TYPEOF(values) != STRSXP) error("Argument `values` must be a string");
  if(XLENGTH(keys) != XLENGTH(values)) error("Arguments must be same length");

  R_xlen_t ki;

  for(ki = 0; ki < XLENGTH(keys); ki++) {
    const char * key = CHAR(STRING_ELT(keys, ki));
    const char * value = CHAR(STRING_ELT(values, ki));

    pfHashSet(hash, key, value);
  }
  SEXP res = PROTECT(allocVector(STRSXP, XLENGTH(keys)));

  for(ki = 0; ki < XLENGTH(keys); ki++) {
    SET_STRING_ELT(
      res, ki, mkChar(pfHashFind(hash, CHAR(STRING_ELT(keys, ki))))
    );
  }
  UNPROTECT(1);
  return res;
}

