/*
 * =====================================================================================
 *
 *       Filename:  environment.h
 *
 *    Description:  Used to be a table
 *
 * =====================================================================================
 */

#ifndef TABLE_INCLUDED
#define TABLE_INCLUDED

#define NR_BUCKETS 1024

/*-----------------------------------------------------------------------------
 *  Types
 *-----------------------------------------------------------------------------*/
struct envnode {
    char *key;
    void * value;
    struct envnode *next;

};

#ifdef VAL_MEM_MANAG
struct keylist{
    char * key;
    struct keylist * next;
};
#endif

typedef struct environ_t {
    struct envnode *bucket;
    int size;
}environ;

/*-----------------------------------------------------------------------------
 *  Functions
 *-----------------------------------------------------------------------------*/
extern int insert(environ *table,char *key,void * value);
extern void * get(environ *table,const char *key);
extern void emptyenv(environ * table);
extern environ * copyenv(environ * table);


#endif
