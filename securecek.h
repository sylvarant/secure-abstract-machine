/*
 * =====================================================================================
 *
 *       Filename:  seccek.h
 *
 *    Description:  internal header for the cek implementation of MiniML
 *
 *         Author:  tea
 *        Company:  Superstar Uni
 *
 * =====================================================================================
 */

#ifndef SECCEK_INCLUDED
#define SECCEK_INCLUDED

// understand other side
#include "attackerlang.h"

/*-----------------------------------------------------------------------------
 * Environment + Storage + Functions - We use the same data structure
 * to reduce the amount of namemap and structures that we need
 *-----------------------------------------------------------------------------*/
struct secenvnode {
    char *key;
    void * value;
    struct secenvnode *next;
};

typedef struct secenviron_t {
    struct secenvnode *bucket;
    int (*cmp)(void *,void*);
    int size;
}secenviron;

/*-----------------------------------------------------------------------------
 *  Control Expressions of the secure lambda langauge
 *-----------------------------------------------------------------------------*/
struct secBoolean ;
struct secClosure ;
struct secLambda;
union secValue ;
struct secSymbol;
struct secApplication; 
struct secError;
struct secIf;
struct secLet;

enum secTag { SECBOOLEAN, SECCLOSURE, SECLAM, SECSYMBOL, SECAPPLICATION, SECLET, SECIF, INSEC} ;

union secValue {
    struct secBoolean * b;
    struct secLambda * l;
    struct secSymbol * s;
    struct secApplication * a;
    struct secClosure * c;
    struct secError * e;
    struct secIf * ii;
    struct secLet * lt;
    struct SI * i;
};

struct secBoolean {
    enum secTag t ;
    unsigned int value ;
};

struct secLambda {
    enum secTag t;
    union secValue body;
    union secValue argument; 
};

struct secSymbol {
    enum secTag t;
    char * name;  
};

struct secApplication {
    enum secTag t;
    union secValue function;
    union secValue argument;
};

struct secClosure {
    enum secTag t;
    union secValue x;
    union secValue body; 
    secenviron  * env;
};

struct secLet {
    enum secTag t;
    union secValue var;
    union secValue expr;
    union secValue body;
};

struct secIf {
    enum secTag t;
    union secValue cond;
    union secValue cons;
    union secValue alt;
};

struct SI{
    enum secTag t;
    union Value term;
};

struct secError{
    enum secTag t;
    char * message;
};

// simplifications
typedef union secValue secValue;


/*-----------------------------------------------------------------------------
 *  Machine Parts 
 *-----------------------------------------------------------------------------*/
typedef union kont_t{
    struct exec1 * e;
    struct execlo * c;
    struct exret * r;
    struct excont * cc;
    struct exlet * lt;
    struct exif * ii;
}kont;

enum Tagk { SEXEC1,SEXECLO,SERET,SECONT, SELET, SEIF };

struct exret{
    enum Tagk t;
    kont * k;  
};

struct excont{
   enum Tagk t;
   secenviron * env;
   kont * k;
};

struct exec1 {
    enum Tagk t; 
    secValue expr;
    secenviron * env;
    kont * k;
};

struct execlo {
    enum Tagk t; 
    secValue expr;
    kont * k;
};

struct exlet{
    enum Tagk t;
    kont * k;
    secenviron * env;
    secValue var;
    secValue body;
};

struct exif{
    enum Tagk t;
    kont * k;
    secenviron * env;
    secValue cons;
    secValue alt;
};

// cek State
typedef struct state_t{
//    secValue * control;
    secenviron * environment; 
    secenviron * namemap;
    kont * continuation;
}state;

#endif
