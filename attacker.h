/*
 * =====================================================================================
 *
 *       Filename:  cek.h
 *
 *    Description:  Header file for the regular cesk machine
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala Uni
 *
 * =====================================================================================
 */

#ifndef CEK_INCLUDED
#define CEK_INCLUDED

#include "environment.h"
#include "commonlang.h"
#include "FFI.h"
#include <stdio.h>

#ifdef STATIC_MEM
    #define mysize unsigned int
    extern void * staticmalloc(mysize);
    #define mymalloc staticmalloc
#else
    #include <stdlib.h>
    #define mymalloc malloc
    #define mysize size_t
#endif 

/*-----------------------------------------------------------------------------
 *  Structure Definitions
 *-----------------------------------------------------------------------------*/

// K
typedef union kont_t{
    struct exec1 * e;
    struct execlo * c;
    struct exret * r;
    struct excont * cc;
}kont;

enum Tagk { EXEC1 , EXECLO, RET, CONT } ;

struct exec1 {
    enum Tagk t; 
    Value expr;
    environ * env;
    kont * k;
};

struct execlo {
    enum Tagk t; 
    Value expr;
    kont * k;
};

struct exret{
	enum Tagk t;
	kont * k;
};

struct excont{
	enum Tagk t;
	kont * k;
	environ * env;
};

// CEK State
typedef struct state_t{
    environ * environment; 
    kont * continuation;
}state;


/*-----------------------------------------------------------------------------
 *  Functions
 *-----------------------------------------------------------------------------*/
extern void * evaluate(void *);
extern Value run(Value expr);
extern state * startstate(void);

// create structures
extern Value MakeBoolean(unsigned int b);
extern Value MakeLambda(Value body,Value arg);
extern Value MakeSymbol(char *);
extern Value MakeApplication(Value a,Value b);
extern Value MakeIS(int f);

#endif

