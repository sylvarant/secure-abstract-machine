/*
 * =====================================================================================
 *
 *       Filename:  cek.h
 *
 *    Description:  Header file for the regular cek machine
 *
 *         Author:  tea
 *        Company:  Superstar Uni
 *
 * =====================================================================================
 */

#ifndef CEK_INCLUDED
#define CEK_INCLUDED

#include "environment.h"
#include "attackerlang.h"
#include "PMA.h"
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
    struct exlet * lt;
    struct exif * ii;
}kont;

enum Tagk { EXEC1 , EXECLO, RET, CONT, KLET, KIF } ;

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

struct exlet{
    enum Tagk t;
    kont * k;
    environ * env;
    Value var;
    Value body;
};

struct exif{
    enum Tagk t;
    kont * k;
    environ * env;
    Value cons;
    Value alt;
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
extern Value MakeLet(Value a, Value b, Value c);
extern Value MakeComp(Value a, Value b);
extern Value MakeIf(Value a, Value b, Value c);
extern Value MakeName(int f);

#endif

