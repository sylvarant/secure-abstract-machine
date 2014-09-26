/*
 * =====================================================================================
 *
 *       Filename:  PMA.h
 *
 *    Description:  PMA setup
 *
 *         Author:  tea
 *        Company:  Superstar Uni
 *
 * =====================================================================================
 */

#ifndef PMA_INCLUDED
#define PMA_INCLUDED

// The protocol
#include "attackerlang.h"


/*-----------------------------------------------------------------------------
 *  Preprocessor
 *-----------------------------------------------------------------------------*/

#ifdef DEBUG
    #include <stdio.h>
    # define DEBUG_PRINT(x) printf("DEBUG:: "); printf x ; printf("\n");fflush(stdout);
#else
    # define DEBUG_PRINT(x) do {} while (0);
#endif

// CPU target
#ifdef SPM

    #define __ANNOTATE(x) __attribute__((annotate(x)))

    #define SPM_FUNC(name)  __ANNOTATE("spm:" name)
    #define SPM_ENTRY(name) __ANNOTATE("spm_entry:" name) __attribute__((noinline, used))
    #define SPM_DATA(name)  SPM_FUNC(name)


    #define __MSP430_INTRINSICS_H_
    #include <msp430.h>
    #define FUNCTIONALITY SPM_FUNC("SPM1") static
    #define SECRET_DATA SPM_DATA("SPM1") static 
    #define ENTRYPOINT SPM_ENTRY("SPM1") extern
#else 
    #define FUNCTIONALITY static
    #define SECRET_DATA static
    #define ENTRYPOINT extern
#endif

#ifdef STATIC_MEM
typedef unsigned int mysize;
#else
#include <stdlib.h>
typedef size_t mysize; 
#endif 

// type define
typedef Value (*teval) (void*);
typedef void*  (*tmalloc) (mysize);

// entry points
ENTRYPOINT void * secure_eval(int seccode);
ENTRYPOINT void setup_secure(void* (*e)(void *),void* (*m)());
ENTRYPOINT void sload (void);

// aux
extern void die (const char * format, ...);

#endif
