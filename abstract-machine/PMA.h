/*
 * =====================================================================================
 *
 *       Filename:  PMA.h
 *
 *    Description:  PMA setup -> Sancus is a previously used pma mechanism
 *
 *         Author:  tea
 *        Company:  Superstar Uni
 *
 * =====================================================================================
 */

#ifndef PMA_INCLUDED
#define PMA_INCLUDED


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
#ifdef SANCUS_SPM

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
#ifdef FIDES_PMA

    #include <fides_libc/string.h>
    #define MALLOC malloc
    #define FREE(x) if((x) != NULL) free((x))

    #define LOCAL static
    #define SECRET_DATA static
    #define FUNCTIONALITY extern 
    #define ENTRYPOINT ENTRY_POINT

    #include <PCBAC/spm_annotations.h>

#else 
    #define FUNCTIONALITY static
    #define SECRET_DATA static
    #define ENTRYPOINT extern
#endif
#endif

#ifdef STATIC_MEM
typedef unsigned int mysize;
#else
#include <stdlib.h>
typedef size_t mysize; 
#endif 

// aux
extern void die (const char * format, ...);

#endif
