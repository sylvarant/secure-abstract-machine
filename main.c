/*
 * =====================================================================================
 *
 *       Filename:  main.c
 *
 *    Description:  startpoint
 *
 *         Author:  tea
 *        Company:  Superstar Uni
 *
 * =====================================================================================
 */

/*-----------------------------------------------------------------------------
 *  Preprocessor
 *-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdarg.h>
#include "PMA.h"

#ifndef FIDES_PMA
#include "Secure/cesk.h"
#endif

#ifdef SANCUS_SPM
    #define __MSP430_INTRINSICS_H_
    #include <msp430.h>
    #define MAIN_TYPE int __attribute__((section(".init9"), aligned(2)))
#else
    #define MAIN_TYPE int
#endif


/*-----------------------------------------------------------------------------
 *  Custom memory scheme
 *-----------------------------------------------------------------------------*/
#ifdef STATIC_MEM

static char memory[2048];
static char * malloc_ptr;

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    mymalloc
 *  Description:    terrible malloc - to debug on the Sancus Processor
 *                  it's the reason we don't have that many free's
 * =====================================================================================
 */
extern void * staticmalloc(mysize size)
{
  void *ret;
  ret = (void*)malloc_ptr;
  malloc_ptr += size;
  return ret;
}

#endif

/*-----------------------------------------------------------------------------
 *  Custom IO
 *-----------------------------------------------------------------------------*/
 #ifdef JOB
int putchar(int c)
{
    P1OUT  = c;
    P1OUT |= 0x80;
    return c;
}
 #endif


/*-----------------------------------------------------------------------------
 *  Auxiliary shared functionality
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    die
 *  Description:    in blitz.h
 * =====================================================================================
 */
extern void die (const char * format, ...)
{
    va_list vargs;
    va_start (vargs, format);
    vfprintf (stderr, format, vargs);
    fprintf (stderr, ".\n");
    exit (EXIT_FAILURE);
}


/*-----------------------------------------------------------------------------
 *  Internal functionality
 *-----------------------------------------------------------------------------*/

void debug_name(long name){
  DEBUG_PRINT(("Received name %lu from CESK\n",name))
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    main
 *  Description:    the startpoint
 * =====================================================================================
 */
MAIN_TYPE main(int argc, char * argv[]){

    DEBUG_PRINT(("Setting up...")) 

    #ifdef STATIC_MEM
        malloc_ptr = memory;
    #endif

    // Start the secure cesk machine
    DEBUG_PRINT(("Starting CESK...")) 
    long name = start();
    debug_name(name);

    DEBUG_PRINT(("Done...")) 

    return 0; 
}
