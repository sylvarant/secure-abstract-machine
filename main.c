/*
 * =====================================================================================
 *
 *       Filename:  main.c
 *
 *    Description:  startpoint
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala Uni
 *
 * =====================================================================================
 */

/*-----------------------------------------------------------------------------
 *  Preprocessor
 *-----------------------------------------------------------------------------*/
#include <string.h>
#include <stdarg.h>
#include "commonlang.h"
#include "attacker.h"
#include "FFI.h"

#ifdef JOB
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

static char * tostring(Value);
/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    generatestring
 *  Description:    do internal work
 * =====================================================================================
 */
static char * generatestring(char * start,int c,Value v1,Value v2){
    int sstart = strlen(start);
    char ** list = (char **) mymalloc(c*(sizeof(char *)));

    list[0] = tostring(v1);
    sstart   += strlen(list[0]);
    list[0] = tostring(v2);
    sstart   += strlen(list[1]);

    char * str = (char *) mymalloc(sizeof(char) * (sstart+(c-1)+2));
    str[0] ='\0';
    strcat(str,start);

    for(int i = 0; i <c ; ++i){
        strcat(str,list[i]);
        if(i == c-1) {}
        else{ strcat(str,",");}
    }

    strcat(str,")");

    #ifndef STATIC_MEM
    for(int i = 0; i < c ; i++){
        free(list[i]);
    }
    free(list);
    #endif
    return str;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    tostring
 *  Description:    conver Value into string, used to debug
 * =====================================================================================
 */
static char * tostring (Value par){

    if(par.b == NULL) {
        DEBUG_PRINT(("NULL VALUE !"))
        exit(1);    
    }
    
    if (par.b->t == BOOLEAN){
        char * str = (char *) mymalloc(6 * sizeof(char));
        sprintf(str,"%s",par.b->value ? "true" : "false");
        return str;
    }
    else if(par.l->t == LAM){
        return generatestring("lambda(",2,par.l->argument,par.l->body); 
    }
    else if(par.s->t == SYMBOL){
        char * str  =(char *) mymalloc(sizeof(char) * (strlen(par.s->name) + 1));
        str[0] ='\0';
        strcat(str,par.s->name);
        return str;
    }
    else if(par.a->t == APPLICATION){
       return generatestring("appl(",2,par.a->function,par.a->argument); 
    }
    else if(par.c->t == CLOSURE){
        return generatestring("clo(",2,par.c->x,par.c->body);
    }
    else if(par.i->t == NAME){
        char * str = (char *) mymalloc(5 * sizeof(char));
        str[0] = '\0';
        return str;
    }
    else{die("unkown result\n");}

    return NULL;
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

	// setup the insecure CEK
    startstate(); 

    // setup the secure 2/S Cesk -> results in the module code being located at position 1
    sload(); 

    // handshake between the insecure and the secure
	setup_secure(evaluate,mymalloc);

    DEBUG_PRINT(("Loading Module...")) 

    Value ret =
        MakeApplication(MakeLambda(MakeApplication(MakeApplication(MakeLambda(MakeSymbol( "x" ),MakeSymbol( "x" )),
        (MakeName(1))),MakeSymbol( "x" )),MakeSymbol( "x" )),MakeBoolean(1)); 

    DEBUG_PRINT(("Evaluating...")) 
    Value ans = run(ret);
    char * result = tostring(ans);
    printf("%s\n",result);

    DEBUG_PRINT(("Done...")) 

    return 0; 
}
