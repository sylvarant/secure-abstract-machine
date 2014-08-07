/*
 * =====================================================================================
 *
 *       Filename:  commonlang.h
 *
 *    Description:  Header file for the common language specification that defines
 *                  the FFI
 *
 *       Compiler:  gcc
 *         Author:  Adriaan Larmuseau
 *
 * =====================================================================================
 */

#ifndef COMMON_INCLUDED
#define COMMON_INCLUDED

// who is who
struct Boolean ;
struct Closure ;
struct Lambda;
union Value ;
struct Symbol;
struct Application; 
struct Name;
struct environ;
struct Error;

enum Tag { BOOLEAN, CLOSURE, LAM, SYMBOL, APPLICATION, NAME} ;


/*-----------------------------------------------------------------------------
 *  Structure Definitions
 *-----------------------------------------------------------------------------*/

union Value {
    struct Boolean * b;
    struct Lambda * l;
    struct Symbol * s;
    struct Application * a;
    struct Closure * c;
    struct Error * e;
    struct Name * i;
};

struct Boolean {
    enum Tag t ;
    unsigned int value ;
};

struct Lambda {
    enum Tag t;
    union Value body;
    union Value argument; 
};

struct Symbol {
    enum Tag t;
    char * name;  
};

struct Application {
    enum Tag t;
    union Value function;
    union Value argument;
};

struct Closure {
    enum Tag t;
    union Value x;
    union Value body; 
    struct environ  * env;
};

struct Name{
    enum Tag t;
    int label;
};


// simplifications
typedef union Value Value ;


/*-----------------------------------------------------------------------------
 *  Functions
 *-----------------------------------------------------------------------------*/

#endif

