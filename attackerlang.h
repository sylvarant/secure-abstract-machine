/*
 * =====================================================================================
 *
 *       Filename:  attackerlang.h
 *
 *    Description:  Header file for the common language specification that defines
 *                  the PMA
 *
 *       Compiler:  gcc
 *         Author:  
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
struct If;
struct Let;
struct Comp;

enum Tag { BOOLEAN, CLOSURE, LAM, SYMBOL, APPLICATION, LET, COMP, IF, NAME};


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
    struct If * ii;
    struct Let * lt;
    struct Comp * co;
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
    struct environ_t  * env;
};

struct Let {
    enum Tag t;
    union Value var;
    union Value expr;
    union Value body;
};

struct Comp {
    enum Tag t;
    union Value left;
    union Value right;
};

struct If {
    enum Tag t;
    union Value cond;
    union Value cons;
    union Value alt;
};

struct Name{
    enum Tag t;
    int label;
};


// simplifications
typedef union Value Value ;

#endif

