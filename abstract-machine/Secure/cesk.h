/*
 * =====================================================================================
 *
 *       Filename:  seccesk.h
 *
 *    Description:  Internal header for the cesk implementation of MiniML
 *
 *         Author:  tea
 *        Company:  Superstar Uni
 *
 * =====================================================================================
 */

#ifndef SECCEK_INCLUDED
#define SECCEK_INCLUDED

#include "../PMA.h"

// naming convetion
#define T(NAME) T##NAME

/*-----------------------------------------------------------------------------
 * Environment + Storage + Map - We use the same data structure
 * to reduce to simplify the code 
 *-----------------------------------------------------------------------------*/
struct secenvnode {
    char *key;
    void * value;
    struct secenvnode *next;
};

typedef struct ENV_t {
    struct secenvnode *bucket;
    int (*cmp)(void *,void*);
    int size;
}ENV;


/*-----------------------------------------------------------------------------
 *  Type Expressions of MiniML
 *-----------------------------------------------------------------------------*/
typedef enum Tag_e{
    T(INT), T(BOOLEAN), T(ARROW),T(UNIT), T(REF)
} T(TAG);

struct T(Arrow){
    struct Type_u * left;
    struct Type_u * right;
};


struct T(Ref){
    struct Type_u * type;
};


typedef struct Type_u{
    T(TAG) t;
    union {
        struct T(Arrow) a;
        struct T(Ref) r;
    };
} TYPE;

// global constant types
extern const struct Type_u T(Int); 
extern const struct Type_u T(Unit);
extern const struct Type_u T(Boolean); 


/*-----------------------------------------------------------------------------
 *  Control Expressions of MiniML
 *-----------------------------------------------------------------------------*/
struct secBoolean ;
struct secClosure ;
struct secLambda;
union  TERM ;
struct secSymbol;
struct secApplication; 
struct secError;
struct secIf;
struct secLet;
struct FI;
struct secUnit;
struct secInt; 
struct secLetrec; 
struct secLocation;
struct secAlloc;
struct secHash;
struct secSequence; 
struct secSet; 
struct secOper; 
struct secFix; 
struct secDeref; 

enum secTag { BOOLEAN, UNIT, CLOSURE, LAM, SYMBOL, APPLICATION, LET, IF, INSEC,
  LETREC, INT, LOCATION, ALLOC, HASH, SEQUENCE, SET, OPER, FIX, DEREF};
enum opTag { PLUS, MIN, TIMES, EQUALS, LESS };

union TERM {
    struct secBoolean * b;
    struct secUnit * u;
    struct secLambda * l;
    struct secSymbol * s;
    struct secApplication * a;
    struct secClosure * c;
    struct secError * e;
    struct secIf * i;
    struct secLet * lt;
    struct FI * f;
    struct secInt * in;
    struct secLetrec * lr;
    struct secLocation * loc;
    struct secDeref * d;
    struct secAlloc * al;
    struct secHash * h;
    struct secSequence * sq;
    struct secSet * st;
    struct secOper * o;
    struct secFix * fix;
};

struct secUnit {
  enum secTag t ;
};


struct secBoolean {
    enum secTag t ;
    unsigned int value ;
};

struct secLambda {
    enum secTag t;
    union TERM body;
    union TERM argument; 
    TYPE ty;
};

struct secSymbol {
    enum secTag t;
    char * name;  
};

struct secApplication {
    enum secTag t;
    union TERM function;
    union TERM argument;
};

struct secClosure {
    enum secTag t;
    union TERM x;
    union TERM body; 
    ENV  * env;
};

struct secLet {
    enum secTag t;
    union TERM var;
    union TERM expr;
    union TERM body;
};

struct secIf {
    enum secTag t;
    union TERM cond;
    union TERM cons;
    union TERM alt;
};

struct FI{
    enum secTag t;
    TYPE ty;
    long (*foreignptr)(long);
};

struct secError{
    enum secTag t;
    char * message;
};

struct secInt{
    enum secTag t;
    int value;
};

struct secLetrec {
    enum secTag t;
    union TERM var;
    union TERM left;
    union TERM right;
    TYPE ty;
};

struct secLocation {
    enum secTag t;
    int count;
    union TERM * value;
};

struct secAlloc {
    enum secTag t;
    union TERM term;
};

struct secHash {
    enum secTag t;
    union TERM term;
};

struct secSequence {
    enum secTag t;
    union TERM left;
    union TERM right;
};

struct secSet {
    enum secTag t;
    union TERM left;
    union TERM right;
};

struct secOper {
    enum secTag t;
    enum opTag op;
    union TERM left;
    union TERM right;
};

struct secFix {
    enum secTag t;
    union TERM term;
};

struct secDeref {
    enum secTag t;
    union TERM term;
};

// simplifications
typedef union TERM TERM;


/*-----------------------------------------------------------------------------
 *  Machine Parts 
 *-----------------------------------------------------------------------------*/

// All the continuations

typedef union kont_t{
    struct done * d;
    struct appkont * a;
    struct appkont2 * a2;
    struct letkont * lt;
    struct ifkont * i;
    struct allockont * al;
    struct hashkont * h;
    struct operkont * o;
    struct derefkont * dr;
    struct setkont * s;
    struct sequencekont * sq;
    struct fixkont * f;
} kont;

typedef union ffikont_t{
  struct empty * e;
  struct waiting * w;
  struct executing * x;
  struct marshall * m; // only distinguished by tag
} ffikont;

enum Tagffi { EMPTY, WAITING, EXECUTING, MARSHALLIN, MARSHALLOUT };
enum Tagk { APPKONT, APPKONT2, DONE, LETKONT, IFKONT, ALLOCKONT,  
  HASHKONT, OPERKONT,OPERKONT2, DEREFKONT, SETKONT, SETKONT2, 
  SEQUENCEKONT, FIXKONT };

struct empty {
  enum Tagffi t;
};

struct waiting {
  enum Tagffi t;
  kont k;
  ENV * e;
  ffikont outerk;
  TYPE ty;
};

struct executing {
  enum Tagffi t;
  ffikont outerk; 
  kont k;
  TYPE ty;
};

struct marshall {
  enum Tagffi t;
  TYPE ty;
  ffikont outerk; 
};

struct done {
    enum Tagk t;
};


struct appkont {
    enum Tagk t; 
    TERM expr;
    ENV * env;
    kont k;
};

struct appkont2 {
    enum Tagk t; 
    TERM expr;
    kont k;
};

struct letkont{
    enum Tagk t;
    kont k;
    ENV * env;
    TERM var;
    TERM body;
};

struct ifkont{
    enum Tagk t;
    kont k;
    ENV * env;
    TERM cons;
    TERM alt;
};

struct allockont {
    enum Tagk t;
    kont k;
    ENV * env;
};

struct hashkont {
    enum Tagk t;
    kont k;
    ENV * env;
};

struct operkont {
    enum Tagk t; // distinguish by tags
    enum opTag op;
    kont k;
    TERM other;
    ENV * env;
};

struct derefkont {
    enum Tagk t;
    kont k;
    ENV * env;
};

struct setkont {
    enum Tagk t; // distinguish by tags
    kont k;
    TERM other; 
    ENV * env;
};

struct sequencekont { 
    enum Tagk t; // distinguish by tags
    kont k;
    TERM other; 
    ENV * env;
};

struct fixkont {
    enum Tagk t; // distinguish by tags
    kont k;
    ENV * env;
};

// cesk State
typedef struct state_t{
//    TERM * control;
    ENV * environment; 
    ENV * namemap;
    ffikont continuation;
}state;


/*-----------------------------------------------------------------------------
 *  Entry points
 *  - returnbacks are handled by the fides/sance compiler
 *  - return pointers are handled by the fides/sancus compiler as well
 *-----------------------------------------------------------------------------*/

ENTRYPOINT long start (void);
ENTRYPOINT long application(long name,long argument);
ENTRYPOINT long allocation(long argument,long type);
ENTRYPOINT long set(long name,long argument);
ENTRYPOINT long dereference(long name);

#endif
