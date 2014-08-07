/*
 * =====================================================================================
 *
 *       Filename:  seccesk.c
 *
 *    Description:  secure 2/S cesk implementation for the lambda calculus
 *    ->  When running on the Sancus Processor we used to use a static memory pool
 *        as the debug builds did not support malloc and free, now that they do the
 *        static memory issues should probably be removed 
 *        and free's should be added
 *
 *          Author:  Adriaan Larmuseau, ajhl
 *        Company:  Uppsala Uni
 *
 * =====================================================================================
 */

#include "FFI.h"
#include "securecek.h"

// Memory management
#ifdef STATIC_MEM
    #define NULL (void*)0
    FUNCTIONALITY void * _mymalloc(mysize);
    #define mymalloc _mymalloc
#else
    #define mymalloc malloc
#endif 


/*-----------------------------------------------------------------------------
 *  Internal Function Declarations of the SPM
 *  Everything used by the secure cesk must be inside the SPM to protect
 *  the secure data
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY int secinsert(secenviron *table,void *key,void * value);
FUNCTIONALITY void * secget(secenviron *table,void *key);
FUNCTIONALITY secenviron * seccopyenv(secenviron * table);
FUNCTIONALITY int getname(void);
FUNCTIONALITY secValue run(secValue,state *);
FUNCTIONALITY state * startstate();
FUNCTIONALITY secValue MakesecBoolean(unsigned int);
FUNCTIONALITY secValue MakesecLambda(secValue,secValue);
FUNCTIONALITY secValue MakesecApplication(secValue ,secValue );
FUNCTIONALITY secValue MakesecSymbol(char * );
FUNCTIONALITY secValue MakesecClosure(secValue ,secValue , secenviron * );
FUNCTIONALITY secValue MakeSI(Value v);
FUNCTIONALITY Value MakeBoolean(unsigned int b);
FUNCTIONALITY Value MakeSymbol(char * name);
FUNCTIONALITY Value MakeIS(int dptr);
FUNCTIONALITY Value MakeLambda(Value body,Value arg);
FUNCTIONALITY Value MakeApplication(Value a, Value b);
FUNCTIONALITY secValue applycont(secValue v,state * st);
FUNCTIONALITY mysize mystrlen(const char *str);
FUNCTIONALITY int mycmp(void *s1, void *s2);
FUNCTIONALITY int mycmpint(void *s1,void *s2);
FUNCTIONALITY char * mycat(char *dest, const char *src);


/*-----------------------------------------------------------------------------
 *  Shared Global Variables
 *-----------------------------------------------------------------------------*/
SECRET_DATA void * (*inseceval)(void *);
SECRET_DATA void * (*insecmalloc)(mysize);
SECRET_DATA state * mystate;

/*-----------------------------------------------------------------------------
 *  Custom memory scheme
 *-----------------------------------------------------------------------------*/
#ifdef STATIC_MEM

SECRET_DATA char memory[2048];
SECRET_DATA char * malloc_ptr;

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    mymalloc
 *  Description:    testing
 * =====================================================================================
 */
 static void * _mymalloc(mysize size)
{
  void *ret;
  ret = (void*)malloc_ptr;
  malloc_ptr += size;
  return ret;
}

#endif

/*-----------------------------------------------------------------------------
 *  Internal functionality
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    mycmp
 *  Description:    taken from apple
 * =====================================================================================
 */
static int mycmp(void *vs1, void *vs2)
{
    char * s1 = (char *) vs1;
    char * s2 = (char *) vs2;

    for ( ; *s1 == *s2; s1++, s2++)
	if (*s1 == '\0')
	    return 0;
    return ((*(unsigned char *)s1 < *(unsigned char *)s2) ? -1 : +1);
}

static int mycmpint(void *vs1, void *vs2)
{
    int s1 = (int) vs1;
    int s2 = (int) vs2;

    if(s1==s2) return 0;
    else return 1;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    mycat
 *  Description:    taken from stack overflow
 * =====================================================================================
 */
  static char * mycat(char *dest, const char *src)
{
    mysize i,j;
    for (i = 0; dest[i] != '\0'; i++)
        ;
    for (j = 0; src[j] != '\0'; j++)
        dest[i+j] = src[j];
    dest[i+j] = '\0';
    return dest;
} 

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    strlen
 *  Description:    taken from bsd
 * =====================================================================================
 */
  static mysize mystrlen(const char *str)
{
	const char *s;
	for (s = str; *s; ++s)
		;
	return (s - str);
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecBoolean
 *  Description:    create a secValue BOOLEAN
 * =====================================================================================
 */
 static secValue MakesecBoolean(unsigned int b) {
    secValue v ;
    struct secBoolean * data = (struct secBoolean*) mymalloc(sizeof(struct secBoolean));
    v.b = data;
    v.b->t = SECBOOLEAN;
    v.b->value = b;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecLambda
 *  Description:    create a secValue LAM
 * =====================================================================================
 */
  static secValue MakesecLambda(secValue body,secValue arg){

    secValue v;

    struct secLambda * data = (struct secLambda*) mymalloc(sizeof(struct secLambda));
    v.l = data;
    v.l->t = SECLAM;
    v.l->argument = arg;
    v.l->body  = body;

    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecApplication
 *  Description:    create a secValue APPLICATION
 * =====================================================================================
 */
  static secValue MakesecApplication(secValue a,secValue b){
    
    secValue v;
    struct secApplication * data = (struct secApplication *) mymalloc(sizeof(struct secApplication));
    v.a = data;
    v.a->t         = SECAPPLICATION;
    v.a->argument  = b;
    v.a->function  = a;
    
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecSymbol
 *  Description:    create a secValue SYMBOL
 * =====================================================================================
 */
  static secValue MakesecSymbol(char * name){
    secValue v;
    struct secSymbol * data = (struct secSymbol*) mymalloc(sizeof(struct secSymbol));
    v.s = data;
    v.s->t    = SECSYMBOL;
    v.s->name = name;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecClosure
 *  Description:    create a secValue Closure
 * =====================================================================================
 */
 static secValue MakesecClosure(secValue x,secValue body, secenviron * htbl){
        secValue clos;
          
        struct secClosure * data = (struct secClosure*) mymalloc(sizeof(struct secClosure));
        clos.c = data;
        clos.c->t = SECCLOSURE;
        clos.c->x = x;
        clos.c->body = body;
        clos.c->env = seccopyenv(htbl);

		return clos;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSI  
 *  Description:    create a secValue IS component
 * =====================================================================================
 */
  static secValue MakeSI(Value v){   
    
    secValue i ;
    struct SI * inter = mymalloc(sizeof(struct SI)); 
    i.i = inter;
    i.i->term = v;
    i.i->t = INSEC;

    return i;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeIS  
 *  Description:    create a Value IS component
 * =====================================================================================
 */
  static Value MakeName(int f){   

    Value i ;
    struct IS * inter = insecmalloc(sizeof(struct Name)); 
    i.i = inter;
    i.i->label = f;
    i.i->t = NAME;

    return i;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeApplication 
 *  Description:    Create a Value Application
 * =====================================================================================
 */
 static Value MakeApplication(Value a, Value b){
    Value v;
    struct Application * data = (struct Application*) insecmalloc(sizeof(struct Application));
    v.a = data;
    v.a->t         = APPLICATION;
    v.a->argument  = b;
    v.a->function  = a;
    
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeBoolean
 *  Description:    create a Value BOOLEAN
 * =====================================================================================
 */
  static Value MakeBoolean(unsigned int b) {
    Value v ;
    struct Boolean * data = (struct Boolean*) insecmalloc(sizeof(struct Boolean));
    v.b = data;
    v.b->t = BOOLEAN;
    v.b->value = b;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSymbol
 *  Description:    create a Value SYMBOL
 * =====================================================================================
 */
  static Value MakeSymbol(char * name){

    Value v;
    struct Symbol * data = (struct Symbol*) insecmalloc(sizeof(struct Symbol));
    v.s = data;
    v.s->t    = SYMBOL;

    char * str = (char *) insecmalloc(sizeof(char) * (3)); 
    str[0] ='\0';
    mycat(str,name);
   
    v.s->name = str;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeLambda
 *  Description:    create a Value LAM
 * =====================================================================================
 */
  static Value MakeLambda(Value body,Value arg){

    Value v;
    struct Lambda * data = (struct Lambda*) insecmalloc(sizeof(struct Lambda));

    v.l = data;
    v.l->t = LAM;
    v.l->argument = arg;
    v.l->body  = body;

    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    secget
 *  Description:    return an element from an environment
 * =====================================================================================
 */
  static void * secget(secenviron *table,void *key)
{
    struct secenvnode *node;
    node = table->bucket;
    while(node) {
        if(table->cmp(key,node->key) == 0)
            return node->value;
        node = node->next;
    }
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    secinsert
 *  Description:    add something to an environment
 * =====================================================================================
 */
static int secinsert(secenviron *table,void *key,void * value)
{
    struct secenvnode **tmp;
    struct secenvnode *node ;

    tmp = &table->bucket;
    while(*tmp) {
        if(table->cmp(key,(*tmp)->key) == 0)
            break;
        tmp = &(*tmp)->next;
    }
    if(*tmp) { 
        node = *tmp;
    } else {
        node = mymalloc(sizeof *node);
        if(node == NULL)
            return -1;
        node->next = NULL;
        *tmp = node;

        table->size++;

    }
    node->key = key;
    node->value = value;

    return 0;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    seccopyenv
 *  Description:    copy an env
 * =====================================================================================
 */
static secenviron * seccopyenv(secenviron * table){
    secenviron * new = (secenviron *) mymalloc(sizeof(secenviron));
    new->size = table->size;
    new->cmp = table->cmp;

    struct secenvnode *node;
    struct secenvnode *nnode;
    node = table->bucket;
    new->bucket = (struct secenvnode *) mymalloc(sizeof(struct secenvnode)); 

    nnode =  new->bucket;
	struct secenvnode * dumb = nnode;
    while(node != NULL) {
        nnode->key             = node->key;
        nnode->value           = node->value;
        nnode->next            = (struct secenvnode *) mymalloc(sizeof(struct secenvnode)); 
        node = node->next;
		dumb = nnode;
        nnode = nnode->next;
    }

	if(dumb != nnode){
		dumb->next = NULL;
	}else{
		new->bucket = NULL;
	}

    return new;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    applycont
 *  Description:    apply continuation
 * =====================================================================================
 */
static secValue applycont(secValue v,state * st){

    if(st->continuation == NULL){
        return v;
    }
    else if(st->continuation->e->t == SEXEC1){
        secValue temp               = st->continuation->e->expr;
        kont * kk                = st->continuation->e->k; 
        st->environment            = st->continuation->e->env;
        st->continuation->c       = (struct execlo *) mymalloc(sizeof(struct execlo));
        st->continuation->c->t    = SEXECLO;
        st->continuation->c->expr = v;
        st->continuation->c->k    = kk;
        return run(temp,st);
    }
    else if(st->continuation->c->t == SEXECLO){
        struct secClosure * cl =  st->continuation->c->expr.c;
        if(cl->t == SECCLOSURE){
            secinsert(cl->env,cl->x.s->name,v.b);
            //secinsert(st->storage,c,v.b);
            st->environment  = cl->env;
            st->continuation = st->continuation->c->k;
            return run(cl->body,st);
        }
        else{
            DEBUG_PRINT(("Expected to Apply a Closure"))
            exit(1);
        }
    }
    else if(st->continuation->cc->t == SECONT){
       st->environment  = st->continuation->cc->env;
       st->continuation = st->continuation->cc->k;
       return run(v,st);
    }
    else if(st->continuation->r->t == SERET){
        st->continuation = st->continuation->r->k;
       return v;     
    }
    else{
        DEBUG_PRINT(("Secure :: Unkown Closure"))
        exit(1);
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    run
 *  Description:    run the cesk
 * =====================================================================================
 */
static secValue run(secValue expr, state * st){

    if(expr.b->t == SECBOOLEAN || expr.c->t == SECCLOSURE){
        return applycont(expr,st); 
    }
    else if (expr.s->t == SECSYMBOL){
        secValue val;
        val.b = secget(st->environment,(void *) expr.s->name); 
        //val.b = secget(st->storage,(void *) c);
        if(val.b == NULL){
            DEBUG_PRINT(("Secure :: Variable Not Found"))
            exit(1);
        }
        return applycont(val,st);
    }
    else if (expr.l->t == SECLAM){
       return applycont(MakesecClosure(expr.l->argument,expr.l->body,seccopyenv(st->environment)),st);  
    }
    else if(expr.a->t == SECAPPLICATION){

        kont * prev = NULL;
        if(st->continuation != NULL){prev = st->continuation;}

        st->continuation = mymalloc(sizeof(kont));
        st->continuation->e = (struct exec1 *) mymalloc(sizeof(struct exec1)); 
        st->continuation->e->t = SEXEC1;
        st->continuation->e->expr = expr.a->argument;
        st->continuation->e->env  = seccopyenv(st->environment);
        st->continuation->e->k    = prev;
        return run(expr.a->function,st);
    }
    else if(expr.i->t == INSEC){

        DEBUG_PRINT(("Calling the Insecure"))
        
        // make Continue continuation
        kont * prev = NULL;
        if(st->continuation != NULL){prev = st->continuation;}

        st->continuation = mymalloc(sizeof(kont)); 
        st->continuation->cc    = (struct excont *) mymalloc(sizeof(struct excont));
        st->continuation->cc->t = SECONT;
        st->continuation->cc->k = prev;
        st->continuation->cc->env = seccopyenv(st->environment);

        void * dle = inseceval((expr.i->term).b); 
        Value ptr = {.b = dle};
        
        if(ptr.b->t == BOOLEAN){
            return run(MakesecBoolean(ptr.b->value),st);       
        }
        else if(ptr.c->t == CLOSURE){
            int name = getname();
            //secinsert(mystate->storage,(void *)c,(void *) (MakesecSymbol("y")).b);
            secinsert(mystate->functions,(void *)name, (void *) (MakesecSymbol("y")).b );
            return run(MakesecLambda(MakeSI(MakeApplication(ptr,MakeName(name))),MakesecSymbol("y")),st); 
        }
        return run(MakeSI(ptr),st);
    }
    else{
        DEBUG_PRINT(("Unknown State"))
        exit(1);
	}
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    getname
 *  Description:    Get a fresh name n 
 * =====================================================================================
 */
static int getname(void){
    static char x = 0;
    return ++x;
}


/*-----------------------------------------------------------------------------
 *  functions for the left machine
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    secure_eval
 *  Description:    run the cesk
 * =====================================================================================
 */
ENTRYPOINT void * secure_eval(int label){
    
    secValue sec;
    Value inp;

    sec.b =  secget(mystate->functions,label); 

    if(sec.b == NULL) return -1;

    DEBUG_PRINT(("RUNNING IN SEC"))

    // make Return continuation
    kont * prev = NULL;
    if(mystate->continuation != NULL){prev = mystate->continuation;}

    mystate->continuation = mymalloc(sizeof(kont)); 
    mystate->continuation->r    = (struct exret *) mymalloc(sizeof(struct exret));
    mystate->continuation->r->t = SERET;
    mystate->continuation->r->k = prev;

    // compute
    secValue ret = run(sec,mystate);

    if(ret.b->t == SECBOOLEAN){
       return (MakeBoolean(ret.b->value)).b; 
    }
    else if (ret.c->t == SECCLOSURE) {
        DEBUG_PRINT(("DONE"))
        int name = getname();
        //secinsert(mystate->storage,(void *)c,);
        secinsert(mystate->functions,(void *) name, (void *) (MakesecApplication(ret,MakeSI(MakeSymbol("z")))).b  );
        return (MakeLambda(MakeName(name),MakeSymbol("z"))).b; 
    } 

    DEBUG_PRINT(("Invalid Return Value"))
    exit(1);
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    setup the secure machine   
 *  Description:    acquire necessary functions
 * =====================================================================================
 */
ENTRYPOINT void setup_secure(void* (*e)(void *),void* (*m)()){
    #ifdef STATIC_MEM
        malloc_ptr = memory; 
    #endif
    inseceval = e;
    insecmalloc = m;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    load 
 *  Description:    upload program to storage
 * =====================================================================================
 */
ENTRYPOINT void sload (void){

    // simplification
    secValue mm = MakesecLambda(MakesecSymbol( "x" ),MakesecSymbol( "x" )) ;

    mystate = mymalloc(sizeof(state));

    static secenviron tbl = {NULL,mycmp,0};
    secenviron * envtable = (secenviron *) mymalloc(sizeof(secenviron));  
    *envtable = tbl;
    mystate->environment = envtable;

    secenviron * funtbl = (secenviron *) mymalloc(sizeof(secenviron));  
    *funtbl = tbl;
    funtbl->cmp = mycmpint;
    mystate->functions = funtbl;
    secinsert(mystate->functions,getname(),mm.b);
    
    mystate->continuation = NULL;
}


