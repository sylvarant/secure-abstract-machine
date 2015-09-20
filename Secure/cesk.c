/*
 * =====================================================================================
 *
 *       Filename:  seccek.c
 *
 *    Description:  secure cesk implementation for MiniML
 *    ->  When running on the Sancus Processor we used to use a static memory pool
 *        as the debug builds did not support malloc and free, now that they do the
 *        static memory issues should probably be removed 
 *        and free's should be added
 *
 *          Author:  tea
 *        Company:  Superstar Uni
 *
 * =====================================================================================
 */

#include "cesk.h"

// Memory management
#ifdef STATIC_MEM
    #define NULL (void*)0
    FUNCTIONALITY void * _mymalloc(mysize);
    #define mymalloc _mymalloc
#else
    #define mymalloc malloc
#endif 


/*-----------------------------------------------------------------------------
 * C-Linking dumbness
 *-----------------------------------------------------------------------------*/

const struct Type_u T(Int) = {.t = T(INT), .a = 0};
const struct Type_u T(Unit) = {.t = T(UNIT), .a = 0};
const struct Type_u T(Boolean) = {.t = T(BOOLEAN), .a = 0};

struct secUnit su = { .t = UNIT};
struct secBoolean str = { .t = BOOLEAN, .value = 1};
struct secBoolean sfa = { .t = BOOLEAN, .value = 0};
const TERM Unit = {.u = &su };
const TERM True = {.b = &str};
const TERM False = {.b = &sfa};

struct done dd = {.t = DONE};
const kont Done = { .d = &dd };
struct empty ee = {.t = EMPTY };
const ffikont Empty = {.e = &ee };

/*-----------------------------------------------------------------------------
 *  Internal Function Declarations of the protected module
 *  Everything used by the secure cesk must be inside the SPM to protect
 *  the secure data
 *-----------------------------------------------------------------------------*/
FUNCTIONALITY int secinsert(ENV *table,void *key,void * value);
FUNCTIONALITY void * secget(ENV *table,void *key);
FUNCTIONALITY ENV * seccopyenv(ENV * table);
FUNCTIONALITY int getname(void);
FUNCTIONALITY TERM run(TERM,state *);
FUNCTIONALITY state * startstate();
FUNCTIONALITY TERM MakesecBoolean(unsigned int);
FUNCTIONALITY TERM MakesecLambda(TERM,TYPE,TERM);
FUNCTIONALITY TERM MakesecApplication(TERM,TERM);
FUNCTIONALITY TERM MakesecSymbol(char * );
FUNCTIONALITY TERM MakesecClosure(TERM, TERM, ENV *);
FUNCTIONALITY TERM MakesecLet(TERM,TERM,TERM);
FUNCTIONALITY TERM MakesecIf(TERM,TERM,TERM);
FUNCTIONALITY TERM MakesecLetrec(TERM,TYPE,TERM,TERM);
FUNCTIONALITY TERM MakesecInt(int);
FUNCTIONALITY TERM MakesecLocation(TERM);
FUNCTIONALITY TERM MakesecAlloc(TERM);
FUNCTIONALITY TERM MakesecHash(TERM);
FUNCTIONALITY TERM MakesecSequence(TERM,TERM);
FUNCTIONALITY TERM MakesecSet(TERM,TERM);
FUNCTIONALITY TERM MakesecOper(enum opTag,TERM,TERM);
FUNCTIONALITY TERM MakesecFix(TERM);
FUNCTIONALITY TERM MakeFI(long ptr);
FUNCTIONALITY TERM applycont(TERM v,state * st);
FUNCTIONALITY mysize mystrlen(const char *str);
FUNCTIONALITY int mycmp(void *s1, void *s2);
FUNCTIONALITY int mycmpint(void *s1,void *s2);
FUNCTIONALITY char * mycat(char *dest, const char *src);


/*-----------------------------------------------------------------------------
 *  control structure
 *-----------------------------------------------------------------------------*/
typedef union control_t {
  TERM t;
  long l;
} CONTROL;

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
FUNCTIONALITY void * _mymalloc(mysize size)
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
FUNCTIONALITY int mycmp(void *vs1, void *vs2)
{
  char * s1 = (char *) vs1;
  char * s2 = (char *) vs2;

  for ( ; *s1 == *s2; s1++, s2++)
	  if (*s1 == '\0')
	    return 0;
  return ((*(unsigned char *)s1 < *(unsigned char *)s2) ? -1 : +1);
}

FUNCTIONALITY int mycmpint(void *vs1, void *vs2)
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
FUNCTIONALITY char * mycat(char *dest, const char *src)
{
  mysize i,j;
  for (i = 0; dest[i] != '\0'; i++);
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
FUNCTIONALITY mysize mystrlen(const char *str)
{
	const char *s;
	for (s = str; *s; ++s);
	return (s - str);
}

/*-----------------------------------------------------------------------------
 *  Create types and terms
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeTRef
 *  Description:  create a reference type
 * =====================================================================================
 */
FUNCTIONALITY TYPE MakeTRef(TYPE type)
{
    TYPE t;
    t.t = T(REF);
    t.r.type  = mymalloc(sizeof(TYPE));
    *(t.r.type) = type;
    return t;
}

/* 
 * ===  FUNCTION ======================================================================
 *         Name:  makeTArrow
 *  Description:  create an Arrow type - mallocs !
 * =====================================================================================
 */
FUNCTIONALITY TYPE MakeTArrow(TYPE left, TYPE right)
{
    TYPE t;
    t.t = T(ARROW);
    t.a.left = mymalloc(sizeof(TYPE));
    t.a.right = mymalloc(sizeof(TYPE));
    *(t.a.left) = left; 
    *(t.a.right) = right; 
    return t;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecBoolean
 *  Description:    create a TERM BOOLEAN
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecBoolean(unsigned int b) 
{
  if(b) return True;
  else return False;
}
 

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecLambda
 *  Description:    create a TERM LAM
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecLambda(TERM body,TYPE ty,TERM arg)
{
  TERM v;
  struct secLambda * data = (struct secLambda*) mymalloc(sizeof(struct secLambda));
  v.l = data;
  v.l->t = LAM;
  v.l->argument = arg;
  v.l->body  = body;
  v.l->ty = ty;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecApplication
 *  Description:    create a TERM APPLICATION
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecApplication(TERM a,TERM b)
{    
  TERM v;
  struct secApplication * data = (struct secApplication *) mymalloc(sizeof(struct secApplication));
  v.a = data;
  v.a->t         = APPLICATION;
  v.a->argument  = b;
  v.a->function  = a;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecSymbol
 *  Description:    create a TERM SYMBOL
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecSymbol(char * name){
  TERM v;
  struct secSymbol * data = (struct secSymbol*) mymalloc(sizeof(struct secSymbol));
  v.s = data;
  v.s->t    = SYMBOL;
  v.s->name = name;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecClosure
 *  Description:    create a TERM Closure
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecClosure(TERM x,TERM body, ENV * htbl){
  TERM clos;
  struct secClosure * data = (struct secClosure*) mymalloc(sizeof(struct secClosure));
  clos.c = data;
  clos.c->t = CLOSURE;
  clos.c->x = x;
  clos.c->body = body;
  clos.c->env = seccopyenv(htbl);
	return clos;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecIf
 *  Description:    create a TERM IF
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecIf(TERM a, TERM b, TERM c)
{
  TERM v;
  struct secIf * data = (struct secIf*) mymalloc(sizeof(struct secIf));
  v.i  = data;
  v.i->t = IF;
  v.i->cond = a;
  v.i->cons = b;
  v.i->alt = c;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecLet
 *  Description:    create a TERM Let
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecLet(TERM a, TERM b, TERM c)
{
  TERM v;
  struct secLet * data = (struct secLet*) mymalloc(sizeof(struct secLet));
  v.lt  = data;
  v.lt->t = LET;
  v.lt->var = a;
  v.lt->expr = b;
  v.lt->body = c;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecLetrec
 *  Description:    create a TERM Letrec
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecLetrec(TERM var, TYPE ty, TERM a, TERM b)
{
  TERM v;
  struct secLetrec * data = (struct secLetrec*) mymalloc(sizeof(struct secLetrec));
  v.lr = data;
  v.lr->t = LETREC;
  v.lr->var = var;
  v.lr->ty = ty;
  v.lr->left = a;
  v.lr->right = b;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecInt
 *  Description:    create a TERM Int
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecInt(int value)
{
  TERM v;
  struct secInt * data = (struct secInt*) mymalloc(sizeof(struct secInt));
  v.in = data;
  v.in->t = INT;
  v.in->value = value;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecLocation
 *  Description:    create a TERM Location
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecLocation(TERM value)
{
  static int count = 0;
  TERM v; 
  struct secLocation * data = (struct secLocation*) mymalloc(sizeof(struct secLocation));
  v.loc = data;
  v.loc->t = LOCATION;
  v.loc->value = mymalloc(sizeof(TERM));
  *(v.loc->value) = value;
  v.loc->count = count++;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecAlloc
 *  Description:    create a TERM Alloc / ref
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecAlloc(TERM value)
{
  TERM v; 
  struct secAlloc * data = (struct secAlloc*) mymalloc(sizeof(struct secAlloc));
  v.al = data;
  v.al->t = ALLOC;
  v.al->term = value;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecHash
 *  Description:    create a TERM hash
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecHash(TERM value)
{
  TERM v;
  struct secHash * data = (struct secHash*) mymalloc(sizeof(struct secHash));
  v.h = data;
  v.h->t = HASH;
  v.h->term = value;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecSequence
 *  Description:    create a TERM sequence
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecSequence(TERM left, TERM right)
{
  TERM v;
  struct secSequence * data = (struct secSequence*) mymalloc(sizeof(struct secSequence));
  v.sq = data;
  v.sq->t = SEQUENCE;
  v.sq->left = left;
  v.sq->right = right;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecSet
 *  Description:    create a TERM sequence
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecSet(TERM left, TERM right)
{
  TERM v;
  struct secSet * data = (struct secSet*) mymalloc(sizeof(struct secSet));
  v.st = data;  
  v.st->t = SET;
  v.st->left = left;
  v.st->right = right;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecOper
 *  Description:    create a TERM operators
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecOper(enum opTag op, TERM left, TERM right)
{
  TERM v;
  struct secOper * data = (struct secOper*) mymalloc(sizeof(struct secOper));
  v.o = data;
  v.o->t = OPER;
  v.o->op = op;
  v.o->left = left;
  v.o->right = right;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakesecFix
 *  Description:    create a TERM fix
 * =====================================================================================
 */
FUNCTIONALITY TERM MakesecFix(TERM term)
{
  TERM v;
  struct secFix * data = (struct secFix*) mymalloc(sizeof(struct secFix));
  v.fix = data; 
  v.fix->t = FIX;
  v.fix->term = term;
  return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSFI 
 *  Description:    create a TERM FI component
 * =====================================================================================
 */
FUNCTIONALITY TERM MakeFI(long arg)
{   
  TERM i ;
  struct FI * inter = mymalloc(sizeof(struct FI)); 
  i.f = inter;
  i.f->foreingptr = (void *) arg;
  i.f->t = INSEC;
  return i;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    secget
 *  Description:    return an element from an environment
 * =====================================================================================
 */
FUNCTIONALITY void * secget(ENV *table,void *key)
{
  struct secenvnode *node;
  node = table->bucket;
  while(node) {
    if(table->cmp(key,node->key) == 0)
        return node->value;
    node = node->next;
  }
  return NULL;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    secinsert
 *  Description:    add something to an environment
 * =====================================================================================
 */
FUNCTIONALITY int secinsert(ENV *table,void *key,void * value)
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
FUNCTIONALITY ENV * seccopyenv(ENV * table)
{
  ENV * new = (ENV *) mymalloc(sizeof(ENV));
  new->size = table->size;
  new->cmp = table->cmp;

  struct secenvnode *node;
  struct secenvnode *nnode;
  node = table->bucket;
  new->bucket = (struct secenvnode *) mymalloc(sizeof(struct secenvnode)); 

  nnode = new->bucket;
	struct secenvnode * dumb = nnode;
  while(node != NULL) {
    nnode->key = node->key;
    nnode->value = node->value;
    nnode->next = (struct secenvnode *) mymalloc(sizeof(struct secenvnode)); 
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

FUNCTIONALITY long plug_outerkont(CONTROL c,state * mystate)
{
   return 1;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    applycont
 *  Description:    apply continuation
 * =====================================================================================
 */
FUNCTIONALITY TERM applycont(TERM v,state * st)
{
  // Patter macht the executing
  if( st->continuation.x->t != EXECUTING ) exit(1); 
  struct executing * ffik = (st->continuation.x);
  kont * k = &ffik->k;

  if(k->a->t == APPKONT)
  {
    TERM temp = k->a->expr;
    kont * kk = k->a->k; 
    st->environment = k->a->env;
    k->a2       = (struct appkont2 *) mymalloc(sizeof(struct appkont2));
    k->a2->t    = APPKONT2;
    k->a2->expr = v;
    k->a2->k    = kk;
    return run(temp,st);
  }
  else if(k->a2->t == APPKONT2)
  {
    struct secClosure * cl = k->a2->expr.c;
    if(cl->t == CLOSURE)
    {
      secinsert(cl->env,cl->x.s->name,v.b);
      st->environment  = cl->env;
      k = k->a2->k;
      return run(cl->body,st);
    }
    else{
      DEBUG_PRINT(("Expected to Apply a Closure"))
      exit(1);
    }
  }
  else if(k->d->t == DONE){
    // st->continuation = k.r->k;
    return v;     
  }
  else if(k->lt->t == LETKONT)
  {
    DEBUG_PRINT(("Let kont"))
    struct letkont * klet = k->lt;
    secinsert(klet->env,klet->var.s->name,v.b);
    st->environment = klet->env;
    k = klet->k;
    return run(klet->body,st);
  }
  else if(k->i->t == IFKONT)
  {
    DEBUG_PRINT(("IFKONT"))
    struct ifkont * kif = k->i;
    st->environment = kif->env;
    k = kif->k;
    if(v.b->value) { return run(kif->cons,st); }
      else { return run(kif->alt,st); }
  }
  else{
    DEBUG_PRINT(("Secure :: Unkown Closure"))
    exit(1);
  }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    run
 *  Description:    run the cek
 * =====================================================================================
 */
static TERM run(TERM expr, state * st){

  // Patter macht the executing
  if( st->continuation.x->t != EXECUTING ) exit(1); 
  struct executing * ffik = (st->continuation.x);
  kont * k = &ffik->k;

  if(expr.b->t == BOOLEAN || expr.c->t == CLOSURE){
      return applycont(expr,st); 
  }
  else if (expr.s->t == SYMBOL)
  {
    TERM val;
    val.b = secget(st->environment,(void *) expr.s->name); 
    //val.b = secget(st->storage,(void *) c);
    if(val.b == NULL){
      DEBUG_PRINT(("Secure :: Variable Not Found"))
        exit(1);
    }
    return applycont(val,st);
  }
  else if (expr.l->t == LAM){
    return applycont(MakesecClosure(expr.l->argument,expr.l->body,seccopyenv(st->environment)),st);  
  }
  else if(expr.a->t == APPLICATION)
  {
    kont * prev = NULL;
    if(k != NULL){prev = k;}
    k = mymalloc(sizeof(kont));
    k->a = (struct appkont *) mymalloc(sizeof(struct appkont)); 
    k->a->t = APPKONT;
    k->a->expr = expr.a->argument;
    k->a->env  = seccopyenv(st->environment);
    k->a->k    = prev;
    return run(expr.a->function,st);
  }
  else if(expr.lt->t == LET)
  {
    DEBUG_PRINT(("LET"))
    kont * prev = NULL;
    if(k != NULL) prev = k;
    k = mymalloc(sizeof(kont));
    k->lt = (struct letkont *) mymalloc(sizeof(struct letkont));
    k->lt->t = LETKONT;
    k->lt->var = expr.lt->var;
    k->lt->body = expr.lt->body;
    k->lt->env  = st->environment;
    k->lt->k    = prev;
    return run(expr.lt->expr,st);
  }
  else if(expr.i->t == IF){
    DEBUG_PRINT(("IF"))
    kont * prev = NULL;
    if(k != NULL) prev = k;
    k = mymalloc(sizeof(kont));
    k->i = (struct ifkont *) mymalloc(sizeof(struct ifkont));
    k->i->t = IFKONT;
    k->i->cons = expr.i->cons;
    k->i->alt = expr.i->alt;
    k->i->env  = st->environment;
    k->i->k    = prev;
    return run(expr.i->cond,st);
  }
  else if(expr.f->t == INSEC){

    DEBUG_PRINT(("Calling the attacker"))
        
    // make Continue continuation
    kont * prev = NULL;

  /*      st->continuation = mymalloc(sizeof(kont)); 
        st->continuation->cc    = (struct excont *) mymalloc(sizeof(struct excont));
        st->continuation->cc->t = SECONT;
        st->continuation->cc->k = prev;
        st->continuation->cc->env = seccopyenv(st->environment);
*/
       /* void * dle = inseceval((expr.i->term).b); 
        Value ptr = {.b = dle};
        
        if(ptr.b->t == BOOLEAN){
            return run(MakesecBoolean(ptr.b->value),st);       
        }
        else if(ptr.c->t == CLOSURE){
            int name = getname();
            //secinsert(mystate->storage,(void *)c,(void *) (MakesecSymbol("y")).b);
            secinsert(mystate->namemap,(void *)name, (void *) (MakesecSymbol("y")).b );
            return run(MakesecLambda(MakeSI(MakeApplication(ptr,MakeName(name))),MakesecSymbol("y")),st); 
        } */
    return run(MakeFI(1),st);
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
 *  namemap for the left machine
 *-----------------------------------------------------------------------------*/

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    secure_eval
 *  Description:    run the cesk
 * =====================================================================================
 */
ENTRYPOINT long application(long name,long argument){
  return 1;
}

ENTRYPOINT long allocation (long argument,long type){
  return 1;
}

ENTRYPOINT long set(long name,long argument){
  return 1;
}

ENTRYPOINT long dereference(long name){
  return 1;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    load 
 *  Description:    upload program to storage
 * =====================================================================================
 */
ENTRYPOINT long start (void){

  // simplification
  TERM expr = MakesecLambda(MakesecSymbol( "x" ),T(Int),MakesecSymbol( "x" )) ;
  TYPE type = MakeTArrow(T(Int),T(Int));
  ffikont kont; 
  struct executing * data = (struct executing*) mymalloc(sizeof(struct executing));
  kont.x = data;
  kont.x->t = EXECUTING;
  kont.x->k = Done;
  kont.x->ty = type; 
  kont.x->outerk = Empty;
  mystate = mymalloc(sizeof(state));

  static ENV tbl = {NULL,mycmp,0};
  ENV * envtable = (ENV *) mymalloc(sizeof(ENV));  
  *envtable = tbl;
  mystate->environment = envtable;

  ENV * funtbl = (ENV *) mymalloc(sizeof(ENV));  
  *funtbl = tbl;
  funtbl->cmp = mycmpint;
  mystate->namemap = funtbl;
  mystate->continuation = kont;

  CONTROL c;
  c.t = expr;
  return plug_outerkont(c,mystate);
}


