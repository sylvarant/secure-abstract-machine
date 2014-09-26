/*
 * =====================================================================================
 *
 *       Filename:  cek.c
 *
 *    Description:  standard cek for the lambda calculus
 *
 *         Author:  tea
 *        Company:  Superstar Uni
 *
 * =====================================================================================
 */

// includes
#include "attacker.h"


/*-----------------------------------------------------------------------------
 *  Global variables
 *-----------------------------------------------------------------------------*/
static state * instate;


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeBoolean
 *  Description:    create a Value BOOLEAN
 * =====================================================================================
 */
extern Value MakeBoolean(unsigned int b) {
    Value v;
    struct Boolean * data = (struct Boolean*) mymalloc(sizeof(struct Boolean));
    v.b = data;
    v.b->t = BOOLEAN ;
    v.b->value = b ;
    return v ;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeLambda
 *  Description:    create a Value LAM
 * =====================================================================================
 */
extern Value MakeLambda(Value body,Value arg){

    Value v;

    struct Lambda * data = (struct Lambda*) mymalloc(sizeof(struct Lambda));
    v.l = data;
    v.l->t = LAM;
    v.l->argument = arg;
    v.l->body  = body;

    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeApplication
 *  Description:    create a Value APPLICATION
 * =====================================================================================
 */
extern Value MakeApplication(Value a,Value b){
    

    Value v;
    struct Application * data = (struct Application*) mymalloc(sizeof(struct Application));
    v.a = data;
    v.a->t         = APPLICATION;
    v.a->argument  = b;
    v.a->function  = a;
    
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeSymbol
 *  Description:    create a Value SYMBOL
 * =====================================================================================
 */
extern Value MakeSymbol(char * name){
    Value v;
    struct Symbol * data = (struct Symbol*) mymalloc(sizeof(struct Symbol));
    v.s = data;
    v.s->t    = SYMBOL;
    v.s->name = name;
    return v;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeName
 *  Description:    create a Label to the outside
 * =====================================================================================
 */
extern Value MakeName(int f){   

    Value i ;
    struct Name * inter = mymalloc(sizeof(struct Name)); 
    i.i = inter;
    i.i->label = f;
    i.i->t = NAME;
    return i;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeIf
 *  Description:    create a Value IF
 * =====================================================================================
 */
extern Value MakeIf(Value a, Value b, Value c)
{
    Value v;
    struct If * data = (struct If*) mymalloc(sizeof(struct If));
    v.ii  = data;
    v.ii->t = IF;
    v.ii->cond = a;
    v.ii->cons = b;
    v.ii->alt = c;
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeIf
 *  Description:    create a Value COMP
 * =====================================================================================
 */
extern Value MakeComp(Value a, Value b)
{
    Value v;
    struct Comp * data = (struct Comp*) mymalloc(sizeof(struct Comp));
    v.co  = data;
    v.co->t = COMP;
    v.co->left = a;
    v.co->right = b;
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeLet
 *  Description:    create a Value Let
 * =====================================================================================
 */
extern Value MakeLet(Value a, Value b, Value c)
{
    Value v;
    struct Let * data = (struct Let*) mymalloc(sizeof(struct Let));
    v.lt  = data;
    v.lt->t = LET;
    v.lt->var = a;
    v.lt->expr = b;
    v.lt->body = c;
    return v;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    MakeClosure
 *  Description:    create a Value Closure
 * =====================================================================================
 */
static Value MakeClosure(Value x,Value body, environ * htbl)
{
    Value clos;
    struct Closure * data = (struct Closure*) mymalloc(sizeof(struct Closure));
    clos.c = data;
    clos.c->t      = CLOSURE;
    clos.c->x = x;
    clos.c->body = body;
    clos.c->env = copyenv(htbl);
	return clos;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    startstate
 *  Description:    create a startpoint for the CEK
 * =====================================================================================
 */
extern state * startstate(void){

    static environ tbl = {NULL,0};
	#ifndef STATIC_MEM
		if(instate != NULL) free(instate);
	#endif
    instate = mymalloc(sizeof(state));
    environ * envtable = (environ *) mymalloc(sizeof(environ));  
    *envtable = tbl;
    instate->environment = envtable;
    instate->continuation = NULL;
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    inapplycont
 *  Description:    apply continuation
 * =====================================================================================
 */
static Value inapplycont(Value v){

    if(instate->continuation == NULL){

        DEBUG_PRINT(("Return %d",v.b->t))
        return v;
    }
    else if(instate->continuation->e->t == EXEC1){
        Value temp               = instate->continuation->e->expr;
        kont * kk                = instate->continuation->e->k; 
        instate->environment            = instate->continuation->e->env;
        instate->continuation->c       = (struct execlo *) mymalloc(sizeof(struct execlo));
        instate->continuation->c->t    = EXECLO;
        instate->continuation->c->expr = v;
        instate->continuation->c->k    = kk;
        DEBUG_PRINT(("Getting ready to execlo %d for %d",temp.b->t,v.b->t))
        return run(temp);
    }
    else if(instate->continuation->c->t == EXECLO){
        struct Closure * cl =  instate->continuation->c->expr.c;
        if(cl->t == CLOSURE){
            insert(cl->env,cl->x.s->name,v.b);
            instate->environment  = cl->env;
            instate->continuation = instate->continuation->c->k;
            return run(cl->body);
        }
        else{DEBUG_PRINT(("Applying non closure %d vs %d",cl->t,CLOSURE))
            exit(1);
        }
    }
	else if(instate->continuation->cc->t == CONT){
       instate->environment  = instate->continuation->cc->env;
       instate->continuation = instate->continuation->cc->k;
       return run(v);
    }
	else if(instate->continuation->r->t == RET){
        instate->continuation = instate->continuation->r->k;
		return v;     
    }
    else if(instate->continuation->lt->t == KLET){
        DEBUG_PRINT(("KLET"))
        struct exlet * klet = instate->continuation->lt;
        insert(klet->env,klet->var.s->name,v.b);
        instate->environment = klet->env;
        instate->continuation = instate->continuation->lt->k;
        return run(klet->body);
    }
    else if(instate->continuation->ii->t == KIF){
        DEBUG_PRINT(("KIF"))
        struct exif * kif = instate->continuation->ii;
        instate->environment = kif->env;
        instate->continuation = instate->continuation->ii->k;
        if(v.b->value) { return run(kif->cons); }
        else { return run(kif->alt); }
    }
    else{DEBUG_PRINT(("Unkown Closure in inapplycont"))
            exit(1);
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    evaluate
 *  Description:    main entry point for the outside
 *					argument is a pointer to deal with Sancus restriction
 * =====================================================================================
 */
extern void * evaluate(void * p){ 

	// make Return continuation
    kont * prev = NULL;
    if(instate->continuation != NULL){prev = instate->continuation;}

    instate->continuation = mymalloc(sizeof(kont)); 
    instate->continuation->r    = (struct exret *) mymalloc(sizeof(struct exret));
    instate->continuation->r->t = RET;
    instate->continuation->r->k = prev;

    Value i = {.b = p}; 
	Value x = run(i); 
	return x.b;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    run
 *  Description:    run the crooked cek
 * =====================================================================================
 */
extern Value run(Value expr){
    if(expr.b == NULL){
        DEBUG_PRINT(("MEMORY FAILURE SOMEWHERE"))
    }
    if(expr.b->t == BOOLEAN || expr.c->t == CLOSURE){
        DEBUG_PRINT(("BOOLEAN CLO %d",expr.b->t))
        return inapplycont(expr); 
    }
    else if ( expr.s->t ==  SYMBOL){
        DEBUG_PRINT(("SYM"))
        Value val;
        val.b = get(instate->environment,(const char *) expr.s->name); 
        if(val.b == NULL){
            DEBUG_PRINT(("INSEC NULL POINTER"))
            exit(1);
        }
        return inapplycont(val);
    }
    else if (expr.l->t == LAM){
        DEBUG_PRINT(("LAM"))
       Value y = inapplycont(MakeClosure(expr.l->argument,expr.l->body,copyenv(instate->environment)));  
       return y;
    }
    else if(expr.a->t == APPLICATION){

        DEBUG_PRINT(("APPL"))
        kont * prev = NULL;
        if(instate->continuation == NULL){
        }else{prev = instate->continuation;}

        instate->continuation = mymalloc(sizeof(kont));

        instate->continuation->e = (struct exec1 *) mymalloc(sizeof(struct exec1)); 
        instate->continuation->e->t = EXEC1;
        instate->continuation->e->expr = expr.a->argument;
        instate->continuation->e->env  = instate->environment;
        instate->continuation->e->k    = prev;
        return run(expr.a->function);
    }
    else if(expr.lt->t == LET){
        DEBUG_PRINT(("LET"))
        kont * prev = NULL;
        if(instate->continuation != NULL) prev = instate->continuation;
        instate->continuation = mymalloc(sizeof(kont));

        instate->continuation->lt = (struct exlet *) mymalloc(sizeof(struct exlet));
        instate->continuation->lt->t = KLET;
        instate->continuation->lt->var = expr.lt->var;
        instate->continuation->lt->body = expr.lt->body;
        instate->continuation->lt->env  = instate->environment;
        instate->continuation->lt->k    = prev;
        return run(expr.lt->expr);
    }
    else if(expr.ii->t == IF){
        DEBUG_PRINT(("IF"))
        kont * prev = NULL;
        if(instate->continuation != NULL) prev = instate->continuation;
        instate->continuation = mymalloc(sizeof(kont));

        instate->continuation->ii = (struct exif *) mymalloc(sizeof(struct exif));
        instate->continuation->ii->t = KIF;
        instate->continuation->ii->cons = expr.ii->cons;
        instate->continuation->ii->alt = expr.ii->alt;
        instate->continuation->ii->env  = instate->environment;
        instate->continuation->ii->k    = prev;
        return run(expr.ii->cond);
    }
    else if(expr.co->t == COMP){
        DEBUG_PRINT(("COMP"))
       
        expr = MakeBoolean(expr.co->left.b == expr.co->right.b);
        return inapplycont(expr);
    }
    else if(expr.i->t == NAME){

        DEBUG_PRINT(("NAME"))

		// make Continue continuation
        kont * prev = NULL;
        if(instate->continuation != NULL){prev = instate->continuation;}

        instate->continuation = mymalloc(sizeof(kont)); 
        instate->continuation->cc    = (struct excont *) mymalloc(sizeof(struct excont));
        instate->continuation->cc->t = CONT;
        instate->continuation->cc->k = prev;
        instate->continuation->cc->env = copyenv(instate->environment);
		
        Value val;
        val.b = secure_eval(expr.i->label);  

        if(val.b == -1){
            DEBUG_PRINT(("Invalid Label"))
            exit(1);
        }

        return run(val);
    }
    else{
		DEBUG_PRINT(("Unkown State"))
        exit(1);
	}
}



