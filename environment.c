/*
 * =====================================================================================
 *
 *       Filename:  environment.c
 *
 *    Description: An environment for the attacker side
 * =====================================================================================
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "environment.h"

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    get
 *  Description:    return an element 
 * =====================================================================================
 */
extern void * get(environ *table,const char *key)
{
    struct envnode *node;
    node = table->bucket;
    while(node) {
        if(strcmp(key,node->key) == 0)
            return node->value;
        node = node->next;
    }
    return NULL;
}


/* 
 * ===  FUNCTION  ======================================================================
 *         Name:    insert
 *  Description:    add something to environment
 * =====================================================================================
 */
extern int insert(environ *table,char *key,void * value)
{
    struct envnode **tmp;
    struct envnode *node ;

    tmp = &table->bucket;
    while(*tmp) {
        if(strcmp(key,(*tmp)->key) == 0)
            break;
        tmp = &(*tmp)->next;
    }
    if(*tmp) { 
        node = *tmp;
    } else {
        node = malloc(sizeof *node);
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
 *         Name:    copyenv
 *  Description:    copy the env
 * =====================================================================================
 */
extern environ * copyenv(environ * table){
    environ * new = (environ *) malloc(sizeof(environ));
    new->size = table->size;

    struct envnode *node;
    struct envnode *nnode;
    node = table->bucket;
    new->bucket = (struct envnode *) malloc(sizeof(struct envnode)); 

    nnode =  new->bucket;
	struct envnode * dumb = nnode;
    while(node != NULL) {
        nnode->key             = node->key;
        nnode->value           = node->value;
        nnode->next            = (struct envnode *) malloc(sizeof(struct envnode)); 
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


