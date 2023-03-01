#include <R.h>
#include <Rdefines.h>
#include "statsR.h"
#include "stats.h"

/*
 * Author: Aidan Lakshman
 * Contact: AHL27@pitt.edu
 *
 * This is a set of C functions that apply an R function to all internal
 * nodes of a dendrogram object. This implementation runs roughly 2x
 * faster than base `stats::dendrapply`, and deals with dendrograms
 * with high numbers of internal branches. Notably, this implementation
 * unrolls the recursion to prevent any possible stack overflow errors. 
 *
 */

/*
 * Linked list struct
 *
 * Each node of the tree is added with the following args:
 *  -       node: tree node, as a pointer to SEXPREC object
 *  -          v: location in parent node's list
 *  -     isLeaf: Counter encoding unmerged children. 0 if leaf or leaf-like subtree.
 *  - origLength: original length of the node before applying function 
 *  -     parent: pointer to node holding the parent node in the tree
 *  -       next: next linked list element
 * 
 */
typedef struct ll_S_dendrapply {
  SEXP node;
  int v;
  unsigned int remove : 1;
  signed int isLeaf : 7;
  unsigned int origLength;
  struct ll_S_dendrapply *parent;
  struct ll_S_dendrapply *next;
} ll_S_dendrapply;


/* Global variable for on.exit() free */
ll_S_dendrapply *dendrapply_ll;
static SEXP leafSymbol;
static PROTECT_INDEX headprot;

/* 
 * Frees the global linked list structure.
 *
 * Called using on.exit() in R for cases where
 * execution is stopped early.
 */
void free_dendrapply_list(){
  ll_S_dendrapply *ptr = dendrapply_ll;
  while(dendrapply_ll){
    dendrapply_ll = dendrapply_ll->next;
    free(ptr);
    ptr=dendrapply_ll;
  }

  return;
}

/* Function to allocate a LL node */
ll_S_dendrapply* alloc_link(ll_S_dendrapply* parentlink, SEXP node, int i, short travtype){
  ll_S_dendrapply *link = malloc(sizeof(ll_S_dendrapply));
  SEXP ls;

  if(travtype == 0){
    link->node = NULL;
    link->isLeaf = -1;
    link->origLength = 0;
  } else if (travtype == 1){
    SEXP curnode;
    curnode = VECTOR_ELT(node, i);
    link->node = curnode;
    ls = getAttrib(curnode, leafSymbol);
    link->isLeaf = (isNull(ls) || (!LOGICAL(ls)[0]) )? length(curnode) : 0;
    link->origLength = link->isLeaf;
  }

  link->next = NULL;
  link->v = i;
  link->parent = parentlink;
  link->remove = 0;

  return link;
}


/*
 * Main workhorse function.
 * 
 * This function traverses the tree according to the traversal style
 * specified, applying the R function as necessary. R ensures that the
 * dendrogram isn't a leaf, so this function assmes the dendrogram has 
 * at least two members.
 */
SEXP new_apply_dend_func(ll_S_dendrapply *head, SEXP f, SEXP env, short travtype){
  ll_S_dendrapply *ptr, *prev, *parent;
  SEXP node, call, newnode, leafVal;

  if(travtype == 0){
    call = PROTECT(LCONS(f, LCONS(head->node, R_NilValue)));
    REPROTECT(head->node = R_forceAndCall(call, 1, env), headprot);
    UNPROTECT(1);
  }

  int n, nv;
  ptr = head;
  prev = head;
  while(ptr){
    R_CheckUserInterrupt();
    /* lazily populate node, apply function to it as well */
    if (travtype==0 && ptr->isLeaf==-1){
      parent = ptr->parent;
      newnode = VECTOR_ELT(parent->node, ptr->v);
      leafVal = getAttrib(newnode, leafSymbol);
      ptr->isLeaf = (isNull(leafVal) || (!LOGICAL(leafVal)[0])) ? length(newnode) : 0;
      ptr->origLength = ptr->isLeaf;
      call = PROTECT(LCONS(f, LCONS(newnode, R_NilValue)));
      newnode = PROTECT(R_forceAndCall(call, 1, env));
      n = length(ptr->parent->node);
      nv = ptr->v;
      while(nv < n){
        /* trying to replicate a weird stats::dendrapply quirk */
        SET_VECTOR_ELT(parent->node, nv, newnode);
        nv += ptr->parent->origLength;
      }
      UNPROTECT(2);

      /* double ELT because it avoids a protect */
      ptr->node = VECTOR_ELT(parent->node, ptr->v);
    }

    if (ptr->remove){
      /* these are nodes flagged for deletion */
      prev->next = prev->next->next;
      free(ptr);
      ptr = prev->next;

    } else if(ptr->isLeaf == 0){
      /* 
      * If the LL node is a leaf or completely merged subtree,
      * apply the function to it and then merge it upwards
      */
      while(ptr->isLeaf == 0 && ptr != head){
        /* 
         * merge upwards, 
         * protection unneeded since parent already protected 
         */
        prev = ptr->parent;
        if(travtype == 0){
          SET_VECTOR_ELT(prev->node, ptr->v, ptr->node);
        } else if(travtype == 1){
          call = PROTECT(LCONS(f, LCONS(ptr->node, R_NilValue)));
          newnode = PROTECT(R_forceAndCall(call, 1, env));

          prev = ptr->parent;
          SET_VECTOR_ELT(prev->node, ptr->v, newnode);
          UNPROTECT(2);
        }

        prev->isLeaf -= 1;

        /* flag node for deletion later */
        ptr->remove = 1;
        ptr = prev;
        prev = ptr;
        R_CheckUserInterrupt();
      }

      /* go to the next element so we don't re-add */
      ptr = ptr->next;

    } else {
      /* ptr->isLeaf != 0, so we need to add nodes */
      node = ptr->node;
      n = ptr->origLength;
      leafVal = getAttrib(node, leafSymbol);
      
      if(isNull(leafVal) || (!LOGICAL(leafVal)[0])){
        ll_S_dendrapply *newlink;
        /*
         * iterating from end to beginning to ensure 
         * we traverse depth-first instead of breadth
         */
        for(int i=n-1; i>=0; i--){
          newlink = alloc_link(ptr, node, i, travtype);
          newlink->next = ptr->next;
          ptr->next = newlink;
        }
      }
      prev = ptr;
      ptr = ptr->next; 
    }
  }

  if (travtype == 1){
    call = PROTECT(LCONS(f, LCONS(head->node, R_NilValue)));
    REPROTECT(head->node = R_forceAndCall(call, 1, env), headprot);
    UNPROTECT(1);
  }
  
  return head->node;
}

/*
 * Main Function
 * 
 * Calls helper functions to build linked list,
 * apply function to all nodes, and reconstruct
 * the dendrogram object. Attempts to free the linked list 
 * at termination, but note memory free not guaranteed to 
 * execute here due to R interrupts. on.exit() used in R to 
 * account for this.
 */
SEXP do_dendrapply(SEXP tree, SEXP fn, SEXP env, SEXP order){
  /* 0 for preorder, 1 for postorder */
  leafSymbol = install("leaf");
  short travtype = INTEGER(order)[0];
  SEXP treecopy;
  PROTECT_WITH_INDEX(treecopy = duplicate(tree), &headprot);

  /* Add the top of the tree into the list */
  dendrapply_ll = malloc(sizeof(ll_S_dendrapply));
  dendrapply_ll->node = treecopy;
  dendrapply_ll->next = NULL;
  dendrapply_ll->parent = NULL;
  dendrapply_ll->isLeaf = length(treecopy);
  dendrapply_ll->origLength = dendrapply_ll->isLeaf;
  dendrapply_ll->v = -1;
  dendrapply_ll->remove = 0;

  /* Apply the function to the list */
  treecopy = new_apply_dend_func(dendrapply_ll, fn, env, travtype);
  
  /* Attempt to free the linked list and unprotect */

  free_dendrapply_list();
  UNPROTECT(1);
  return treecopy;
}