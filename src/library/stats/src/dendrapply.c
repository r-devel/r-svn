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
 * Full Description of implementation:
 *
 * ll_S_dendrapply is essentially a doubly-linked list node struct with
 * some additional data. For each node, we lazily allocated a node containing
 * a pointer to the SEXP object, a pointer to the next element of the linked list,
 * and a pointer to the node containing the parent SEXP object from R. For each
 * node in the dendrogram, we create a linked list node for it in C, allowing
 * us to traverse the linked list iteratively to evaluate the function at all nodes.
 * The secondary link is to reconstruct the original dendrogram structure.
 * See below for description of the additional data in the struct.
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
static SEXP leafSymbol, class;
static PROTECT_INDEX headprot;
static int throwWarning;

/*
 * Function to allocate a dummy LL node
 * Lazy, values are filled with drp_assign_dendnode_child
 */
ll_S_dendrapply* drp_alloc_link(ll_S_dendrapply* parentlink, int i){
  ll_S_dendrapply *link = malloc(sizeof(ll_S_dendrapply));
  link->node = NULL;
  link->isLeaf = -1;
  link->origLength = 0;
  link->next = NULL;
  link->v = i;
  link->parent = parentlink;
  link->remove = 0;

  return link;
}

/*
 * Get the i'th element of a dendrogram node
 * Replicates functionality of stats:::`[[.dendrogram`
 * without having to go back into R
 */
SEXP drp_get_dend_child(ll_S_dendrapply* link, int i, int shouldReclass){
  SEXP curnode = VECTOR_ELT(link->node, i);
  if(shouldReclass)
    classgets(curnode, class);
  return(curnode);
}

/*
 * Assign the child of a node to a link
 * Some traversals use lazy evaluation; this fills in these unevaluated nodes
 */
ll_S_dendrapply* drp_assign_dendnode_child(ll_S_dendrapply* link, ll_S_dendrapply* parentlink, int i){
  SEXP curnode = drp_get_dend_child(parentlink, i, 1);
  link->node = curnode;
  SEXP ls = getAttrib(curnode, leafSymbol);
  link->isLeaf = (isNull(ls) || (!LOGICAL(ls)[0])) ? length(curnode) : 0;
  link->origLength = link->isLeaf;

  return link;
}

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


/*
 * Main workhorse function.
 *
 * This function traverses the tree and applies the function to each node.
 * If INORDER, we apply the function to the node and then add its children
 * to the linked list. If POSTORDER, we add children first, and apply the
 * function once all children have been evaluated.
 *
 * Once all the children of a node have been processed, the child subtrees
 * are combined into the parent (this is where function is applied for POSTORDER.
 * R ensures that the dendrogram isn't a leaf, so this function assumes the
 * dendrogram has at least two members.
 */
SEXP dendrapply_internal_func(ll_S_dendrapply* head, SEXP f, SEXP env, short travtype){
  ll_S_dendrapply *ptr, *prev;
  SEXP node, newnode, leafVal;
  SEXP nodeSym = install("node");
  SEXP R_fcall = PROTECT(lang2(f, nodeSym));
  MARK_NOT_MUTABLE(R_fcall);

  /* for inorder traversal, apply function to root and reprotect it */
  if(travtype == 0){
    defineVar(nodeSym, head->node, env);
    REPROTECT(head->node = R_forceAndCall(R_fcall, 1, env), headprot);
  }

  int n, nv;
  ptr = head;
  prev = head;
  while(ptr){
    R_CheckUserInterrupt();

    if (travtype==0 && ptr->isLeaf==-1){
      /* lazily populate node and apply function to it */
      ptr = drp_assign_dendnode_child(ptr, ptr->parent, ptr->v);
      defineVar(nodeSym, ptr->node, env);
      ptr->node = PROTECT(R_forceAndCall(R_fcall, 1, env));

      n = length(ptr->parent->node);
      nv = ptr->v;

      while(nv < n){
        /*
         * replicating a quirk of stats::dendrapply that CRAN depends on.
         * error message may need some work
         */
        SET_VECTOR_ELT(ptr->parent->node, nv, ptr->node);
        nv += ptr->parent->origLength;
      }
      UNPROTECT(1);

      if(n % ptr->parent->origLength != 0){
        /* set signal to throw a warning if the above while loop triggered */
        throwWarning = 1;
      }

      /* double child access because it avoids a protect */
      ptr->node = drp_get_dend_child(ptr->parent, ptr->v, 0);
    }

    if (ptr->remove) {
      /* these are nodes flagged for deletion */
      prev->next = prev->next->next;
      free(ptr);
      ptr = prev->next;

    } else if(ptr->isLeaf == 0) {
      /* If the LL node is a leaf or completely merged subtree, merge it upwards */
      while(ptr->isLeaf == 0 && ptr != head){
        /*
         * merge upwards; protection unneeded since parent already protected
         * postorder traversal evaluates the function at this point, preorder does not
         */
        prev = ptr->parent;
        if(travtype == 0){
          SET_VECTOR_ELT(prev->node, ptr->v, ptr->node);
        } else if(travtype == 1){
          defineVar(nodeSym, ptr->node, env);
          newnode = PROTECT(R_forceAndCall(R_fcall, 1, env));
          prev = ptr->parent;
          SET_VECTOR_ELT(prev->node, ptr->v, newnode);
          UNPROTECT(1);
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
          newlink = drp_alloc_link(ptr, i);
          if(travtype == 1)
            newlink = drp_assign_dendnode_child(newlink, ptr, i);
          newlink->next = ptr->next;
          ptr->next = newlink;
        }
      }
      prev = ptr;
      ptr = ptr->next;
    }
  }

  /* apply function to the root node (last) if post-order traversal */
  if (travtype == 1){
    defineVar(nodeSym, head->node, env);
    REPROTECT(head->node = R_forceAndCall(R_fcall, 1, env), headprot);
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
SEXP dendrapply(SEXP tree, SEXP fn, SEXP env, SEXP order){
  /* 0 for preorder, 1 for postorder */
  leafSymbol = install("leaf");
  short travtype = INTEGER(order)[0];
  throwWarning = 0;

  /* used to replicate stats:::`[[.dendrogram` in C */
  class = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(class, 0, mkChar("dendrogram"));

  SEXP treecopy;
  PROTECT_WITH_INDEX(treecopy = duplicate(tree), &headprot);

  /* Add the top of the tree into the list */
  dendrapply_ll = drp_alloc_link(NULL, -1);
  dendrapply_ll->node = treecopy;
  dendrapply_ll->isLeaf = length(treecopy);
  dendrapply_ll->origLength = dendrapply_ll->isLeaf;

  /* Apply the function to the list */
  treecopy = dendrapply_internal_func(dendrapply_ll, fn, env, travtype);

  /* Throwing a warning if it triggered, may need some work */
  if(throwWarning==1)
    warning("`dendrapply` replicated the return value of at least one function call.");

  /* Attempt to free the linked list and unprotect */
  free_dendrapply_list();
  UNPROTECT(2);
  return treecopy;
}