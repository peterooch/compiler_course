#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

typedef enum _NodeType 
{
    CODE_N, // DONE
    PROC_N, FUNC_N, // DONE
    PARAMLIST_N, EMPTY_PARAMLIST, // DONE
    BLOCK_N, DECLARATIONS_N, INNERBLOCK_N, // DONE
    RETURN_N, // DONE
    ASSIGNMENT_N, // DONE
    ASSIGNMENT_BYINDEX_N, // DONE
    ASSIGNMENT_DEREF_N, // DONE
    VAR_N, VARLIST_N, // DONE
    IF_N, IFELSE_N, // DONE
    WHILE_N, // DONE
    CALL_N, // DONE
    ARGS_N, // DONE
    tVOID_N, tINT_N, tCHAR_N, tREAL_N, tBOOL_N, tSTRING_N, //DONE
    tINTPTR_N, tCHARPTR_N, tREALPTR_N, // DONE
    ASS_N, // DONE
    EXPR_N, // Everything bigger than this is an expression, NOT TO BE USED directly
    LOGICAL_N, // DONE
    COMP_N, // DONE
    ARITHCOMP_N, // DONE
    ARITH_N, // DONE
    NOT_N, // DONE
    ADDRESS_N, // DONE
    DEREF_N, // DONE
    STRLEN_N, // DONE
    IDENTIFIER_N, // DONE
    ARITHEXPR_N, // DONE
    UNARYEXPR_N, // DONE
    NULLPTR_N, // DONE
    BOOLLITERAL_N, // DONE
    CHARLITERAL_N, // DONE
    INTLITERAL_N, // DONE
    REALLITERAL_N, // DONE
    STRINGLITERAL_N, // DONE
} NodeType;

typedef char* str;
typedef struct _node
{
    NodeType nodetype;
    int nchildren;
    str data;
    int line;
    struct _node** children;
} node;

node* mkleaf(NodeType n);
node* mkleaf_str(NodeType n, const str data);
node* mknode(NodeType n, int count, ...);
void printtree(node* tree, int spacing);

#define PBIT 0x10
typedef enum _Type
{
    tVOID, /* PROCs */
    tINT,
    tCHAR,
    tREAL,
    tBOOL,
    tSTRING,
    tNULLPTR = PBIT,
    tINTPTR,
    tCHARPTR,
    tREALPTR
} Type;

/* Utility macros */
#define ISPTRTYPE(x) (x >= tINT && x <= tREAL)
#define ISPTR(x)     (x & PBIT)
#define TOPTR(x)     (x | PBIT)
#define TOSCALAR(x)  (x & ~PBIT)

#define CALLABLE_FLAG (1 << 0)
typedef struct _table_entry
{
    str identifier;
    /* Put here additional entry info members */
    Type type;
    int flags; // Is identifier function/procedure
    Type* parmeters;
    int nparameters;
    /* Tree data */
    struct _table_entry* left;
    struct _table_entry* right;
    int height;
} table_entry;

typedef struct _scopestack
{
    table_entry* symbol_table;
    Type return_type;
    struct _scopestack* next;
} scopestack, *scopestack_ptr;

Type verify_call(node* call_expr);
Type evaltype(node* expr);
void analyzer(node* n);
void process_node(node* n);

#define CALLABLE_ID 0
#define FUNC_TYPE 1
#define FUNC_PARAMS 2
#define FUNC_BLOCK 3
#define PROC_PARAMS 1
#define PROC_BLOCK 2
#define IS_FUNC(x) (x->nodetype == FUNC_N)

#endif