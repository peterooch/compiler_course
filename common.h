#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

static char msg[512];

#define POST_MSG(func, fmt, ...) \
		sprintf(msg, fmt, __VA_ARGS__); \
		func(msg)
#define ERR_MSG(fmt, ...) \
		POST_MSG(error, fmt, __VA_ARGS__)
#define DBG_MSG(fmt, ...) \
		POST_MSG(printf, fmt, __VA_ARGS__)

typedef enum _NodeType 
{
    CODE_N,
    PROC_N, FUNC_N,
    PARAMLIST_N, EMPTY_PARAMLIST,
    BLOCK_N, DECLARATIONS_N, INNERBLOCK_N,
    RETURN_N,
    ASSIGNMENT_N,
    ASSIGNMENT_BYINDEX_N,
    ASSIGNMENT_DEREF_N,
    VAR_N, VARLIST_N,
    IF_N, IFELSE_N,
    WHILE_N,
    CALL_N, /* FIXME This can be an expression and not one */
	ARGS_N, EMPTY_ARGS_N,
    tVOID_N, tINT_N, tCHAR_N, tREAL_N, tBOOL_N, tSTRING_N,
    tINTPTR_N, tCHARPTR_N, tREALPTR_N,
    ASS_N,
    EXPR_N,
    LOGICAL_N,
    COMP_N,
    ARITH_N,
    NOT_N,
    ADDRESS_N,
    DEREF_N,
    STRLEN_N,
    IDENTIFIER_N,
    NULLPTR_N,
    BOOLLITERAL_N,
    CHARLITERAL_N,
    INTLITERAL_N,
    REALLITERAL_N,
    STRINGLITERAL_N,
    BOOLEXPR_N,
    ARITHEXPR_N,
    UNARYEXPR_N,
    POINTEREXPR_N,
} NodeType;

typedef char* str;
typedef struct _node
{
    NodeType nodetype;
    int nchildren;
    str data;
    struct _node** children;
} node;

node* mkleaf(NodeType n);
node* mkleaf_str(NodeType n, const str data);
node* mknode(NodeType n, int count, ...);
void printtree(node* tree, int spacing);

#define PBIT 0x10
#define FBIT 0x100
typedef enum _Type
{
    tINT = 0x1,
    tCHAR,
    tREAL,
    tBOOL,
    tSTRING,
    tVOID, /* PROCs */
    tNULLPTR = PBIT,
    tINTPTR,
    tCHARPTR,
    tREALPTR
} Type;

/* Utility macros */
#define ISPTR(x)    (x & PBIT)
#define TOPTR(x)    (x | PBIT)
#define TOSCALAR(x) (x & ~PBIT)
#define TOFMASK(x)  (x | FBIT)
#define ISFMASK(x)  (x & FBIT)
#define NOFMASK(x)  (x & ~FBIT)

typedef struct _param_list
{
    int count;
    Type type;
    struct _param_list* next;
} param_list;

typedef struct _table_entry
{
    str identifier;
    /* Put here additional entry info members */
    Type type;
    param_list* parmeters;
    /* Tree data */
    struct _table_entry* left;
    struct _table_entry* right;
    int height;
} table_entry;

typedef struct _scopestack
{
    table_entry** symbol_table;
    struct _scopestack* next;
} scopestack, *scopestack_ptr;

void process_node(node* n);

#define CALLABLE_ID 0
#define FUNC_TYPE 1
#define FUNC_PARAMS 2
#define FUNC_BLOCK 3
#define PROC_PARAMS 1
#define PROC_BLOCK 2
#define IS_FUNC(x) (x->nodetype == FUNC_N)

#endif