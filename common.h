#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

typedef enum _NodeType 
{
    CODE_N,     PROC_N, FUNC_N,     PARAMLIST_N, EMPTY_PARAMLIST,     BLOCK_N, DECLARATIONS_N, INNERBLOCK_N,     RETURN_N,     ASSIGNMENT_N,     ASSIGNMENT_BYINDEX_N,     ASSIGNMENT_DEREF_N,     VAR_N, VARLIST_N,     IF_N, IFELSE_N,     WHILE_N,     CALL_N,     ARGS_N,     tVOID_N, tINT_N, tCHAR_N, tREAL_N, tBOOL_N, tSTRING_N,     tINTPTR_N, tCHARPTR_N, tREALPTR_N,     ASS_N,     EXPR_N,     LOGICALAND_N,     LOGICALOR_N,
    FIRST_BINARY,
    EQ_N = FIRST_BINARY,     NE_N,
    GT_N,
    GE_N,
    LT_N,
    LE_N,
    MUL_N,     DIV_N,
    PLUS_N,
    MINUS_N,
    NOT_N,     ADDRESS_N,     DEREF_N,     STRLEN_N,     IDENTIFIER_N,     ARITHEXPR_N,     UNARYPLUS_N,     UNARYMINUS_N,    NULLPTR_N,     BOOLLITERAL_N,     CHARLITERAL_N,     INTLITERAL_N,     REALLITERAL_N,     STRINGLITERAL_N,     ARRAY_ACCESS_N
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
#define isnum(x) (x == tINT || x == tREAL)
#define CALLABLE_FLAG (1 << 0)
typedef struct _table_entry
{
    str identifier;
    /* Put here additional entry info members */
    Type type;
    int flags;     Type* parmeters;
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

Type verify_call(node* call_expr, FILE* f, str* t);
Type evaltype(node* expr, FILE* f, str* t);
void analyzer(node* n);
void process_node(node* n, FILE* f);

#define CALLABLE_ID 0
#define FUNC_TYPE 1
#define FUNC_PARAMS 2
#define FUNC_BLOCK 3
#define PROC_PARAMS 1
#define PROC_BLOCK 2
#define IS_FUNC(x) (x->nodetype == FUNC_N)

#endif