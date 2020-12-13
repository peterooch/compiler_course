#include "common.h"

#define max(a, b) ((a > b) ? a : b)
#define height(x) (x ? x->height : 0)

#define NTToT(x) case x ## _N : entry->type = x
/* Probably try to switch from a simple string to something that has meaning */
table_entry* alloc_entry(const node* n, NodeType type)
{
    table_entry* entry = (table_entry*)calloc(1, sizeof(table_entry));
    entry->identifier = strdup(n->data);
    entry->height = 1;

    switch (type)
    {
        NTToT(tINT);
        NTToT(tCHAR);
        NTToT(tREAL);
        NTToT(tBOOL);
        NTToT(tSTRING);
        NTToT(tVOID);
        NTToT(tINTPTR);
        NTToT(tCHARPTR);
        NTToT(tREALPTR);
        default:
            entry->type = -1;
    }

    return entry;
}

table_entry* rightRotate(table_entry* y)
{
    table_entry* x = y->left;
    table_entry* t = x->right;

    x->right = y;
    y->left = t;

    y->height = max(height(y->left), height(y->right)) + 1;
    x->height = max(height(x->left), height(x->right)) + 1;

    return x;
}

table_entry* leftRotate(table_entry* x)
{
    table_entry* y = x->right;
    table_entry* t = y->left;

    y->left = x;
    x->right = t;

    x->height = max(height(x->left), height(x->right)) + 1;
    y->height = max(height(y->left), height(y->right)) + 1;

    return y;
}

#define RET(x) { *root = x; return 1; }
/* returns 1 if insertion was successful, returns 0 if item already exists */
int add_entry(table_entry** root, const node* n, NodeType type)
{
    int cmp;
    table_entry* entry;
    str identifier;

    if (!root || !n || !n->data)
        return 0;

    identifier = n->data;
    entry = *root;

    if (!entry)
        RET(alloc_entry(n, type));

    cmp = strcmp(identifier, entry->identifier);

    if (cmp < 0)
    {
        if (!add_entry(&entry->left, n, type))
            return 0;
    }
    else if (cmp > 0)
    {
        if (!add_entry(&entry->right, n, type))
            return 0;
    }
    else
    {
        return 0;
    }

    entry->height = 1 + max(height(entry->left), height(entry->right));

    int balance = height(entry->left) - height(entry->right);

    if (balance > 1)
    {
        cmp = strcmp(identifier, entry->left->identifier);
        if (cmp < 0)
        {
            RET(rightRotate(entry));
        }
        if (cmp > 0)
        {
            entry->left = leftRotate(entry->left);
            RET(rightRotate(entry));
        }
    }
    if (balance < -1)
    {
        cmp = strcmp(identifier, entry->right->identifier);
        if (cmp > 0)
        {
            RET(leftRotate(entry));
        }
        if (cmp < 0)
        {
            entry->right = rightRotate(entry->right);
            RET(leftRotate(entry));
        }
    }
    return 1;
}

/* if insert code is working then lookup code will take O(log(n)) to find the entry
   return a null pointer if was not found */
table_entry* get_entry(table_entry* root, const str identifier)
{
    table_entry* current = root;
    int cmp;

    while (current && (cmp = strcmp(identifier, current->identifier)) != 0)
    {
        if (cmp < 0)
            current = current->left;
        else
            current = current->right;
    }
    return current;
}

void delete_table(table_entry* root)
{
    if (root)
    {
        delete_table(root->left);
        delete_table(root->right);
        free(root->identifier);
        free(root->parmeters);
        free(root);
    }
}

scopestack_ptr stack = NULL;

/* Stack for scopes */
void scope_push()
{
    scopestack_ptr news = malloc(sizeof(scopestack));
    news->symbol_table = calloc(1, sizeof(table_entry*));
    news->next = stack;
    stack = news;
}

void scope_pop()
{
    scopestack_ptr temp;

    if (stack == NULL)
        return;

    temp = stack;
    stack = temp->next;

    delete_table(*(temp->symbol_table));
    free(temp->symbol_table);
    free(temp);
}

void error(const str message)
{
    printf("\n%s\n", message);
    exit(EXIT_FAILURE);
}

int add_symbol(node* n, NodeType type)
{
    return add_entry(stack->symbol_table, n, type);
}

table_entry* find_symbol(const str identifier)
{
    scopestack_ptr current = stack;

    while (current)
    {
        table_entry* symbol = get_entry(*stack->symbol_table, identifier);
        if (symbol)
            return symbol;
        current = current->next;
    }
    return NULL;
}

Type evaltype(node* expr)
{
    switch (expr->nodetype)
    {    
        case IDENTIFIER_N:
        {
            table_entry* entry = find_symbol(expr->data);
            if (entry)
            {
                return entry->type;
            }
            else
            {
                error("ERROR WITH TYPE!");
            }
            break;
        }
    }
}

void analyzer(node* n)
{
    scope_push();

    if (!n ||  n->nodetype != CODE_N)
        error("AST Error!");
    
    for (int i = 0; i < n->nchildren; i++)
        process_node(n->children[i]);
}

void process_node(node* n)
{
    int i;

    if (!n)
        return;
    
    if (n->nodetype >= EXPR_N)
        error("process node does not deal with expressions");

    switch (n->nodetype)
    {
        case FUNC_N: /* How to check if returns are legit? */
        case PROC_N:
        {
            /* Add identifier to global scope */
            if (add_symbol(n->children[CALLABLE_ID], IS_FUNC(n) ? n->children[FUNC_TYPE]->nodetype : tVOID_N) == 0)
            {
                error("ID exists already in scope");
            }
            /* Add new scope */
            scope_push();
            /* Add parameters to new scope */
            process_node(n->children[IS_FUNC(n) ? FUNC_PARAMS : PROC_PARAMS]);
            /* Process function block */
            process_node(n->children[IS_FUNC(n) ? FUNC_BLOCK : PROC_BLOCK]);
            /* Need to test if FUNC had a return in the end... */
            /* Pop procedure scope */
            scope_pop();
            break;
        }
        case VAR_N:
        case PARAMLIST_N:
        case BLOCK_N:
        case DECLARATIONS_N:
        case INNERBLOCK_N:
        {
            for (i = 0; i < n->nchildren; i++)
            {
                process_node(n->children[i]);
            }
            break;
        }
        case VARLIST_N:
        {
            NodeType nodetype = n->children[n->nchildren - 1]->nodetype;
            for (i = 0; i < n->nchildren - 1; i++)
            {
                if (add_symbol(n->children[i], nodetype) == 0)
                    error("ID exists already in scope");
            }
            break;
        }
        case IF_N:
        case IFELSE_N:
        case WHILE_N:
        {
            Type type = evaltype(n->children[0]);
            if (type != tBOOL)
            {
                error("Expression given is not bolean");
            }
            process_node(n->children[1]);
            if (n->nodetype == IFELSE_N)
            {
                process_node(n->children[2]);
            }
            break;
        }
        case ASSIGNMENT_N:
        {
            Type exprtype;
            table_entry* id = find_symbol(n->children[0]->data);
            if(id == NULL)
            {
                error("identifier not exsit");
            }
            exprtype = evaltype(n->children[1]);
            if (!(exprtype == id->type || (ISPTR(id->type) && exprtype == tNULLPTR)))
            {
                error("type mismatch");
            }
            break;
        }
        default:
            error("AST ERROR!");
            break;
    }
}