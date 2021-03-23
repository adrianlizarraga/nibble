#ifndef NIBBLE_LLIST_H
#define NIBBLE_LLIST_H
// Linked-list implementation inspired by the Linux Kernel.
// Can be embedded in any struct type.

typedef struct DLLNode DLLNode;

struct DLLNode {
    DLLNode* prev;
    DLLNode* next;
};

#define dll_head_create(name) { &(name), &(name) }
#define dll_entry(ptr, type, member) container_of(ptr, type, member)
#define container_of(ptr, type, member) ((type*) ((char*)ptr - offsetof(type, member)))

static inline void dll_head_init(DLLNode* list)
{
    list->prev = list;
    list->next = list;
}

static inline void dll_add(DLLNode* new_node, DLLNode* prev_node)
{
    DLLNode* next_node = prev_node->next;

    next_node->prev = new_node;
    new_node->next = next_node;
    new_node->prev = prev_node;
    prev_node->next = new_node;
}

#endif
