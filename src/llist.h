#ifndef NIBBLE_LLIST_H
#define NIBBLE_LLIST_H
// Intrusive linked-list implementation inspired by the Linux Kernel.
// Can be embedded in any struct type.

typedef struct DLList DLList;

struct DLList {
    DLList* prev;
    DLList* next;
};

#define dllist_head_create(name) { &(name), &(name) }
#define dllist_entry(ptr, type, member) container_of(ptr, type, member)
#define container_of(ptr, type, member) ((type*) ((char*)ptr - offsetof(type, member)))

static inline void dllist_head_init(DLList* list)
{
    list->prev = list;
    list->next = list;
}

static inline void dllist_add(DLList* prev_node, DLList* new_node)
{
    DLList* next_node = prev_node->next;

    next_node->prev = new_node;
    new_node->next = next_node;
    new_node->prev = prev_node;
    prev_node->next = new_node;
}

static inline void dllist_replace(DLList* old_node, DLList* new_node)
{
    new_node->next = old_node->next;
    new_node->next->prev = new_node;
    new_node->prev = old_node->prev;
    new_node->prev->next = new_node;
}

#endif
