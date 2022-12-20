#ifndef NIBBLE_LLIST_H
#define NIBBLE_LLIST_H
#include <stddef.h>
#include <stdbool.h>

// Intrusive linked-list implementation inspired by the Linux Kernel.
// Can be embedded in any struct type.

typedef struct List List;
typedef List ListNode;

struct List {
    List* prev;
    List* next;
};

#define list_head_create(name) \
    {                          \
        &(name), &(name)       \
    }
#define list_entry(ptr, type, member) container_of(ptr, type, member)
#define container_of(ptr, type, member) ((type*)((char*)ptr - offsetof(type, member)))

static inline void list_head_init(List* list)
{
    list->prev = list;
    list->next = list;
}

static inline void list_add(ListNode* prev_node, ListNode* new_node)
{
    List* next_node = prev_node->next;

    next_node->prev = new_node;
    new_node->next = next_node;
    new_node->prev = prev_node;
    prev_node->next = new_node;
}

static inline void list_add_last(List* list, ListNode* node)
{
    list_add(list->prev, node);
}

static inline void list_replace(List* old_node, List* new_node)
{
    new_node->next = old_node->next;
    new_node->next->prev = new_node;
    new_node->prev = old_node->prev;
    new_node->prev->next = new_node;
}

static inline bool list_empty(List* list)
{
    return (list->next == list);
}

static inline void list_rm(ListNode* node)
{
    ListNode* prev = node->prev;
    ListNode* next = node->next;

    next->prev = prev;
    prev->next = next;

    node->next = node->prev = NULL;
}

#endif
