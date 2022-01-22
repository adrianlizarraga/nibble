#include "queue.h"

void queue_init(Queue* queue, Allocator* arena)
{
    queue->first = queue->last = NULL;
    queue->arena = arena;
}

void queue_push(Queue* queue, void* data)
{
    QueueItem* item = alloc_type(queue->arena, QueueItem, false);
    item->data = data;
    item->next = NULL;

    if (!queue->first) {
        queue->first = item;
    }
    else {
        queue->last->next = item;
    }

    item->prev = queue->last;
    queue->last = item;
}

bool queue_is_empty(Queue* queue)
{
    bool is_empty = !queue->first;

    assert(!is_empty || (is_empty && !queue->first));

    return is_empty;
}

void* queue_pop(Queue* queue)
{
    QueueItem* item = queue->first;

    queue->first = item->next;

    if (queue->first) {
        queue->first->prev = NULL;
    }
    else {
        queue->last = NULL;
    }

    return item->data;
}

