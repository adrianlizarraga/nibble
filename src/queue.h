#include "allocator.h"

typedef struct QueueItem {
    void* data;
    struct QueueItem* next;
    struct QueueItem* prev;
} QueueItem;

typedef struct Queue {
    QueueItem* first;
    QueueItem* last;
    Allocator* arena;
} Queue;

void queue_init(Queue* queue, Allocator* arena);
void queue_push(Queue* queue, void* data);
bool queue_is_empty(Queue* queue);
void* queue_pop(Queue* queue);
