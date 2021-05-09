#include <stddef.h>
#include <stdint.h>

#include "allocator.c"
#include "array.c"
#include "cstring.c"
#include "hash_map.c"
#include "llist.h"
#include "print.c"

void test_allocator(void)
{

    // Test allocator creation, non-expanding allocation, and allocator destruction.
    {
        Allocator allocator = allocator_create(512);
        assert((allocator.end - allocator.buffer) == 512);

        void* m = mem_allocate(&allocator, 256, 1, true);
        assert(m);
        assert((allocator.at - allocator.buffer) == 256);

        unsigned char* old_buffer = allocator.buffer;
        m = mem_allocate(&allocator, 1024, 1, false);
        assert(m);
        assert(old_buffer != allocator.buffer);
        assert((allocator.at - allocator.buffer) == 1024);
        assert((allocator.end - allocator.buffer) == ((1024 + 1) * 2));

        allocator_destroy(&allocator);
        assert(!allocator.buffer);
        assert(!allocator.at);
        assert(!allocator.end);
    }

    // Test expansion, aligned allocation, and resetting the allocator after expansion.
    {
        Allocator allocator = allocator_create(512);

        unsigned char* old_buffer = allocator.buffer;
        void* m = mem_allocate(&allocator, 1024, DEFAULT_ALIGN, false);
        assert(m);
        assert(((uintptr_t)m & (DEFAULT_ALIGN - 1)) == 0);
        assert(old_buffer != allocator.buffer);
        assert((allocator.end - allocator.buffer) >= 1024);

        allocator_reset(&allocator);
        assert(allocator.buffer);
        assert(allocator.at == allocator.buffer);
        assert(allocator.end > allocator.buffer);

        allocator_destroy(&allocator);
        assert(!allocator.buffer);
        assert(!allocator.at);
        assert(!allocator.end);
    }

    // Test freeing the previous allocation
    {
        Allocator allocator = allocator_create(512);
        assert(allocator.pat == NULL);

        // Alloc 16 bytes
        void* m1 = mem_allocate(&allocator, 16, 1, true);
        assert(m1);
        assert((uintptr_t)allocator.pat == (uintptr_t)m1);
        assert((allocator.at - allocator.buffer) == 16);

        // Alloc 10 more bytes
        void* m2 = mem_allocate(&allocator, 10, 1, true);
        unsigned char* saved_at = allocator.at;
        assert(m2);
        assert(m1 != m2);
        assert((uintptr_t)allocator.pat == (uintptr_t)m2);
        assert((allocator.at - allocator.buffer) == 26);

        // Try to free the first allocation. Should be a no-op because the last allocation is m2.
        mem_free(&allocator, m1);
        assert(saved_at == allocator.at);
        assert((uintptr_t)allocator.pat == (uintptr_t)m2);

        // Free the second allocation. Should succeed.
        mem_free(&allocator, m2);
        assert(allocator.pat == NULL);
        assert((uintptr_t)allocator.at == (uintptr_t)m2);

        // Try to free the second allocation again. Should be a no-op.
        mem_free(&allocator, m2);
        assert(allocator.pat == NULL);
        assert((uintptr_t)allocator.at == (uintptr_t)m2);

        allocator_destroy(&allocator);
        assert(!allocator.buffer);
        assert(!allocator.at);
        assert(!allocator.end);
    }

    // Test allocator state restoration after non-expanding allocation.
    {
        Allocator allocator = allocator_create(512);

        void* m = mem_allocate(&allocator, 16, 1, false);
        assert(m);
        assert((allocator.at - allocator.buffer) == 16);

        AllocatorState state = allocator_get_state(&allocator);
        {
            m = mem_allocate(&allocator, 64, 1, false);
            assert(m);
            assert((allocator.at - allocator.buffer) == 64 + 16);
        }
        allocator_restore_state(state);

        assert((allocator.at - allocator.buffer) == 16);

        allocator_destroy(&allocator);
        assert(!allocator.buffer);
        assert(!allocator.at);
        assert(!allocator.end);
    }

    // Test allocator state restoration after expanding allocation.
    {
        Allocator allocator = allocator_create(512);

        void* m = mem_allocate(&allocator, 16, 1, false);
        assert(m);
        assert((allocator.at - allocator.buffer) == 16);

        unsigned char* old_buffer = allocator.buffer;
        AllocatorState state = allocator_get_state(&allocator);
        {
            m = mem_allocate(&allocator, 2048, 1, false);
            assert(m);
            assert(old_buffer != allocator.buffer);
            assert((allocator.at - allocator.buffer) == 2048);
        }
        allocator_restore_state(state);

        assert(old_buffer == allocator.buffer);
        assert((allocator.at - allocator.buffer) == 16);

        allocator_destroy(&allocator);
        assert(!allocator.buffer);
        assert(!allocator.at);
        assert(!allocator.end);
    }
}

void test_array(void)
{
    Allocator allocator = allocator_create(1024);

    // Test array create and len/cap tracking.
    {
        int* a = array_create(&allocator, int, 128);
        assert(array_len(a) == 0);
        assert(array_cap(a) == 128);

        for (int i = 0; i < 20; i++)
        {
            array_push(a, i);
            assert(array_back(a) == i);
            assert(array_len(a) == (size_t)i + 1);
            assert(array_cap(a) == 128);
        }
    }

    // Test array reallocation.
    {
        int* a = array_create(&allocator, int, 16);
        int* old_a = a;

        for (int i = 0; i < 256; i++)
        {
            array_push(a, i);
            assert(array_back(a) == i);
            assert(array_len(a) == (size_t)i + 1);
            assert(array_cap(a) >= array_len(a));
        }
        assert(old_a != a);
        assert(array_cap(a) == array_len(a));

        array_push(a, 257);
        assert(array_cap(a) > array_len(a));
    }

    // Test array clearing
    {
        int* a = array_create(&allocator, int, 16);
        array_push(a, 10);
        assert(array_len(a) > 0);

        array_clear(a);
        assert(array_len(a) == 0);
        assert(array_cap(a) > 0);
    }

    // Test insertion
    {
        int* a = array_create(&allocator, int, 8);
        for (int i = 0; i < 4; i++)
        {
            array_push(a, i);
        }
        assert(array_len(a) == 4);

        array_insert(a, 1, 100);
        assert(array_len(a) == 5);
        assert(a[0] == 0);
        assert(a[1] == 100);
        assert(a[2] == 1);
        assert(a[3] == 2);
        assert(a[4] == 3);
    }

    // Test array pop
    {
        int* a = array_create(&allocator, int, 8);

        array_push(a, 333);

        size_t old_len = array_len(a);
        int geo = array_pop(a);

        assert(array_len(a) == old_len - 1);
        assert(geo == 333);
    }

    // Test array remove.
    {
        int* a = array_create(&allocator, int, 10);
        for (int i = 0; i < 8; i++)
        {
            array_push(a, i);
        }
        assert(array_len(a) == 8);

        int rindex = 3;
        array_remove(a, rindex);
        assert(array_len(a) == 7);

        for (int i = 0; i < 7; i++)
        {
            if (i < rindex)
            {
                assert(a[i] == i);
            }
            else
            {
                assert(a[i] == i + 1);
            }
        }
    }

    // Test array remove (swap last).
    {
        int* a = array_create(&allocator, int, 10);
        for (int i = 0; i < 8; i++)
        {
            array_push(a, i);
        }
        assert(array_len(a) == 8);

        int rindex = 3;
        array_remove_swap(a, rindex);
        assert(array_len(a) == 7);
        assert(a[rindex] == 7);
    }

    // Test array push elems
    {
        int b[3] = {1, 2, 3};
        int* a = NULL;

        array_push(a, 0);
        assert(array_len(a) == 1);

        array_push_elems(a, b, 3);
        assert(array_len(a) == 4);

        for (int i = 0; i < 3; ++i)
        {
            assert(a[i + 1] == b[i]);
        }

        array_free(a);
    }

    allocator_destroy(&allocator);
}

void test_hmap(void)
{
    HMap map = hmap(21, NULL);

    for (uint64_t i = 1; i <= (1 << 20); ++i)
    {
        uint64_t* r = hmap_put(&map, i, i);

        assert(r);
        assert(*r == i);
    }

    for (uint64_t i = 1; i <= (1 << 20); ++i)
    {
        uint64_t* r = hmap_get(&map, i);

        assert(r);
        assert(*r == i);
    }

    ftprint_out("cap = %lu, len = %lu\n", map.cap, map.len);

    hmap_destroy(&map);
}

void test_interning(void)
{
    Allocator arena = allocator_create(4096);
    HMap strmap = hmap(8, NULL);

    const char* a = "hello";
    const char* b = "hello!";
    const char* a_in = intern_str(&arena, &strmap, a, strlen(a));
    const char* b_in = intern_str(&arena, &strmap, b, strlen(b));

    assert(a != a_in);
    assert(b != b_in);
    assert(a_in == intern_str(&arena, &strmap, a, strlen(a)));
    assert(b_in == intern_str(&arena, &strmap, b, strlen(b)));
    assert(strmap.len == 2);
    assert(a_in != b_in);

    hmap_destroy(&strmap);
    allocator_destroy(&arena);
}

typedef struct TestNode {
    int num;
    DLList node;
} TestNode;

void test_dllist(void)
{
    // Test adding new values to the head of the list.
    {
        DLList head = dllist_head_create(head);

        TestNode node1 = {.num = 1};
        TestNode node2 = {.num = 2};
        TestNode node3 = {.num = 3};

        dllist_add(&head, &node1.node);
        dllist_add(&head, &node2.node);
        dllist_add(&head, &node3.node);

        int i = 0;
        for (DLList* n = head.next; n != &head; n = n->next)
        {
            TestNode* entry = dllist_entry(n, TestNode, node);
            assert(entry->num == 3 - i);

            i += 1;
        }
    }

    // Test adding new values to the tail of the list.
    {
        DLList head = dllist_head_create(head);

        TestNode node1 = {.num = 1};
        TestNode node2 = {.num = 2};
        TestNode node3 = {.num = 3};

        dllist_add(head.prev, &node1.node);
        dllist_add(head.prev, &node2.node);
        dllist_add(head.prev, &node3.node);

        int i = 0;
        for (DLList* n = head.next; n != &head; n = n->next)
        {
            TestNode* entry = dllist_entry(n, TestNode, node);
            assert(entry->num == i + 1);

            i += 1;
        }
    }
}

int main(void)
{
    ftprint_out("Nibble utilities tests!\n");

    test_allocator();
    test_array();
    test_hmap();
    test_interning();
    test_dllist();
}
