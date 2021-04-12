#include "nibble.h"
#include "allocator.h"
#include "cstring.h"
#include "hash_map.h"

typedef struct NibbleCtx {
    HashMap ident_map;
    HashMap str_lit_map;
    Allocator allocator;
} NibbleCtx;

typedef struct InternedIdent {
    struct InternedIdent* next;
    size_t len;
    Keyword kw;
    bool is_kw;
    char str[];
} InternedIdent;

const char* keywords[KW_COUNT];

static StringView keyword_names[KW_COUNT] = {
    [KW_VAR] = string_view_lit("var"),
    [KW_CONST] = string_view_lit("const"),
    [KW_ENUM] = string_view_lit("enum"),
    [KW_UNION] = string_view_lit("union"),
    [KW_STRUCT] = string_view_lit("struct"),
    [KW_PROC] = string_view_lit("proc"),
    [KW_TYPEDEF] = string_view_lit("typedef"),
    [KW_SIZEOF] = string_view_lit("sizeof"),
    [KW_TYPEOF] = string_view_lit("typeof"),
    [KW_LABEL] = string_view_lit("label"),
    [KW_GOTO] = string_view_lit("goto"),
    [KW_BREAK] = string_view_lit("break"),
    [KW_CONTINUE] = string_view_lit("continue"),
    [KW_RETURN] = string_view_lit("return"),
    [KW_IF] = string_view_lit("if"),
    [KW_ELSE] = string_view_lit("else"),
    [KW_ELIF] = string_view_lit("elif"),
    [KW_WHILE] = string_view_lit("while"),
    [KW_DO] = string_view_lit("do"),
    [KW_FOR] = string_view_lit("for"),
    [KW_SWITCH] = string_view_lit("switch"),
    [KW_CASE] = string_view_lit("case"),
    [KW_UNDERSCORE] = string_view_lit("_"),
};

static NibbleCtx nibble;

bool nibble_init(void)
{
    nibble.allocator = allocator_create(4096);
    nibble.str_lit_map = hash_map(6, NULL);
    nibble.ident_map = hash_map(6, NULL);

    // Compute the total amount of memory needed to store all interned keywords.
    // Why? Program needs all keywords to reside in a contigous block of memory to facilitate
    // determining whether a string is a keyword using simple pointer comparisons.
    size_t kws_size = 0;
    for (int i = 0; i < KW_COUNT; ++i) {
        size_t size = offsetof(InternedIdent, str) + keyword_names[i].len + 1; 

        kws_size += ALIGN_UP(size, DEFAULT_ALIGN);
    }

    char* kws_mem = mem_allocate(&nibble.allocator, kws_size, DEFAULT_ALIGN, false);
    if (!kws_mem) {
        return false;
    }

    char* kws_mem_ptr = kws_mem;

    for (int i = 0; i < KW_COUNT; ++i) {
        const char* str = keyword_names[i].str;
        size_t len = keyword_names[i].len;
        size_t size = offsetof(InternedIdent, str) + len + 1;
        InternedIdent* kw = (void*)kws_mem_ptr;

        kw->next = NULL;
        kw->len = len;
        kw->is_kw = true;
        kw->kw = (Keyword)i;

        memcpy(kw->str, str, len);
        kw->str[len] = '\0';

        hash_map_put(&nibble.ident_map, hash_bytes(str, len), (uintptr_t)kw);
        keywords[i] = kw->str;

        kws_mem_ptr += ALIGN_UP(size, DEFAULT_ALIGN);
    }
    assert((size_t)(kws_mem_ptr - kws_mem) == kws_size);
    assert(nibble.ident_map.len == KW_COUNT);

    return true;
}

void nibble_cleanup(void)
{
#ifndef NDEBUG
    print_allocator_stats(&nibble.allocator, "Nibble mem stats");
    ftprint_out("Ident map: len = %lu, cap = %lu, total_size (malloc) = %lu\n",
                nibble.ident_map.len, nibble.ident_map.cap,
                nibble.ident_map.cap * sizeof(HashMapEntry));
    ftprint_out("StrLit map: len = %lu, cap = %lu, total_size (malloc) = %lu\n",
                nibble.str_lit_map.len, nibble.str_lit_map.cap,
                nibble.str_lit_map.cap * sizeof(HashMapEntry));
#endif

    hash_map_destroy(&nibble.str_lit_map);
    hash_map_destroy(&nibble.ident_map);
    allocator_destroy(&nibble.allocator);
}

const char* intern_str_lit(const char* str, size_t len)
{
    Allocator* allocator = &nibble.allocator;
    HashMap* strmap = &nibble.str_lit_map;

    const char* interned_str = intern_str(allocator, strmap, str, len);

    if (!interned_str) {
        // TODO: Handle in a better way.
        ftprint_err("[INTERNAL ERROR]: Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        exit(1);
    }

    return interned_str;
}

// TODO: Reuse duplicated interning code!
const char* intern_ident(const char* str, size_t len, bool* is_kw, Keyword* kw)
{
    Allocator* allocator = &nibble.allocator;
    HashMap* strmap = &nibble.ident_map;
    uint64_t key = hash_bytes(str, len);
    uint64_t* pval = hash_map_get(strmap, key);
    InternedIdent* intern = pval ? (void*)*pval : NULL;

    // Collisions will only occur if identical hash values are produced. Collisions due to
    // contention for hash map slots will not occur (open addressing).
    //
    // Walk the linked list in case of collision.
    for (InternedIdent* it = intern; it; it = it->next) {
        if ((it->len == len) && (cstr_ncmp(it->str, str, len) == 0)) {
            if (is_kw) {
                *is_kw = it->is_kw;
                if (kw) { *kw = it->kw; }
            }
            return it->str;
        }
    }

    // If we got here, need to add this string to the intern table.
    InternedIdent* new_intern = mem_allocate(allocator, offsetof(InternedIdent, str) + len + 1, DEFAULT_ALIGN, false);
    if (new_intern) {
        new_intern->next = intern; // Record collision. If a collision did _NOT_ occur, this will be null.
        new_intern->len = len;
        new_intern->is_kw = false;
        new_intern->kw = (Keyword)0; // TODO: Have an invalid or none keyword type. Just clean this up in some way!

        memcpy(new_intern->str, str, len);
        new_intern->str[len] = '\0';

        hash_map_put(strmap, key, (uintptr_t)new_intern);
    } else {
        // TODO: Handle in a better way.
        ftprint_err("[INTERNAL ERROR]: Out of memory.\n%s:%d\n", __FILE__, __LINE__);
        exit(1);
    }

    if (is_kw) {
        *is_kw = new_intern->is_kw;
        if (kw) { *kw = new_intern->kw; }
    }

    return new_intern->str;
}
