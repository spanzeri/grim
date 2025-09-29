#include "common.h"

[[noreturn]] PRINTF_LIKE(3, 4)
void fatal__impl(const char *file, int line, const char *fmt, ...) {
    fprintf(stderr, "%s:%d: Fatal error: ", file, line);
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);
    DEBUGBREAK();
    exit(1);
}

void *xmalloc(usize size) {
    void *ptr = malloc(size);
    if (!ptr) {
        fatal("Out of memory (malloc(%zu) failed)", size);
    }
    return ptr;
}

void *xrealloc(void *ptr, usize size) {
    void *new_ptr = realloc(ptr, size);
    if (!new_ptr) {
        fatal("Out of memory (realloc(%p, %zu) failed)", ptr, size);
    }
    return new_ptr;
}

static usize align_up(usize ptr, usize alignment) {
    usize mask = alignment - 1;
    return (ptr + mask) & ~mask;
}

static u8* align_ptr_up(void* ptr, usize alignment) {
    usize mask = alignment - 1;
    return (u8*)((usize)((u8*)ptr + mask) & ~mask);
}

void arena_grow(Arena* arena, usize min_size) {
    usize block_size = align_up(MAX(ARENA_BLOCK_SIZE, min_size), 4096);
    arena->ptr = xmalloc(block_size);
    arena->end = arena->ptr + block_size;
    darray_add(arena->blocks, arena->ptr);
}

void* arena_alloc(Arena* arena, usize size) {
    u8* ptr = align_ptr_up(arena->ptr, ARENA_ALIGNMENT);
    if (ptr + size > arena->end) {
        arena_grow(arena, size + ARENA_ALIGNMENT);
        ptr = align_ptr_up(arena->ptr, ARENA_ALIGNMENT);
        ASSERT(ptr + size <= arena->end);
    }
    arena->ptr = ptr + size;
    return ptr;
}

void arena_reset(Arena* arena) {
    for (int i = 0; i < darray_len(arena->blocks); i++) {
        free(arena->blocks[i]);
    }
    darray_free(arena->blocks);
    arena->ptr    = NULL;
    arena->end    = NULL;
    arena->blocks = NULL;
}

void *darray__grow(void *da, int len, usize elem_size) {
    int curr_cap = darray_cap(da);
    int next_cap = curr_cap + (curr_cap >> 1);
    next_cap = MAX(next_cap, 16);       // At least 16 elements
    next_cap = MAX(next_cap, len);      // Make sure we can fit 'len' elements

    ASSERT(next_cap > curr_cap);
    ASSERT(next_cap >= len);

    usize new_size = sizeof(DArray_Header) + elem_size * (size_t)next_cap;

    DArray_Header *new_hdr = NULL;
    if (da) {
        new_hdr = xrealloc(darray__header(da), new_size);
    } else {
        new_hdr = xmalloc(sizeof(DArray_Header) + new_size);
        new_hdr->len = 0;
    }

    new_hdr->cap = next_cap;

    return (new_hdr + 1);
}

static void darray_test(int argc, const char *argv[]) {
    const char **arr = NULL;
    ASSERT(darray_len(arr) == 0, "Initial arr len = %d, expected 0", darray_len(arr));
    darray_add(arr, "Hello");
    darray_add(arr, "World");
    for (int i = 0; i < argc; i++) {
        darray_add(arr, argv[i]);
    }
    for (int i = 0; i < darray_len(arr); i++) {
        printf("Array arr[%d] = %s\n", i, arr[i]);
    }

    const char *extra[] = {
        "This", "is", "a", "test", "of", "dynamic", "arrays",
        "This", "is", "a", "test", "of", "dynamic", "arrays",
        "This", "is", "a", "test", "of", "dynamic", "arrays",
        "This", "is", "a", "test", "of", "dynamic", "arrays",
    };
    for (usize i = 0; i < COUNTOF(extra); i++) {
        darray_add(arr, extra[i]);
    }

    darray_free(arr);
    ASSERT(darray_len(arr) == 0, "After free arr len = %d, expected 0", darray_len(arr));
    ASSERT(arr == NULL, "After free arr = %p, expected NULL", (void*)arr);

    enum { ARR_COUNT = 1024 };
    int* int_arr = NULL;
    for (int i = 0; i < ARR_COUNT; i++) {
        darray_add(int_arr, i * 2);
    }
    ASSERT(darray_len(int_arr) == ARR_COUNT, "int_arr len = %d, expected %d", darray_len(int_arr), ARR_COUNT);
    for (int i = 0; i < ARR_COUNT; i++) {
        ASSERT(int_arr[i] == i * 2, "int_arr[%d] = %d, expected %d", i, int_arr[i], i * 2);
    }

    printf("Array length = %d, cap = %d\n", darray_len(arr), darray_cap(arr));

    darray_free(int_arr);
    ASSERT(darray_len(int_arr) == 0, "After free int_arr len = %d, expected 0", darray_len(int_arr));
    ASSERT(int_arr == NULL, "After free int_arr = %p, expected NULL", (void*)int_arr);
}

//
// String interning
//

// @TODO: This is not thread-safe
static Intern_String *interns;

const char *str_intern_range(const char *str, const char *end) {
    usize len = (usize)(end - str);
    for (Intern_String *it = interns; it < darray_end(interns); it++) {
        if (it->len == len && strncmp(it->str, str, len) == 0) {
            return it->str;
        }
    }

    char *new_str = xmalloc(len + 1);
    memcpy(new_str, str, len);
    new_str[len] = '\0';

    darray_add(interns, (Intern_String){ .len = len, .str = new_str });
    return new_str;
}

const char *str_intern(const char *str) {
    return str_intern_range(str, str + strlen(str));
}

static void str_intern_test(void) {
    const char *s1 = str_intern("Hello, World!");
    const char *s2 = str_intern("Hello, World!");
    const char *s3 = str_intern("Goodbye, World!");
    ASSERT(s1 == s2);
    ASSERT(s1 != s3);
    const char hello[] = "Hello";
    const char *s4 = str_intern_range(hello, hello + COUNTOF(hello) - 1);
    const char *s5 = str_intern("Hello");
    ASSERT(s4 == s5);
    const char hello2[] = "Hello!";
    const char *s6 = str_intern_range(hello2, hello2 + COUNTOF(hello2) - 1);
    ASSERT(s4 != s6);
}

//
// Error reporting
//

bool break_on_syntax_error = false;

PRINTF_LIKE(1, 2)
void syntax_error(const char *fmt, ...)  {
    fprintf(stderr, "Syntax error: ");
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);
    if (break_on_syntax_error) {
        DEBUGBREAK();
    }
}

//
// Test
//

TEST(common) {
    const char *args[] = { "arg1", "arg2", "arg3", "arg4", "arg5" };
    darray_test(COUNTOF(args), args);
    str_intern_test();
}
