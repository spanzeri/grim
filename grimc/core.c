#include "core.h"

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

void *darray__grow(void *da, usize len, usize elem_size) {
    usize curr_cap = darray_cap(da);
    usize next_cap = curr_cap + (curr_cap >> 1);
    next_cap = MAX(next_cap, 16);       // At least 16 elements
    next_cap = MAX(next_cap, len);      // Make sure we can fit 'len' elements

    ASSERT(next_cap > curr_cap);
    ASSERT(next_cap >= len);

    usize new_size = sizeof(DArray_Header) + elem_size * next_cap;

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
