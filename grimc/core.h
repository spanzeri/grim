//
// Core header that contains all the basic definitions and utilities.
//

#ifndef GRIMC_CORE_H
#define GRIMC_CORE_H

#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <threads.h>

#define MAX(a, b)   ((a) > (b) ? (a) : (b))
#define MIN(a, b)   ((a) < (b) ? (a) : (b))

#define STATIC_ASSERT(cond, ...) _Static_assert((cond), #cond)

#ifdef _MSC_VER
    #define DEBUGBREAK() __debugbreak()
#elif defined(__GNUC__) || defined(__clang__)
    #if defined(__i386__) || defined(__x86_64__) \
        || defined(__amd64__) || defined(__x86_64)
        #define DEBUGBREAK() __asm__ volatile("int $3")
    #elif defined(__aarch64__)
        #define DEBUGBREAK() __asm__ volatile("brk #0")
    #elif defined(__arm__)
        #define DEBUGBREAK() __asm__ volatile("bkpt #0")
    #else
        #define DEBUGBREAK() __builtin_trap()
    #endif
#else
    #define DEBUGBREAK() (*(volatile int *)0 = 0xDEADC0DE)
#endif

#define __ASSERT_FAIL(cond_str, ...)                                                    \
    fprintf(stderr, "%s:%d: Assertion '%s' failed.\n", __FILE__, __LINE__, cond_str);   \
    __VA_OPT__(                                                                         \
        fputs(" > ", stderr);                                                           \
        fprintf(stderr, __VA_ARGS__);                                                   \
        fputc('\n', stderr);                                                            \
    )                                                                                   \
    DEBUGBREAK()

#ifdef NDEBUG
    #define ASSERT(cond, ...)   ((void)0)
    #define ASSERT_ALWAYS(...)  ((void)0)
#else

    #define ASSERT(cond, ...)                       \
        do {                                        \
            if (!(cond)) {                          \
                __ASSERT_FAIL(#cond, __VA_ARGS__);  \
            }                                       \
        } while (0)

    #define ASSERT_ALWAYS(...)                      \
        do {                                        \
            __ASSERT_FAIL("", __VA_ARGS__);         \
        } while (0)
#endif

typedef uint8_t             u8;
typedef uint16_t            u16;
typedef uint32_t            u32;
typedef unsigned long long  u64;
typedef int8_t              i8;
typedef int16_t             i16;
typedef int32_t             i32;
typedef long long           i64;
typedef size_t              usize;
typedef ptrdiff_t           isize;

STATIC_ASSERT(sizeof(u8)  == 1);
STATIC_ASSERT(sizeof(u16) == 2);
STATIC_ASSERT(sizeof(u32) == 4);
STATIC_ASSERT(sizeof(u64) == 8);
STATIC_ASSERT(sizeof(i8)  == 1);
STATIC_ASSERT(sizeof(i16) == 2);
STATIC_ASSERT(sizeof(i32) == 4);
STATIC_ASSERT(sizeof(i64) == 8);
STATIC_ASSERT(sizeof(usize) == sizeof(void *));
STATIC_ASSERT(sizeof(isize) == sizeof(void *));

static const u8     U8_MAX  = 0xFF;
static const u16    U16_MAX = 0xFFFF;
static const u32    U32_MAX = 0xFFFFFFFF;
static const u64    U64_MAX = 0xFFFFFFFFFFFFFFFFULL;
static const i8     I8_MIN  = (-128);
static const i8     I8_MAX  = 127;
static const i16    I16_MIN = (-32768);
static const i16    I16_MAX = 32767;
static const i32    I32_MIN = (-2147483647 - 1);
static const i32    I32_MAX = 2147483647;
static const i64    I64_MIN = (-9223372036854775807LL - 1);
static const i64    I64_MAX = 9223372036854775807LL;

#define COUNTOF(x) (sizeof(x) / sizeof(0[x]))

#define UNUSED(x) ((void)(x))

#define PRINTF_LIKE(fmt_idx, first_arg) __attribute__((format(printf, fmt_idx, first_arg)))

[[noreturn]] PRINTF_LIKE(3, 4)
static void fatal__impl(const char *file, int line, const char *fmt, ...);

#define fatal(...)  fatal__impl(__FILE__, __LINE__, __VA_ARGS__)

//
// Memory allocation
//

void *xmalloc(usize size);
void *xrealloc(void *ptr, usize size);

//
// Dynamic Array
//

typedef struct DArray_Header {
    usize   len;
    usize   cap;
} DArray_Header;

#define darray__header(da)          (((DArray_Header *)(da)) - 1)
#define darray__can_fit(da, n)      (darray_len(da) + (n) <= darray_cap(da))
#define darray__fit(da, n)          (darray__can_fit(da, n) ? 0 : ((da) = darray__grow((da), darray_len(da), sizeof(*da)), 0))

#define darray_len(da)              ((da) ? darray__header(da)->len : 0)
#define darray_cap(da)              ((da) ? darray__header(da)->cap : 0)
#define darray_add(da, ...)         (darray__fit((da), 1), (da)[darray_len(da)] = (__VA_ARGS__), darray__header(da)->len++)
#define darray_free(da)             ((da) ? (free(darray__header(da)), (da) = NULL) : 0)
#define darray_back(da)             ((da)[darray_len(da) - 1])
#define darray_pop(da)              (darray_len(da) > 0 ? darray__header(da)->len-- : 0)
#define darray_clear(da)            ((da) ? darray__header(da)->len = 0 : 0)
#define darray_reserve(da, n)       (darray__fit((da), n))
#define darray_begin(da)            ((da))
#define darray_end(da)              ((da) + darray_len(da))
#define darray_remove_swap(da, idx)                             \
    do {                                                        \
        ASSERT((idx) < darray_len(da));                         \
        (da)[idx] = darray_back(da);                            \
        darray_pop(da);                                         \
    } while (0)

void *darray__grow(void *da, usize len, usize elem_size);

#endif // GRIMC_CORE_H
