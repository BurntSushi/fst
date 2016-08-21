#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "fst.h"

#ifndef DEBUG
  #define DEBUG false
#endif

#define B(x) ((const uint8_t *)(x))

size_t bytelen(const uint8_t *bytes) {
    return (size_t) strlen((const char *) bytes);
}

bool test_fst_builder_memory() {
    bool passed = true;
    const uint8_t *key1 = B("bar");
    const uint8_t *key2 = B("baz");
    const uint8_t *key3 = B("foo");
    fst_builder_memory *builder;
    fst *fst;

    builder = fst_builder_memory_new();
    passed = passed && fst_builder_memory_add(builder, key1, bytelen(key1));
    passed = passed && fst_builder_memory_add(builder, key2, bytelen(key2));
    passed = passed && fst_builder_memory_add(builder, key3, bytelen(key3));
    fst = fst_builder_memory_finish(builder, NULL);
    passed = passed && fst_contains_key(fst, key1, bytelen(key1));
    passed = passed && fst_contains_key(fst, key2, bytelen(key2));
    passed = passed && fst_contains_key(fst, key3, bytelen(key3));

    fst_free(fst);
    return passed;
}

bool test_fst_simple_accessors() {
    bool passed = true;
    const uint8_t *key1 = B("bar");
    const uint8_t *key2 = B("baz");
    const uint8_t *key3 = B("foo");
    uint64_t val = 0;
    fst_builder_memory *builder;
    fst *fst;

    builder = fst_builder_memory_new();
    passed = passed && fst_builder_memory_add(builder, key1, bytelen(key1));
    passed = passed && fst_builder_memory_add(builder, key2, bytelen(key2));
    passed = passed && fst_builder_memory_add(builder, key3, bytelen(key3));
    fst = fst_builder_memory_finish(builder, NULL);
    passed = passed && fst_contains_key(fst, key1, bytelen(key1));
    passed = passed && fst_contains_key(fst, key2, bytelen(key2));
    passed = passed && fst_contains_key(fst, key3, bytelen(key3));

    passed = passed && fst_get(fst, key2, bytelen(key2), &val);
    if (val != 0) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_simple_accessors] key retrieval expected 0 "
                "but got %lu\n", val);
        }
        passed = false;
    }

    size_t len = fst_len(fst);
    if (len != 3) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_simple_accessors] fst_len == %zu, expected %d\n",
                len, 3);
        }
        passed = false;
    }

    size_t size = fst_size(fst);
    if (size != 49) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_simple_accessors] fst_size == %zu, expected %d\n",
                size, 49);
        }
        passed = false;
    }

    fst_free(fst);
    return passed;
}

void run_test(bool (test)(), const char *name, bool *passed) {
    if (!test()) {
        *passed = false;
        fprintf(stderr, "FAILED: %s\n", name);
    } else {
        fprintf(stderr, "PASSED: %s\n", name);
    }
}

int main() {
    bool passed = true;

    run_test(test_fst_builder_memory, "test_fst_builder_memory",
             &passed);
    run_test(test_fst_simple_accessors, "test_fst_simple_accessors",
             &passed);

    if (!passed) {
        exit(1);
    }
    return 0;
}
