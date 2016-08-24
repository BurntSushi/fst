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

size_t bytelen(const uint8_t *bytes)
{
    return (size_t) strlen((const char *) bytes);
}

fst *fst_create3(const char *k1, uint64_t v1,
                 const char *k2, uint64_t v2,
                 const char *k3, uint64_t v3)
{
    fst_builder_memory *builder = fst_builder_memory_new();
    if (NULL != k1) {
        if (!fst_builder_memory_add(builder, B(k1), strlen(k1), v1, NULL)) {
            if (DEBUG) {
                fprintf(stderr, "[fst_create3] error adding: %s\n", k1);
            }
            return NULL;
        }
    }
    if (NULL != k2) {
        if (!fst_builder_memory_add(builder, B(k2), strlen(k2), v2, NULL)) {
            if (DEBUG) {
                fprintf(stderr, "[fst_create3] error adding: %s\n", k2);
            }
            return NULL;
        }
    }
    if (NULL != k3) {
        if (!fst_builder_memory_add(builder, B(k3), strlen(k3), v3, NULL)) {
            if (DEBUG) {
                fprintf(stderr, "[fst_create3] error adding: %s\n", k3);
            }
            return NULL;
        }
    }
    fst *fst = fst_builder_memory_finish(builder, NULL);
    if (DEBUG && NULL == fst) {
        fprintf(stderr, "[fst_create3] error finishing build\n");
    }
    return fst;
}

bool test_fst_builder_memory() {
    bool passed = true;
    fst *fst = fst_create3("bar", 0, "baz", 0, "foo", 0);
    if (NULL == fst) {
        return false;
    }

    passed = passed && fst_contains_key(fst, B("bar"), 3);
    passed = passed && fst_contains_key(fst, B("baz"), 3);
    passed = passed && fst_contains_key(fst, B("foo"), 3);

    fst_free(fst);
    return passed;
}

bool test_fst_new_from_bytes() {
    const char *fst_bytes = "\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x72\x10\x02\xc5\x00\x10\x84\xc4\x01\x05\x66\x62\x10\x02\x03\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00";
    size_t fst_bytes_len = 49;
    bool passed = true;
    fst *fst = fst_new_from_bytes(B(fst_bytes), fst_bytes_len, NULL);
    if (NULL == fst) {
        return false;
    }

    passed = passed && fst_contains_key(fst, B("bar"), 3);
    passed = passed && fst_contains_key(fst, B("baz"), 3);
    passed = passed && fst_contains_key(fst, B("foo"), 3);

    fst_free(fst);
    return passed;
}

bool test_fst_simple_accessors() {
    bool passed = true;
    uint64_t val = 0;

    fst *fst = fst_create3("bar", 0, "baz", 9, "foo", 0);
    if (NULL == fst) {
        return false;
    }

    passed = passed && fst_contains_key(fst, B("bar"), 3);
    passed = passed && fst_contains_key(fst, B("baz"), 3);
    passed = passed && fst_contains_key(fst, B("foo"), 3);
    passed = passed && fst_get(fst, B("baz"), 3, &val);
    if (val != 9) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_simple_accessors] key retrieval expected 9 "
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
    if (size != 51) {
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

bool test_fst_stream() {
    bool passed = true;
    const uint8_t *key;
    size_t key_len;
    uint64_t value;

    fst *fst = fst_create3("bar", 0, "baz", 1, "foo", 2);
    fst_stream *stream = fst_stream_new(fst);

    if (!fst_stream_next(stream, &key, &key_len, &value)) {
        if (DEBUG) {
            fprintf(stderr, "[test_fst_stream] expected at least one item\n");
        }
        passed = false;
    } else {
        if (0 != memcmp((const void *) key, (const void *) "bar", 3)) {
            if (DEBUG) {
                fprintf(stderr, "[test_fst_stream] expected 'bar'\n");
            }
            passed = false;
        }
        if (0 != value) {
            if (DEBUG) {
                fprintf(stderr, "[test_fst_stream] expected value '0'\n");
            }
            passed = false;
        }
    }
    if (!fst_stream_next(stream, &key, &key_len, &value)) {
        if (DEBUG) {
            fprintf(stderr, "[test_fst_stream] expected at least two items\n");
        }
        passed = false;
    } else {
        if (0 != memcmp((const void *) key, (const void *) "baz", 3)) {
            if (DEBUG) {
                fprintf(stderr, "[test_fst_stream] expected 'baz'\n");
            }
            passed = false;
        }
        if (1 != value) {
            if (DEBUG) {
                fprintf(stderr, "[test_fst_stream] expected value '1'\n");
            }
            passed = false;
        }
    }
    if (!fst_stream_next(stream, &key, &key_len, &value)) {
        if (DEBUG) {
            fprintf(
                stderr, "[test_fst_stream] expected at least three items\n");
        }
        passed = false;
    } else {
        if (0 != memcmp((const void *) key, (const void *) "foo", 3)) {
            if (DEBUG) {
                fprintf(stderr, "[test_fst_stream] expected 'foo'\n");
            }
            passed = false;
        }
        if (2 != value) {
            if (DEBUG) {
                fprintf(stderr, "[test_fst_stream] expected value '2'\n");
            }
            passed = false;
        }
    }

    fst_stream_free(stream);
    fst_free(fst);
    return passed;
}

bool test_fst_is_disjoint() {
    bool passed = true;

    fst *fst1 = fst_create3("bar", 0, "baz", 0, "foo", 0);
    fst *fst2 = fst_create3("aaa", 0, "baz", 0, "zzz", 0);
    fst *fst3 = fst_create3("aaa", 0, "bbb", 0, "zzz", 0);

    if (fst_is_disjoint(fst1, fst_stream_new(fst2))) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_is_disjoint] fst1 is not disjoint with fst2\n");
        }
        passed = false;
    }
    if (!fst_is_disjoint(fst1, fst_stream_new(fst3))) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_is_disjoint] fst1 is disjoint with fst3\n");
        }
        passed = false;
    }

    fst_free(fst1);
    fst_free(fst2);
    fst_free(fst3);
    return passed;
}

bool test_fst_is_subset() {
    bool passed = true;

    fst *fst1 = fst_create3("bar", 0, "baz", 0, "foo", 0);
    fst *fst2 = fst_create3("baz", 0, NULL, 0, NULL, 0);
    fst *fst3 = fst_create3("foobar", 0, NULL, 0, NULL, 0);

    if (!fst_is_subset(fst2, fst_stream_new(fst1))) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_is_subset] fst2 is a subset of fst1\n");
        }
        passed = false;
    }
    if (fst_is_subset(fst3, fst_stream_new(fst1))) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_is_subset] fst3 is not a subset of fst1\n");
        }
        passed = false;
    }

    fst_free(fst1);
    fst_free(fst2);
    fst_free(fst3);
    return passed;
}

bool test_fst_is_superset() {
    bool passed = true;

    fst *fst1 = fst_create3("bar", 0, "baz", 0, "foo", 0);
    fst *fst2 = fst_create3("baz", 0, NULL, 0, NULL, 0);
    fst *fst3 = fst_create3("foobar", 0, NULL, 0, NULL, 0);

    if (!fst_is_superset(fst1, fst_stream_new(fst2))) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_is_superset] fst1 is a superset of fst2\n");
        }
        passed = false;
    }
    if (fst_is_superset(fst1, fst_stream_new(fst3))) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_is_superset] fst1 is not a superset of fst3\n");
        }
        passed = false;
    }

    fst_free(fst1);
    fst_free(fst2);
    fst_free(fst3);
    return passed;
}

bool test_fst_automaton_regex() {
    bool passed = true;
    const uint8_t *key;
    size_t key_len;
    uint64_t value;

    fst_automaton *re = fst_automaton_regex_new_nul(".*oo", NULL);
    if (NULL == re) {
        if (DEBUG) {
            fprintf(stderr, "[test_fst_automaton_regex] regex failure\n");
        }
        return false;
    }

    fst *fst = fst_create3("bar", 0, "baz", 1, "foo", 2);
    fst_stream_builder *sb = fst_stream_builder_new(fst);
    fst_stream_builder_automaton(sb, re);
    fst_stream *stream = fst_stream_builder_finish(sb);

    if (!fst_stream_next(stream, &key, &key_len, &value)) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_automaton_regex] expected at least one item\n");
        }
        passed = false;
    } else {
        if (0 != memcmp((const void *) key, (const void *) "foo", 3)) {
            if (DEBUG) {
                fprintf(stderr, "[test_fst_automaton_regex] expected 'foo'\n");
            }
            passed = false;
        }
        if (2 != value) {
            if (DEBUG) {
                fprintf(
                    stderr, "[test_fst_automaton_regex] expected value '2'\n");
            }
            passed = false;
        }
    }
    if (fst_stream_next(stream, &key, &key_len, &value)) {
        if (DEBUG) {
            fprintf(
                stderr,
                "[test_fst_automaton_regex] "
                "expected stream with one element\n");
        }
        passed = false;
    }

    fst_stream_free(stream);
    /* fst_automaton_free(re); */
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
    run_test(test_fst_new_from_bytes, "test_fst_new_from_bytes",
             &passed);
    run_test(test_fst_simple_accessors, "test_fst_simple_accessors",
             &passed);
    run_test(test_fst_stream, "test_fst_stream", &passed);
    run_test(test_fst_is_disjoint, "test_fst_is_disjoint", &passed);
    run_test(test_fst_is_subset, "test_fst_is_subset", &passed);
    run_test(test_fst_is_superset, "test_fst_is_superset", &passed);
    run_test(test_fst_automaton_regex, "test_fst_automaton_regex", &passed);

    if (!passed) {
        exit(1);
    }
    return 0;
}
