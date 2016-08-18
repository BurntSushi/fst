#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "fst.h"

#ifndef DEBUG
  #define DEBUG false
#endif

bool test_fst_raw_builder_memory() {
    bool passed = true;
    const char *key1 = "bar";
    const char *key2 = "foo";

    fst_raw_builder_memory *builder = fst_raw_builder_memory_new();
    passed = passed && fst_raw_builder_memory_add(
        builder, (const uint8_t *) "bar", strlen(key1));
    passed = passed && fst_raw_builder_memory_add(
        builder, (const uint8_t *) "foo", strlen(key2));
    passed = passed && fst_raw_builder_memory_finish(builder);

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

    run_test(test_fst_raw_builder_memory, "test_fst_raw_builder_memory",
             &passed);

    if (!passed) {
        exit(1);
    }
    return 0;
}
