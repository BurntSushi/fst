#ifndef _FST_H
#define _FST_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct fst_raw_builder_memory fst_raw_builder_memory;

fst_raw_builder_memory *fst_raw_builder_memory_new();

bool fst_raw_builder_memory_add(fst_raw_builder_memory *builder,
                                const uint8_t *key, size_t key_len);

bool fst_raw_builder_memory_finish(fst_raw_builder_memory *builder);

#ifdef __cplusplus
}
#endif

#endif
