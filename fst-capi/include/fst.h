#ifndef _FST_H
#define _FST_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct fst fst;

typedef struct fst_stream fst_stream;

typedef struct fst_builder_memory fst_builder_memory;

typedef struct fst_error fst_error;

fst_builder_memory *fst_builder_memory_new();

bool fst_builder_memory_add(fst_builder_memory *builder,
                             const uint8_t *key, size_t key_len);

fst *fst_builder_memory_finish(fst_builder_memory *builder,
                               fst_error *error);

bool fst_get(fst *fst, const uint8_t *key, size_t key_len, uint64_t *value);

bool fst_contains_key(fst *fst, const uint8_t *key, size_t key_len);

fst_stream *fst_stream_new(fst *fst);

bool fst_stream_next(fst_stream *stream, const uint8_t **key, size_t *key_len,
                     uint64_t *value);

void fst_stream_free(fst_stream *stream);

size_t fst_len(fst *fst);

size_t fst_size(fst *fst);

void fst_free(fst *Fst);

/*
 * fst_error_new allocates space for an error.
 *
 * If error information is desired, then fst_error_new should be called
 * to create an fst_error pointer, and that pointer can be passed to
 * any function that can return an error.
 *
 * It is not safe to use errors from multiple threads simultaneously. An error
 * value may be reused.
 */
fst_error *fst_error_new();

/*
 * fst_error_free frees the error given.
 *
 * This must be called at most once.
 */
void fst_error_free(fst_error *err);

/*
 * fst_error_message returns a NUL terminated string that describes the error
 * message.
 *
 * The pointer returned must not be freed. Instead, it will be freed when
 * fst_error_free is called. The pointer returned here may change or become
 * invalid if err is passed to other functions.
 */
const char *fst_error_message(fst_error *err);

#ifdef __cplusplus
}
#endif

#endif
