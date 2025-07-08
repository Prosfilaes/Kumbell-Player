#ifndef MMAP_H
#define MMAP_H

#include <stdint.h>
#include <stddef.h>

typedef struct
{
    int64_t start_idx;
    int64_t end_idx;
    uint8_t value;
} Mmap_Record;

#ifdef __cplusplus
extern "C"
{
#endif

    int mmap_open(const char *filename);
    void mmap_close(void);
    size_t mmap_length(void);
    Mmap_Record mmap_get(size_t index);

#ifdef __cplusplus
}
#endif

#endif // MMAP_H
