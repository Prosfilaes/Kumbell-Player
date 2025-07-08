#include "mmap_c.h"
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#define HEADER_SIZE 51
#define RECORD_SIZE 17

struct Mmap_File
{
    int fd;
    void *map;
    size_t size;
    size_t length;
} mmap_file = {.fd = 0, .map = NULL, .size = 0, .length = 0};

int mmap_open(const char *filename)
{
    int fd = open(filename, O_RDONLY);
    if (fd == -1)
        return -1;

    struct stat st;
    if (fstat(fd, &st) == -1)
    {
        close(fd);
        return -2;
    }

    if (st.st_size < HEADER_SIZE)
    {
        close(fd);
        return -3;
    }

    void *map = mmap(NULL, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
    if (map == MAP_FAILED)
    {
        close(fd);
        return -4;
    }

    mmap_file.fd = fd;
    mmap_file.map = map;
    mmap_file.size = st.st_size;
    mmap_file.length = (st.st_size - HEADER_SIZE) / RECORD_SIZE;

    return 0;
}

void mmap_close(void)
{
    if (!mmap_file.map)
        return;
    munmap(mmap_file.map, mmap_file.size);
    close(mmap_file.fd);
}

size_t mmap_length(void)
{
    return mmap_file.length;
}

Mmap_Record mmap_get(size_t index)
{
    Mmap_Record null_record = {.start_idx = 0, .end_idx = 0, .value = 0};
    if (index >= mmap_file.length)
        return null_record;
    uint8_t *base = (uint8_t *)mmap_file.map + HEADER_SIZE;
    return *(const Mmap_Record *)(base + index * RECORD_SIZE);
}
