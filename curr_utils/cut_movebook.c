#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define MAX_LINE_LEN 256

int main(void) {
    char line[MAX_LINE_LEN];

    while (fgets(line, sizeof(line), stdin)) {
        char *p = line;

        // Skip leading whitespace
        while (isspace(*p)) p++;

        // First integer
        char *start = p;
        while (isdigit(*p)) p++;
        while (isspace(*p)) p++;

        // Second integer
        while (isdigit(*p)) p++;

        // Null-terminate after second number
        *p = '\0';

        // Output trimmed line
        puts(start);
    }

    return EXIT_SUCCESS;
}
