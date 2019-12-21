#include <limits.h>

int parseLong(char *str, long *val)
{
    char *temp;
    int rc = 1;
    errno = 0;
    *val = strtol(str, &temp, 0);

    if (temp == str || *temp != '\0' ||
        ((*val == LONG_MIN || *val == LONG_MAX) && errno == ERANGE))
        rc = 0;

    return rc;
}
