#include "binary_search.h"

const int *binary_search(int value, const int *arr, size_t length) {
    if (!arr) return NULL;

    for (int i = 0, j = length - 1; i <= j; ) {
        int mid = (i + j) / 2;
        if (value == arr[mid])
            return arr + mid;
        if (value < arr[mid])
            j = mid - 1;
        else
            i = mid + 1;
    }
    return NULL;
}
