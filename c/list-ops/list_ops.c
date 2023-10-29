#include "list_ops.h"
#include <stddef.h>
#include <string.h>
#include <assert.h>

static list_t *create_list(size_t length) {
    list_t *list = malloc(sizeof *list + length * sizeof *list->elements);
    assert(list != NULL);
    list->length = length;
    // list elements are unititialized
    return list;
}

list_t *new_list(size_t length, list_element_t elements[]) {
    list_t *list = create_list(length);

    // initialize the elements
    /*
    list_element_t *p = list->elements;
    for (size_t i = 0; i < length; i++) 
        *p++ = *elements++;
    */
    memcpy(list->elements, elements, length * sizeof *elements);

    return list;
}

size_t length_list(list_t *list) {
    assert(list != NULL);
    return list->length;
}

list_t *append_list(list_t *first, list_t *second) {
    assert(first != NULL && second != NULL);
    list_t *new = create_list(first->length + second->length);

    /*
    list_element_t *p = new->elements;
    list_element_t *q = first->elements;
    for (size_t i = 0; i < first->length; i++) 
        *p++ = *q++;

    q = second->elements;
    for (size_t i = 0; i < second->length; i++) 
        *p++ = *q++;
    */
    memcpy( new->elements,
            first->elements,
            first->length * sizeof *first->elements
    );
    memcpy( new->elements + first->length,
            second->elements,
            second->length * sizeof *second->elements
    );

    return new;
}

list_t *filter_list(list_t *input, bool (*filter)(list_element_t)) {
    assert(input != NULL);
    list_t *filtered = create_list(input->length);

    list_element_t *p = filtered->elements;
    size_t count = 0;
    for (size_t i = 0; i < input->length; i++) {
        if (filter(input->elements[i])) {
            *p++ = input->elements[i];
            count++;
        }
    }

    // shrink the new list to the appropriate size
    filtered = realloc(
            filtered,
            sizeof *filtered + count * sizeof *filtered->elements
    );
    assert (filtered != NULL);

    filtered->length = count;
    return filtered;
}

list_t *map_list(list_t *input, list_element_t (*map)(list_element_t)) {
    assert(input != NULL);
    list_t *mapped = create_list(input->length);

    list_element_t *p = mapped->elements;
    for (size_t i = 0; i < input->length; i++)
        *p++ = map(input->elements[i]);

    return mapped;
}

list_element_t foldl_list(
        list_t *input,
        list_element_t accumulator,
        list_element_t (*foldl)(list_element_t, list_element_t)
) {
    assert(input != NULL);
    for (size_t i = 0; i < input->length; i++)
        accumulator = foldl(accumulator, input->elements[i]);
    return accumulator;
}

list_element_t foldr_list(
        list_t *input,
        list_element_t accumulator,
        list_element_t (*foldr)(list_element_t, list_element_t)
) {
    assert(input != NULL);
    for (size_t i = 0; i < input->length; i++)
        accumulator = foldr(input->elements[input->length - i - 1], accumulator);
    return accumulator;
}

list_t *reverse_list(list_t *input) {
    assert(input != NULL);
    list_t *reversed = create_list(input->length);

    list_element_t *p = reversed->elements;
    list_element_t *q = input->elements + input->length - 1;

    for (size_t i = 0; i < input->length; i++)
        *p++ = *q--;

    return reversed;
}

void delete_list(list_t *list) {
    free(list);
}
