#include <stdlib.h>
#include <assert.h>
#include "linked_list.h"

// First, the structures
struct list_node {
   struct list_node *prev, *next;
   ll_data_t data;
};

struct list {
   struct list_node *head, *tail;
   size_t count;
};

// Allocations
static void initialize_list(struct list *list) {
    list->head = NULL;
    list->tail = NULL;
    list->count = 0;
}

struct list *list_create(void) {
    struct list *list = (struct list *) malloc(sizeof(struct list));
    initialize_list(list);
    return list;
}

static struct list_node *list_node_create(ll_data_t data) {
    struct list_node *node = (struct list_node *) malloc(sizeof(struct list_node));
    node->prev = NULL;
    node->next = NULL;
    node->data = data;
    return node;
}

// List manipulation functions
size_t list_count(const struct list *list) {
    return list->count;
}

void list_push(struct list *list, ll_data_t item_data) {
    struct list_node *node = list_node_create(item_data);
    if (list->tail == NULL) {
        list->head = node;
    }
    else {
        list->tail->next = node;
        node->prev = list->tail;
    }
    list->tail = node;
    list->count += 1;
}

ll_data_t list_pop(struct list *list) {
    assert(list->count > 0);

    struct list_node *node = list->tail;
    ll_data_t value = node->data;
    if (node->prev == NULL) {
        initialize_list(list);
    }
    else {
        list->tail = node->prev;
        list->count -= 1;
    }
    free(node);
    return value;
}

void list_unshift(struct list *list, ll_data_t item_data) {
    struct list_node *node = list_node_create(item_data);
    if (list->head == NULL) {
        list->tail = node;
    }
    else {
        list->head->prev = node;
        node->next = list->head;
    }
    list->head = node;
    list->count += 1;
}

ll_data_t list_shift(struct list *list) {
    assert(list->count > 0);

    struct list_node *node = list->head;
    ll_data_t value = node->data;
    if (node->next == NULL) {
        initialize_list(list);
    }
    else {
        list->head = node->next;
        list->count -= 1;
    }
    free(node);
    return value;
}

void list_delete(struct list *list, ll_data_t data) {
    struct list_node *node = list->head;
    while (node != NULL) {
        if (node->data != data) {
            node = node->next;
            continue;
        }

        if (node->next != NULL && node->prev != NULL) {
            node->prev->next = node->next;
            node->next->prev = node->prev;
        }
        else if (node->next != NULL) {
            node->next->prev = NULL;
            list->head = node->next;
        }
        else if (node->prev != NULL) {
            node->prev->next = NULL;
            list->tail = node->prev;
        }
        list->count -= 1;
        free(node);
        return;
    }
}

// Destroy an entire list
void list_destroy(struct list *list) {
    // first, free all the nodes
    while (list->count)
        list_shift(list);
    // then, free the list
    free(list);
}

/*
This destroy func gives me the error
```none
free(): double free detected in tcache 2
```
```c
void list_destroy(struct list *list) {
    struct list_node *node = list->head;
    while (node != NULL) {
		struct list_node *next = node->next;
        free(node);
        node = next;
    }
    free(list);
}
```
*/
