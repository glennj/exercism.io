#include <stdlib.h>
#include <assert.h>
#include "linked_list.h"
#include <stdio.h>

// First, the structures
struct list_node {
   struct list_node *prev, *next;
   ll_data_t data;
};

struct list {
   struct list_node *head, *tail;
   size_t count;
};

// ------------------------------------------------------------
// Allocations

struct list *list_create(void) {
    struct list *list = malloc(sizeof *list);
    if (list != NULL) {
        list->head = NULL;
        list->tail = NULL;
        list->count = 0;
    }
    return list;
}

static struct list_node *
list_node_create(
        struct list_node *prev,
        struct list_node *next,
        ll_data_t data
) {
    struct list_node *node = malloc(sizeof *node);
    if (node != NULL) {
        node->prev = prev;
        node->next = next;
        node->data = data;
    }
    return node;
}

// Destroy an entire list
// First, free all the nodes; then, free the list
void list_destroy(struct list *list) {
    assert(list != NULL);
    while (list->count)
        list_shift(list);
    free(list);
}

// ------------------------------------------------------------
size_t list_count(const struct list *list) {
    assert(list != NULL);
    return list->count;
}

// ------------------------------------------------------------
// push, unshift: adding nodes

static struct list_node *
list_add_node(
        struct list *list,
        struct list_node *prev,
        struct list_node *next,
        ll_data_t data
) {
    struct list_node *node = list_node_create(prev, next, data);
    assert(node != NULL);

    if (prev == NULL)
        list->head = node;
    else
        prev->next = node;

    if (next == NULL)
        list->tail = node;
    else
        next->prev = node;

    list->count += 1;
    return node;
}

void list_push(struct list *list, ll_data_t item_data) {
    assert(list != NULL);
    list_add_node(list, list->tail, NULL, item_data);
}

void list_unshift(struct list *list, ll_data_t item_data) {
    assert(list != NULL);
    list_add_node(list, NULL, list->head, item_data);
}

// ------------------------------------------------------------
// pop, shift, delete: removing nodes

static ll_data_t
list_remove_node(
        struct list *list,
        struct list_node *node
) {
    if (node->prev == NULL)
        list->head = node->next;
    else
        node->prev->next = node->next;

    if (node->next == NULL)
        list->tail = node->prev;
    else
        node->next->prev = node->prev;

    ll_data_t value = node->data;
    free(node);
    list->count -= 1;
    return value;
}

ll_data_t list_shift(struct list *list) {
    assert(list != NULL);
    assert(list->head != NULL);
    return list_remove_node(list, list->head);
}

ll_data_t list_pop(struct list *list) {
    assert(list != NULL);
    assert(list->tail != NULL);
    return list_remove_node(list, list->tail);
}

void list_delete(struct list *list, ll_data_t data) {
    assert(list != NULL);
    struct list_node *node = list->head;
    while (node != NULL) {
        if (node->data == data) {
            list_remove_node(list, node);
            return;
        }
        node = node->next;
    }
}
