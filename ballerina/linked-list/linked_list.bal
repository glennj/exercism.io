public type LinkedList record {
    Node? head = ();
    Node? tail = ();
    int length = 0;
};

type Node record {
    int value;
    Node? next = ();
    Node? prev = ();
};

public function newLinkedList() returns LinkedList => {};

# Adds a new value to the tail of a linked list
#
# + list - the linked list
# + value - integer value to add
public function push(LinkedList list, int value) {
    Node node = {value};
    Node? tail = list.tail;
    if tail == () {
        list.head = node;
    } else {
        tail.next = node;
        node.prev = tail;
    }
    list.tail = node;
    list.length += 1;
}

# Adds a new value to the head of a linked list
#
# + list - the linked list
# + value - integer value to add
public function unshift(LinkedList list, int value) {
    Node node = {value};
    Node? head = list.head;
    if head == () {
        list.tail = node;
    } else {
        head.prev = node;
        node.next = head;
    }
    list.head = node;
    list.length += 1;
}

# Remove a value from the tail of a linked list
#
# + list - the linked list
# + return - the value, or nil if the list is empty
public function pop(LinkedList list) returns int? {
    Node? node = list.tail;
    if node == () {
        return ();
    }
    if node.prev == () {
        list.head = ();
    }
    list.tail = node.prev;
    list.length -= 1;
    node.prev = ();
    return node.value;
}

# Remove a value from the head of a linked list
#
# + list - the linked list
# + return - the value, or nil if the list is empty
public function shift(LinkedList list) returns int? {
    Node? node = list.head;
    if node == () {
        return ();
    }
    if node.next == () {
        list.tail = ();
    }
    list.head = node.next;
    list.length -= 1;
    node.next = ();
    return node.value;
}

# Count the number of values in a list
#
# + list - the linked list
# + return - the number of values, or zero for an empty list
public function count(LinkedList list) returns int {
    return list.length;
}

# Delete the given value from the linked list
#
# + list - the linked list
# + value - integer value to delete
public function delete(LinkedList list, int value) {
    Node? node = list.head;
    while node != () {
        if node.value != value {
            node = node.next;
            continue;
        }

        Node? prev = node.prev;
        Node? next = node.next;
        if prev == () { // delete the head
            _ = shift(list);
        } else if next == () { // delete the tail
            _ = pop(list);
        } else {
            prev.next = next;
            next.prev = prev;
            node.next = ();
            node.prev = ();
            list.length -= 1;
        }
        break;
    }
}
