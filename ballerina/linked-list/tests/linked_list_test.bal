import ballerina/test;

@test:Config {}
function testPopGetsElementFromList() {
    LinkedList list = newLinkedList();
    push(list, 7);
    test:assertEquals(pop(list), 7);
}

@test:Config {
    //enable: false
}
function testPushPopRespectivelyAddRemoveAtEndOfList() {
    LinkedList list = newLinkedList();
    push(list, 11);
    push(list, 13);
    test:assertEquals(pop(list), 13);
    test:assertEquals(pop(list), 11);
}

@test:Config {
    //enable: false
}
function testShiftGetsElementFromList() {
    LinkedList list = newLinkedList();
    push(list, 17);
    test:assertEquals(shift(list), 17);
}

@test:Config {
    //enable: false
}
function testShiftGetsFirstElementFromList() {
    LinkedList list = newLinkedList();
    push(list, 23);
    push(list, 5);
    test:assertEquals(shift(list), 23);
    test:assertEquals(shift(list), 5);
}

@test:Config {
    //enable: false
}
function testUnshiftAddsElementAtStartOfList() {
    LinkedList list = newLinkedList();
    unshift(list, 23);
    unshift(list, 5);
    test:assertEquals(shift(list), 5);
    test:assertEquals(shift(list), 23);
}

@test:Config {
    //enable: false
}
function testPopPushShiftUnshiftCanBeUsedInAnyOrder() {
    LinkedList list = newLinkedList();
    push(list, 1);
    push(list, 2);
    test:assertEquals(pop(list), 2);
    push(list, 3);
    test:assertEquals(shift(list), 1);
    unshift(list, 4);
    push(list, 5);
    test:assertEquals(shift(list), 4);
    test:assertEquals(pop(list), 5);
    test:assertEquals(shift(list), 3);
}

@test:Config {
    //enable: false
}
function testCountAnEmptyList() {
    LinkedList list = newLinkedList();
    test:assertEquals(count(list), 0);
}

@test:Config {
    //enable: false
}
function testCountAListWithItems() {
    LinkedList list = newLinkedList();
    push(list, 37);
    push(list, 1);
    test:assertEquals(count(list), 2);
}

@test:Config {
    //enable: false
}
function testCountIsCorrectAfterMutation() {
    LinkedList list = newLinkedList();
    push(list, 31);
    test:assertEquals(count(list), 1);
    unshift(list, 43);
    test:assertEquals(count(list), 2);
    _ = shift(list);
    test:assertEquals(count(list), 1);
    _ = pop(list);
    test:assertEquals(count(list), 0);
}

@test:Config {
    //enable: false
}
function testPoppingToEmptyDoesNotBreakTheList() {
    LinkedList list = newLinkedList();
    push(list, 41);
    push(list, 59);
    _ = pop(list);
    _ = pop(list);
    push(list, 47);
    test:assertEquals(count(list), 1);
    test:assertEquals(pop(list), 47);
}

@test:Config {
    //enable: false
}
function testShiftingToEmptyDoesNotBreakTheList() {
    LinkedList list = newLinkedList();
    push(list, 41);
    push(list, 59);
    _ = shift(list);
    _ = shift(list);
    push(list, 47);
    test:assertEquals(count(list), 1);
    test:assertEquals(shift(list), 47);
}

@test:Config {
    //enable: false
}
function testDeletesTheOnlyElement() {
    LinkedList list = newLinkedList();
    push(list, 61);
    delete(list, 61);
    test:assertEquals(count(list), 0);
}

@test:Config {
    //enable: false
}
function testDeletesTheElementWithTheGivenValue() {
    LinkedList list = newLinkedList();
    push(list, 71);
    push(list, 83);
    push(list, 79);
    delete(list, 83);
    test:assertEquals(count(list), 2);
    test:assertEquals(pop(list), 79);
    test:assertEquals(shift(list), 71);
}

@test:Config {
    //enable: false
}
function testDeletesTheElementWithTheGivenValueReassigningTail() {
    LinkedList list = newLinkedList();
    push(list, 71);
    push(list, 83);
    push(list, 79);
    delete(list, 83);
    test:assertEquals(count(list), 2);
    test:assertEquals(pop(list), 79);
    test:assertEquals(pop(list), 71);
}

@test:Config {
    //enable: false
}
function testDeletesTheElementWithTheGivenValueReassigningHead() {
    LinkedList list = newLinkedList();
    push(list, 71);
    push(list, 83);
    push(list, 79);
    delete(list, 83);
    test:assertEquals(count(list), 2);
    test:assertEquals(shift(list), 71);
    test:assertEquals(shift(list), 79);
}

@test:Config {
    //enable: false
}
function testDeletesTheFirstOfTwoElements() {
    LinkedList list = newLinkedList();
    push(list, 97);
    push(list, 101);
    delete(list, 97);
    test:assertEquals(count(list), 1);
    test:assertEquals(pop(list), 101);
}

@test:Config {
    //enable: false
}
function testDeletesTheSecondOfTwoElements() {
    LinkedList list = newLinkedList();
    push(list, 97);
    push(list, 101);
    delete(list, 101);
    test:assertEquals(count(list), 1);
    test:assertEquals(pop(list), 97);
}

@test:Config {
    //enable: false
}
function testDoesNotModifyListIfElementNotFound() {
    LinkedList list = newLinkedList();
    push(list, 89);
    delete(list, 103);
    test:assertEquals(count(list), 1);
}

@test:Config {
    //enable: false
}
function testDeleteOnlyTheFirstOccurrence() {
    LinkedList list = newLinkedList();
    push(list, 73);
    push(list, 9);
    push(list, 9);
    push(list, 107);
    delete(list, 9);
    test:assertEquals(count(list), 3);
    test:assertEquals(pop(list), 107);
    test:assertEquals(pop(list), 9);
    test:assertEquals(pop(list), 73);
}
