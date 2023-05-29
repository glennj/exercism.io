import ballerina/http;
import ballerina/test;

http:Client cl = check new (string `http://localhost:${port}`);

@test:Config
function testMenu() returns error? {
    json menu = check cl->get("/menu");
    test:assertEquals(menu, {"Butter Cake": 15, "Chocolate Cake": 20, "Tres Leches": 25});
}

@test:Config
function testMenuWithInvalidPath() returns error? {
    http:Response res = check cl->get("/menu/all");
    test:assertEquals(res.statusCode, http:STATUS_NOT_FOUND);
}

@test:Config
function testMenuWithInvalidPath2() returns error? {
    http:Response res = check cl->get("/menu/menu");
    test:assertEquals(res.statusCode, http:STATUS_NOT_FOUND);
}

string? orderId = ();

@test:Config
function testOrderPlacementSuccessStatusCode() returns error? {
    http:Response res = check cl->post("/order", {
        "username": "mary_jane",
        "order_items": [
            {"item": "Tres Leches", "quantity": 3}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_CREATED);
}

@test:Config
function testOrderPlacementSuccess() returns error? {
    record {|
        string order_id;
        int total;
    |} res = check cl->post("/order", {
        "username": "mary",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1},
            {"item": "Chocolate Cake", "quantity": 2}
        ]
    });
    orderId = res.order_id;
    test:assertEquals(res.total, 65);
}

string? orderId2 = ();

@test:Config
function testOrderPlacementSuccess2() returns error? {
    record {|
        string order_id;
        int total;
    |} res = check cl->post("/order", {
        "username": "amy",
        "order_items": [
            {"item": "Butter Cake", "quantity": 1},
            {"item": "Chocolate Cake", "quantity": 1}
        ]
    });
    orderId2 = res.order_id;
    test:assertEquals(res.total, 35);
}

@test:Config
function testOrderPlacementFailureForInvalidJsonPayloadStructure() returns error? {
    http:Response res = check cl->post("/order", {
        "username": "mary",
        "order_items": [
            {"Tres Leches": 1},
            {"Chocolate Cake": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForInvalidJsonPayloadStructure2() returns error? {
    http:Response res = check cl->post("/order", {
        "usernames": "amy",
        "order_items": [
            {"item": "Butter Cake", "quantity": 1},
            {"item": "Chocolate Cake", "quantity": 1}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadOfInvalidContentType() returns error? {
    http:Response res = check cl->post("/order", "string");
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadOfInvalidContentType2() returns error? {
    http:Response res = check cl->post("/order", xml `<root/>`);
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadWithEmptyUsername() returns error? {
    http:Response res = check cl->post("/order", {
        "username": "",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1},
            {"item": "Chocolate Cake", "quantity": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadWithEmptyOrderItemsArray() returns error? {
    http:Response res = check cl->post("/order", {"username": "sumudu", "order_items": []});
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadWithEmptyOrderItemsArray2() returns error? {
    http:Response res = check cl->post("/order", {"order_items": [], "username": "sumudu"});
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadWithMoreThanOneArrayMemberForSameItem() returns error? {
    http:Response res = check cl->post("/order", {
        "username": "sumudu",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1},
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadWithMoreThanOneArrayMemberForSameItem2() returns error? {
    http:Response res = check cl->post("/order", {
        "username": "sumudu",
        "order_items": [
            {"item": "Butter Cake", "quantity": 1},
            {"item": "Tres Leches", "quantity": 2},
            {"item": "Chocolate Cake", "quantity": 2},
            {"item": "Butter Cake", "quantity": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadWithUnknownOrderItem() returns error? {
    http:Response res = check cl->post("/order", {
        "username": "sumudu",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1},
            {"item": "Coffee Cake", "quantity": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadWithUnknownOrderItem2() returns error? {
    http:Response res = check cl->post("/order", {
        "username": "sumudu",
        "order_items": [
            {"item": "Lemon Drizzle", "quantity": 1},
            {"item": "Coffee Cake", "quantity": 2},
            {"item": "Butter Cake", "quantity": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadWithInvalidOrderQuantity() returns error? {
    http:Response res = check cl->post("/order", {
        "username": "sumudu",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1},
            {"item": "Butter Cake", "quantity": 0}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testOrderPlacementFailureForPayloadWithInvalidOrderQuantity2() returns error? {
    http:Response res = check cl->post("/order", {
        "username": "sumudu",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1},
            {"item": "Chocolate Cake", "quantity": -1},
            {"item": "Butter Cake", "quantity": 0}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess]
}
function testRetrievingOrderStatusWithAValidOrderId() returns error? {
    string orderIdString = check orderId.ensureType();

    record {|
        string order_id;
        string status;
    |} orderStatusJson = check cl->get(string `/order/${orderIdString}`);

    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess2]
}
function testRetrievingOrderStatusWithAValidOrderId2() returns error? {
    string orderIdString = check orderId2.ensureType();

    record {|
        string order_id;
        string status;
    |} orderStatusJson = check cl->get(string `/order/${orderIdString}`);

    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");
}

@test:Config
function testRetrievingOrderStatusWithAnInvalidOrderId() returns error? {
    http:Response res = check cl->get(string `/order/invalid`);
    test:assertEquals(res.statusCode, http:STATUS_NOT_FOUND);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess, testOrderPlacementSuccess2]
}
function testRetrievingOrderStatusWithAnInvalidOrderId2() returns error? {
    http:Response res = check cl->get(string `/order/${check orderId.ensureType(string)}${check orderId2.ensureType(string)}`);
    test:assertEquals(res.statusCode, http:STATUS_NOT_FOUND);
}

@test:Config
function testRetrievingOrderStatusForOrderIdWithUpdatedStatus() returns error? {
    record {|
        string order_id;
        int total;
    |} res = check cl->post("/order", {
        "username": "jo",
        "order_items": [
            {"item": "Tres Leches", "quantity": 2},
            {"item": "Chocolate Cake", "quantity": 1},
            {"item": "Butter Cake", "quantity": 1}
        ]
    });

    string orderIdString = res.order_id;

    json orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");

    lock {
        orderStatus[orderIdString] = "in progress";
    }

    orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "in progress");

    lock {
        orderStatus[orderIdString] = "completed";
    }

    orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "completed");
}

@test:Config
function testRetrievingOrderStatusForOrderIdWithUpdatedStatus2() returns error? {
    record {|
        string order_id;
        int total;
    |} res = check cl->post("/order", {
        "username": "alex",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1}
        ]
    });

    string orderIdString = res.order_id;

    json orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");

    lock {
        orderStatus[orderIdString] = "in progress";
    }

    orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "in progress");

    lock {
        orderStatus[orderIdString] = "completed";
    }

    orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "completed");

    lock {
        orderStatus[orderIdString] = "pending";
    }

    orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess]
}
function testUpdatingAnOrderSuccessfully() returns error? {
    string orderIdString = check orderId.ensureType();

    record {|
        string order_id;
        int total;
    |} res = check cl->put(string `/order/${orderIdString}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 2},
            {"item": "Chocolate Cake", "quantity": 1},
            {"item": "Butter Cake", "quantity": 1}
        ]
    });

    test:assertEquals(res.order_id, orderIdString);
    test:assertEquals(res.total, 85);

    json orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess]
}
function testUpdatingAnOrderSuccessfully2() returns error? {
    string orderIdString = check orderId.ensureType();

    record {|
        string order_id;
        int total;
    |} res = check cl->put(string `/order/${orderIdString}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 1}
        ]
    });

    test:assertEquals(res.order_id, orderIdString);
    test:assertEquals(res.total, 25);

    json orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");
}

@test:Config
function testOrderUpdateFailureForInvalidOrderId() returns error? {
    int testId = 9876;

    lock {
        string id = testId.toString();

        while orderStatus.hasKey(id) {
            testId -= 1;
        }
    }

    http:Response res = check cl->put(string `/order/${testId}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 3}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_NOT_FOUND);
}

@test:Config
function testOrderUpdateFailureForNonPendingOrder() returns error? {
    record {|
        string order_id;
        int total;
    |} res = check cl->post("/order", {
        "username": "sunil",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1}
        ]
    });

    string orderIdString = res.order_id;

    json orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");

    lock {
        orderStatus[orderIdString] = "in progress";
    }

    http:Response updateRes = check cl->put(string `/order/${orderIdString}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(updateRes.statusCode, http:STATUS_FORBIDDEN);

    lock {
        orderStatus[orderIdString] = "completed";
    }

    updateRes = check cl->put(string `/order/${orderIdString}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(updateRes.statusCode, http:STATUS_FORBIDDEN);
}

@test:Config
function testOrderUpdateFailureForNonPendingOrder2() returns error? {
    record {|
        string order_id;
        int total;
    |} res = check cl->post("/order", {
        "username": "ahas",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1},
            {"item": "Butter Cake", "quantity": 1}
        ]
    });

    string orderIdString = res.order_id;

    json orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");

    lock {
        orderStatus[orderIdString] = "in progress";
    }

    http:Response updateRes = check cl->put(string `/order/${orderIdString}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(updateRes.statusCode, http:STATUS_FORBIDDEN);

    lock {
        orderStatus[orderIdString] = "completed";
    }

    updateRes = check cl->put(string `/order/${orderIdString}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(updateRes.statusCode, http:STATUS_FORBIDDEN);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess]
}
function testOrderUpdateFailureForInvalidJsonPayloadStructure() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId.ensureType(string)}`, {
        "username": "mary",
        "order_items": [
            {"Tres Leches": 1},
            {"Chocolate Cake": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess2]
}
function testOrderUpdateFailureForInvalidJsonPayloadStructure2() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId2.ensureType(string)}`, <json[]>[{"Tres Leches": 1}]);
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess]
}
function testOrderUpdateFailureForPayloadOfInvalidContentType() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId.ensureType(string)}`, "string");
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess2]
}
function testOrderUpdateFailureForPayloadOfInvalidContentType2() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId2.ensureType(string)}`,
                                    xml `<order_items><TresLeches>2</TresLeches></order_items>`);
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess]
}
function testOrderUpdateFailureForPayloadWithEmptyOrderItemsArray() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId.ensureType(string)}`, {"order_items": []});
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess2]
}
function testOrderUpdateFailureForPayloadWithEmptyOrderItemsArray2() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId2.ensureType(string)}`, {"order_items": []});
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess]
}
function testOrderUpdateFailureForPayloadWithMoreThanOneArrayMemberForSameItem() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId.ensureType(string)}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 1},
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess2]
}
function testOrderUpdateFailureForPayloadWithMoreThanOneArrayMemberForSameItem2() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId2.ensureType(string)}`, {
        "order_items": [
            {"item": "Butter Cake", "quantity": 1},
            {"item": "Butter Cake", "quantity": 1},
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess]
}
function testOrderUpdateFailureForPayloadWithUnknownOrderItem() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId.ensureType(string)}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 1},
            {"item": "Coffee Cake", "quantity": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess2]
}
function testOrderUpdateFailureForPayloadWithUnknownOrderItem2() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId2.ensureType(string)}`, {
        "order_items": [
            {"item": "Lemon Drizzle", "quantity": 1},
            {"item": "Coffee Cake", "quantity": 2}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess]
}
function testOrderUpdateFailureForPayloadWithInvalidOrderQuantity() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId.ensureType(string)}`, {
        "username": "sumudu",
        "order_items": [
            {"item": "Tres Leches", "quantity": -1}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config {
    dependsOn: [testOrderPlacementSuccess2]
}
function testOrderUpdateFailureForPayloadWithInvalidOrderQuantity2() returns error? {
    http:Response res = check cl->put(string `/order/${check orderId2.ensureType(string)}`, {
        "username": "sumudu",
        "order_items": [
            {"item": "Tres Leches", "quantity": 0}
        ]
    });
    test:assertEquals(res.statusCode, http:STATUS_BAD_REQUEST);
}

@test:Config
function testDeletingAnOrderSuccessfully() returns error? {
    record {|
        string order_id;
        int total;
    |} res = check cl->post("/order", {
        "username": "amani",
        "order_items": [
            {"item": "Butter Cake", "quantity": 2}
        ]
    });

    http:Response deleteRes = check cl->delete(string `/order/${res.order_id}`);
    test:assertEquals(deleteRes.statusCode, http:STATUS_OK);
}

@test:Config
function testDeletingAnOrderSuccessfully2() returns error? {
    record {|
        string order_id;
        int total;
    |} res = check cl->post("/order", {
        "username": "may",
        "order_items": [
            {"item": "Chocolate Cake", "quantity": 2}
        ]
    });

    http:Response deleteRes = check cl->delete(string `/order/${res.order_id}`);
    test:assertEquals(deleteRes.statusCode, http:STATUS_OK);
}

@test:Config
function testOrderDeletionFailureWithAnInvalidOrderId() returns error? {
    http:Response res = check cl->delete("/order/invalid");
    test:assertEquals(res.statusCode, http:STATUS_NOT_FOUND);
}

@test:Config
function testOrderDeletionFailureWithAnInvalidOrderId2() returns error? {
    int testId = 5432;

    lock {
        string id = testId.toString();

        while orderStatus.hasKey(id) {
            testId -= 1;
        }
    }
    http:Response res = check cl->delete(string `/order/${testId}`);
    test:assertEquals(res.statusCode, http:STATUS_NOT_FOUND);
}

@test:Config
function testOrderDeletionFailureForNonPendingOrder() returns error? {
    record {|
        string order_id;
        int total;
    |} res = check cl->post("/order", {
        "username": "anne",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1}
        ]
    });

    string orderIdString = res.order_id;

    json orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");

    lock {
        orderStatus[orderIdString] = "in progress";
    }

    http:Response updateRes = check cl->delete(string `/order/${orderIdString}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(updateRes.statusCode, http:STATUS_FORBIDDEN);

    lock {
        orderStatus[orderIdString] = "completed";
    }

    updateRes = check cl->delete(string `/order/${orderIdString}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(updateRes.statusCode, http:STATUS_FORBIDDEN);
}

@test:Config
function testOrderDeletionFailureForNonPendingOrder2() returns error? {
    record {|
        string order_id;
        int total;
    |} res = check cl->post("/order", {
        "username": "marianne",
        "order_items": [
            {"item": "Tres Leches", "quantity": 1}
        ]
    });

    string orderIdString = res.order_id;

    json orderStatusJson = check cl->get(string `/order/${orderIdString}`);
    test:assertEquals(orderStatusJson.order_id, orderIdString);
    test:assertEquals(orderStatusJson.status, "pending");

    lock {
        orderStatus[orderIdString] = "in progress";
    }

    http:Response updateRes = check cl->delete(string `/order/${orderIdString}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(updateRes.statusCode, http:STATUS_FORBIDDEN);

    lock {
        orderStatus[orderIdString] = "completed";
    }

    updateRes = check cl->delete(string `/order/${orderIdString}`, {
        "order_items": [
            {"item": "Tres Leches", "quantity": 2}
        ]
    });
    test:assertEquals(updateRes.statusCode, http:STATUS_FORBIDDEN);
}
