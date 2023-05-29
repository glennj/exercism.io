import ballerina/http;
import ballerina/mime;

configurable int port = 8080;

enum CakeKind {
    BUTTER_CAKE = "Butter Cake",
    CHOCOLATE_CAKE = "Chocolate Cake",
    TRES_LECHES = "Tres Leches"
}

const map<int> MENU = {
    "Butter Cake": 15,
    "Chocolate Cake": 20,
    "Tres Leches": 25
};

enum OrderStatus {
    PENDING = "pending",
    IN_PROGRESS = "in progress",
    COMPLETED = "completed"
}

type OrderDetail record {|
    CakeKind item;
    int quantity;
|};

type Order record {|
    string username;
    OrderDetail[] order_items;
|};

type OrderUpdate record {|
    OrderDetail[] order_items;
|};

isolated map<Order> orders = {};
isolated map<OrderStatus> orderStatus = {};

service on new http:Listener(port) {
    resource function get menu() returns MENU => MENU;


    isolated resource function post 'order(@http:Payload Order & readonly newOrder) returns http:Created|http:BadRequest|error {
        if newOrder.username == "" {
            return <http:BadRequest>{body: {message: "missing username"}};
        }

        string order_id = check generateOrderId();

        int|http:BadRequest sum = computeSum(newOrder.order_items);
        if sum is http:BadRequest {
            return sum;
        }

        lock {
            orders[order_id] = newOrder;
        }
        lock {
            orderStatus[order_id] = PENDING;
        }

        return <http:Created>{
            body: {order_id, total: <int>sum},
            mediaType: mime:APPLICATION_JSON
        };
    }

    isolated resource function get 'order/[string orderId]() returns http:Ok|http:NotFound {
        lock {
            if !orders.hasKey(orderId) {
                return <http:NotFound>{body: {message: "unknown order id"}};
            }
        }

        string status = "";
        lock {
            status = orderStatus.get(orderId);
        }
        return <http:Ok>{
            body: {
                order_id: orderId,
                status
            },
            mediaType: mime:APPLICATION_JSON
        };
    }

    resource function put 'order/[string orderId](@http:Payload OrderUpdate & readonly updatedOrder) returns http:Ok|http:BadRequest|http:Forbidden|http:NotFound {
        lock {
            if !orders.hasKey(orderId) {
                return <http:NotFound>{body: {message: "unknown order id"}};
            }
        }
        lock {
            if orderStatus[orderId] != PENDING {
                return <http:Forbidden>{body: {message: "cannot update order"}};
            }
        }

        int|http:BadRequest sum = computeSum(updatedOrder.order_items);
        if sum is http:BadRequest {
            return sum;
        }

        lock {
            orders[orderId] = {
                username: orders.get(orderId).username,
                order_items: updatedOrder.order_items
            };
        }
        lock {
            orderStatus[orderId] = PENDING;
        }

        return <http:Ok>{
            body: {order_id: orderId, total: <int>sum}
        };
    }

    resource function delete 'order/[string orderId]() returns http:Ok|http:Forbidden|http:NotFound {
        lock {
            if !orders.hasKey(orderId) {
                return <http:NotFound>{body: {message: "unknown order id"}};
            }
        }
        lock {
            if orderStatus[orderId] != PENDING {
                return <http:Forbidden>{body: {message: "cannot delete order"}};
            }
        }

        lock {
            _ = orders.remove(orderId);
        }
        lock {
            _ = orderStatus.remove(orderId);
        }
        return <http:Ok>{};
    }
}


isolated int currentOrderId = 0;

isolated function generateOrderId() returns string|error {
    lock {
        currentOrderId += 1;
        return currentOrderId.toString();
    }
}


isolated function computeSum(OrderDetail[] items) returns int|(http:BadRequest & readonly) {
    if items.length() == 0 {
        return <http:BadRequest & readonly>{body: {message: "missing order items"}};
    }

    int total = 0;
    map<boolean> seen = {};
    foreach OrderDetail item in items {
        if seen.hasKey(item.item) {
            return <http:BadRequest & readonly>{body: {message: "duplicate item"}};
        }
        if !MENU.hasKey(item.item) {
            return <http:BadRequest & readonly>{body: {message: "unknown item"}};
        }
        if item.quantity <= 0 {
            return <http:BadRequest & readonly>{body: {message: "must order a positive number"}};
        }
        seen[item.item] = true;
        total += MENU.get(item.item) * item.quantity;
    }
    return total;
}
