# The Cake is a Lie

Welcome to The Cake is a Lie on Exercism's Ballerina Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

## Problem Statement

**CakeStation**, a popular bakery that bakes various cakes, has asked you to set up an online order management system for them.

The details of items on the menu are as follows.

| Item | Price |
| ---- | ----- |
| Butter Cake | 15 |
| Chocolate Cake | 20 |
| Tres Leches | 25 |

The following is expected to be possible.

- Retrieve the menu.
- Place an order.
- Retrieve the status of an order (one of `pending`, `in progress`, or `completed`).
- Update an order. Only allowed if the status is still `pending`.
- Delete an order. Only allowed if the status is still `pending`.

## Constraints

1. Retrieve the Menu
    - Method - `GET`
    - Path - `/menu`
    - Response
        - 200 OK
        - content-type - `application/json`
        - a JSON object with the item names as the keys and the prices as the values (integer)
            ```json
            {
               "Butter Cake": 15,
               "Chocolate Cake": 20,
               "Tres Leches": 25
            }
            ```

2. Place an Order
    - Method - `POST`
    - Path - `/order`
    - Request
        - the request payload will be a JSON object with the following fields
            - `username` - username of the user (`string`)
            - `order_items` - an array of JSON objects representing the order items. Each JSON object will contain exactly two fields: a string `item` field indicating the name of the item and an integer `quantity` field indicating the order quantity for the particular item.
            ```json
            {
                "type": "object",
                "properties": {
                    "username": {
                        "type": "string"
                    },
                    "order_items": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "item": {
                                    "type": "string"
                                },
                                "quantity": {
                                    "type": "integer"
                                }
                            },
                            "required": [
                                "item",
                                "quantity"
                            ]
                        }
                    }
                },
                "required": [
                    "username",
                    "order_items"
                ]
            }
            ```

    - Response
        - 400 Bad Request if
            - the payload does not match the expected format or
            - the `username` is empty or
            - the `order_items` array is empty or
            - the `order_items` array contains more than one JSON object for a particular type of cake or
            - a member of the `order_items` array has an `item` field with a type of cake that is not on the menu or
            - a member of the `order_items` array has a `quantity` field that has a value less than 1
        - 201 Created if the order is placed successfully
            - content-type - `application/json`
            - a JSON object with two fields
                - an `order_id` containing the randomly generated order ID string. This should be the string representation of a valid numeric value.
                - a `total` field containing the total value (integer) of the order (i.e., sum of price x quantity for each kind of cake)
                ```json
                {
                    "type": "object",
                    "properties": {
                        "order_id": {
                            "type": "string"
                        },
                        "total": {
                            "type": "integer"
                        }
                    },
                    "required": [
                        "order_id",
                        "total"
                    ]
                }
                ```

3. Retrieve the Status of an Order
    - Method - `GET`
    - Path - `/order/{orderId}`
    - Response
        - 404 Not Found if there is no order with the specified `orderId`
        - 200 OK
            - content-type - `application/json`
            - a JSON object with two string fields `order_id` and `status`, representing the order ID and the status (one of `pending`, `in progress`, or `completed`) respectively

                ```json
                {
                    "type": "object",
                    "properties": {
                        "order_id": {
                            "type": "string"
                        },
                        "status": {
                            "type": "string"
                        }
                    },
                    "required": [
                        "order_id",
                        "status"
                    ]
                }
                ```

4. Update an Order. Only allowed if the status is still `pending`.
    - Method - `PUT`
    - Path - `/order/{orderId}`
    - Request
        - the request payload will be a JSON object with a single field `order_items`.
            - `order_items` - an array of JSON objects representing the order items. Each JSON object will contain exactly two fields: a string `item` field indicating the name of the item and an integer `quantity` field indicating the order quantity for the particular item.

            ```json
            {
                "type": "object",
                "properties": {
                    "order_items": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "item": {
                                    "type": "string"
                                },
                                "quantity": {
                                    "type": "integer"
                                }
                            },
                            "required": [
                                "item",
                                "quantity"
                            ]
                        }
                    }
                },
                "required": [
                    "order_items"
                ]
            }
            ```

    - Response
        - 404 Not Found if there is no order with the specified `orderId`
        - 403 Forbidden if the order status is not `pending` (i.e., `in progress` or `completed`)
        - 400 Bad Request if the `order_items` array doesn't meet the same requirements as that for placing the order (2)
        - 200 OK if the update is successful
            - content-type - `application/json`
            - a JSON object with two fields
                - an `order_id` containing the same order ID string as the original order
                - a `total` field containing the total value of the order (i.e. the sum of price x quantity for each kind of cake)

                ```json
                {
                    "type": "object",
                    "properties": {
                        "order_id": {
                            "type": "string"
                        },
                        "total": {
                            "type": "integer"
                        }
                    },
                    "required": [
                        "order_id",
                        "total"
                    ]
                }
                ```

        >**Note**: if an update attempt fails, no changes should be done to the existing order.

5. Delete an Order. Only allowed if the status is still `pending`.
    - Method - `DELETE`
    - Path - `/order/{orderId}`
    - Response
        - 404 Not Found if there is no order with the specified `orderId`
        - 403 Forbidden if the order status is not `pending` (i.e., `in progress` or `completed`)
        - 200 OK if the deletion is successful

6. The generated order ID should be unique, should be a string, and should be a string representation of a valid number (e.g., `"1"`, `"1234"`).

## Definition

- Use a `configurable` variable `port` with the default value of `8080` to specify the port the service listens on.
- Use a module-level `orderStatus` map to maintain the status of the orders, in which the value is either `"pending"`, `"in progress"`, or `"completed"` against the randomly generated order ID string.
  - key - randomly generated order ID string
  - value - status of the order (`"pending"`, `"in progress"`, or `"completed"`)
- Update the `orderStatus` map with the value `"pending"` for each new order.

## Example 1

Retrieving the menu.

```cmd
$ curl -v http://localhost:8080/menu

< HTTP/1.1 200 OK
< content-type: application/json
{"Butter Cake":15, "Chocolate Cake":20, "Tres Leches":25}
```

## Example 2

Attempting to place the order with a valid payload.

```cmd
$ curl -v http://localhost:8080/order -d '{"username": "mary", "order_items": [{"item": "Tres Leches", "quantity": 1}, {"item": "Chocolate Cake", "quantity": 2}]}' -H 'Content-Type: application/json'

< HTTP/1.1 201 Created
< content-type: application/json
{"order_id":"7", "total":65}
```

**Note**: "order_id" should/will be a randomly generated **unique** string.

## Example 3

Attempting to place the order with payload does not match the expected format.

```cmd
$ curl -v http://localhost:8080/order -d '{"username": "mary", "order_items": [{"Tres Leches": 1}, {"Chocolate Cake": 2}]}' -H 'Content-Type: application/json'

< HTTP/1.1 400 Bad Request
```

## Hints

- [Simple HTTP service](https://ballerina.io/learn/by-example/http-basic-rest-service/)
- A [quoted identifier](https://ballerina.io/learn/language-basics/#syntax) (`'order`) can be used to use `order` (which is a keyword) as the path.
- Using [service data binding](https://ballerina.io/learn/by-example/http-service-data-binding/) will validate that the payload is of the expected structure, and will handle responding with status code 400 (Bad Request) when binding fails.
- Using [status code records as return types](https://ballerina.io/learn/by-example/http-send-different-status-codes-with-payload/) and returning record values of those types from resource methods will handle setting the relevant status code to the response internally.
- [Configurable variables](https://ballerina.io/learn/by-example/configurable-variables)

## Source

### Created by

- @vordimous

### Contributed to by

- @vordimous

### Based on

This is an exercise to introduce users to using Ballerina HTTP service data binding - https://ballerina.io/learn/by-example/http-service-data-binding/