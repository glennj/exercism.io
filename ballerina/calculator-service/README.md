# Calculator Service

Welcome to Calculator Service on Exercism's Ballerina Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

HTTP endpoints are better with type checking. Adding a Record type to a POST request will ensure the correct data is sent every time.

## Objectives

- Modify the provided code.
- Add the necessary attributes to this record to accept two operands and an operator.
- Add an HTTP resource function to accept a POST request on path '/calc'
- The function should accept the Calculation record type as the payload and return a generic JSON object

### Accepts

```json
{ "operand1": 4, "operand2": 10.2, "operator": "+" }
```

### Returns

```json
{ "expression": "4+10.2", "result": 14.2 }
```

## Source

### Contributed to by

- @vordimous
- @hemikak
- @kasun04
- @vinok88
- @ThisaruGuruge

### Based on

This is an exercise to introduce the Ballerina Record type in Network interactions - https://ballerina.io/learn/by-example/#records