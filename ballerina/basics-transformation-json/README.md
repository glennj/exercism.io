# JSON Fuel Allowance

Welcome to JSON Fuel Allowance on Exercism's Ballerina Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

## Problem statement

Every employee of ABC Corp receives an unlimited fuel allowance. Employees are required to send a record every time they fill up their vehicles. These records are appended to the same JSON file and processed at the month's end.

As a member of the digital operations team, your task is to read this JSON file and write the results to another JSON file. The output file should contain an entry for each employee with the following details:

- Employee number
- Number of gas fill-ups
- Total fuel cost
- Total gallons
- Total miles accrued

You are given two `string` arguments.

1. Input JSON file path.
1. Output JSON file path.

Input JSON file contains an array of JSON objects. Each JSON object takes the following form.


```json
{
    "type": "array",
    "items": {
        "type": "object",
        "properties": {
            "employeeId": {
                "type": "integer"
            },
            "odometerReading": {
                "type": "integer"
            },
            "gallons": {
                "type": "number"
            },
            "gasPrice": {
                "type": "number"
            }
        },
        "required": [
            "employeeId",
            "odometerReading",
            "gallons",
            "gasPrice"
        ]
    }
}
```

```ballerina
// Ballerina record type definition
type FillUpEntry record {|
    int employeeId;
    int odometerReading;
    decimal gallons;
    decimal gasPrice;
|};
```

Your task is to transform the JSON input to the following JSON format and write the content to the given path. The output file should contain an entry for each employee, sorted in ascending order by the `employeeId`.

```json
{
    "type": "array",
    "items": {
        "type": "object",
        "properties": {
            "employeeId": {
                "type": "integer"
            },
            "gasFillUpCount": {
                "type": "integer"
            },
            "totalFuelCost": {
                "type": "number"
            },
            "totalGallons": {
                "type": "number"
            },
            "totalMilesAccrued": {
                "type": "integer"
            }
        },
        "required": [
            "employeeId",
            "gasFillUpCount",
            "totalFuelCost",
            "totalGallons",
            "totalMilesAccrued"
        ]
    }
 }
```

```ballerina
// Ballerina record type definition
type EmployeeFillUpSummary record {|
    int employeeId;
    int gasFillUpCount;
    decimal totalFuelCost;
    decimal totalGallons;
    int totalMilesAccrued;
|};
```

## Constraints

- Number of fill-up records `n` in the input JSON file:  0 <= n <= 1000
- Number of employees in the company: 97
- Gas price is per gallon
- No missing fill-up records

## Example 1

- Input: `example01_input.json`

```json
[
    {
        "employeeId": 2312,
        "odometerReading": 230,
        "gallons": 18.561,
        "gasPrice": 4.56
    },
    {
        "employeeId": 2312,
        "odometerReading": 500,
        "gallons": 19.345,
        "gasPrice": 4.89
    }
]
```

- Output: `example01_output.json`

```json
[
    {
        "employeeId": 2312,
        "gasFillUpCount": 2,
        "totalFuelCost": 179.23521,
        "totalGallons": 37.906,
        "totalMilesAccrued": 270
    }
]
```

## Example 2

- Input: `example02_input.json`

```json
[
    {
        "employeeId": 2413,
        "odometerReading": 4089,
        "gallons": 21.682,
        "gasPrice": 3.46
    },
    {
        "employeeId": 3423,
        "odometerReading": 6582,
        "gallons": 15.248,
        "gasPrice": 4.56
    },
    {
        "employeeId": 2413,
        "odometerReading": 4127,
        "gallons": 4.221,
        "gasPrice": 3.40
    },
    {
        "employeeId": 2413,
        "odometerReading": 4349,
        "gallons": 11.192,
        "gasPrice": 4.10
    },
    {
        "employeeId": 3423,
        "odometerReading": 6767,
        "gallons": 8.696,
        "gasPrice": 3.34
    },
    {
        "employeeId": 2413,
        "odometerReading": 4547,
        "gallons": 9.197,
        "gasPrice": 2.90
    }
]

```

- Output: `example02_output.json`

```json
[
    {
        "employeeId": 2413,
        "gasFillUpCount": 4,
        "totalFuelCost": 161.92962,
        "totalGallons": 46.292,
        "totalMilesAccrued": 458
    },
    {
        "employeeId": 3423,
        "gasFillUpCount": 2,
        "totalFuelCost": 98.57552,
        "totalGallons": 23.944,
        "totalMilesAccrued": 185
    }
]
```

## Hints

- Use [`ballerina/io` module](https://lib.ballerina.io/ballerina/io/latest) to read/write JSON files.
- Convert the JSON values to application-specific types to work with data in a type-safe manner. Refer to [Converting to user defined type example](https://ballerina.io/learn/by-example/convert-from-json-to-user-defined-type) in Ballerina By Examples (BBE) for information.

```ballerina
   json jsonPayload = {id: "2", title: "Jeru", artist: "Gerry Mulligan", price: 17.99};

   // Using cloneWithType  
   Album album1 = check jsonPayload.cloneWithType();
   io:println(album1);
 
   // Using fromJsonWithType
   Album album2 = check jsonPayload.fromJsonWithType();
   io:println(album2);
```

- [Destructuring records](https://ballerina.io/learn/by-example/destructure-records-using-query) can be useful in reducing the verbosity of the code.

```ballerina
   Album {id, title, artist, price} = album1;
   io:println(id);
   io:println(title);
```

- Use expression-bodied functions for functions with a single expression. E.g., functions that have only the return statement.

```ballerina
   function cfr(int deaths, int cases) returns decimal => <decimal>deaths / <decimal>cases * 100;
```

- [Ballerina table syntax](https://ballerina.io/learn/by-example/table)

## Source

### Created by

- @vordimous

### Contributed to by

- @vordimous

### Based on

This is an exercise to introduce users to converting the JSON values to application-specific types to work with data in a type-safe manner using Ballerina. - https://ballerina.io/learn/by-example/destructuring-records