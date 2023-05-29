# CSV Fuel Allowance

Welcome to CSV Fuel Allowance on Exercism's Ballerina Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

## Problem statement

Every employee of ABC Corp receives an unlimited fuel allowance. Employees are required to send a record every time they fill up their vehicles. These records are appended to the same CSV file and processed at the month's end.

As a member of the digital operations team, your task is to read this CSV file and write the results to another CSV file. The output file should contain an entry for each employee with the following details:

- Employee number
- Number of gas fill-ups
- Total fuel cost
- Total gallons
- Total miles accrued

You are given two `string` arguments.

1. Input CSV file path
1. Output CSV file path

The input CSV file has `n` lines, and each line takes the following form.

| Column name      | Ballerina type |
| ----------- | ----------- |
| employee_id      | `int`       |
| odometer_reading   | `int`       |
| gallons   | `decimal` |
| gas_price(USD)  |`decimal`|

The output file should contain an entry for each employee, sorted in ascending order by the `employee_id`. Each line takes the following form:

| Column name      | Ballerina type |
| ----------- | ----------- |
| employee_id      | `int`       |
| gas_fill_up_count   | `int`        |
| total_fuel_cost(USD)   | `decimal` |
| total_gallons  |`decimal`|
| total_miles_accrued  |`int`|

## Constraints

- Number of lines `n` in the input CSV file:  2 <= n <= 1000
- Number of employees in the company: 97
- Gas price is per gallon
- No missing fill-up records

## Example 1

- Input: `example01_input.csv`

```csv
2312,230,18.561,4.56
2312,500,19.345,4.89
```

- Output: `example01_output.csv`

```csv
2312,2,179.23521,37.906,270
```

## Example 2

- Input: `example02_input.csv`

```csv
2413,04089,21.682,3.46
3423,06582,15.248,4.56
2413,04127,04.221,3.40
2413,04349,11.192,4.10
3423,06767,08.696,3.34
2413,04547, 09.197,2.90
```

- Ouput: `example02_output.csv`

```csv
2413,4,161.92962,46.292,458
3423,2,98.57552,23.944,185
```

## Hints

- Use [`ballerina/io` module](https://lib.ballerina.io/ballerina/io/latest) to read/write csv files
- The input and output files will/should not contain headers
- [Ballerina table syntax](https://ballerina.io/learn/by-example/table)

## Source

### Created by

- @vordimous

### Contributed to by

- @vordimous

### Based on

This is an exercise to introduce users to converting the CSV values to application-specific types to work with data in a type-safe manner using Ballerina. - https://ballerina.io/learn/by-example/table-syntax