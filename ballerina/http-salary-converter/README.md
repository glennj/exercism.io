# PaySwitch - Salary Converter

Welcome to PaySwitch - Salary Converter on Exercism's Ballerina Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

## Problem Statement

A company called **Insure Everyone** is paying its employees in foreign currencies. But, due to legislative requirements, the salary must be converted to the local currency. Its employees are located around the world.
Insure Everyone's HR department is seeking help from a talented developer to build a program to convert each employee's salary to their corresponding local currency. Insure Everyone relies on exchange rates provided by an external API. 

Given the external exchange rate API, you need to write a function that takes a `source currency`, `salary` (in source currency), and the employee's `local currency` that will return the salary in local currency.

### Currency Exchange API

The response from the external currency exchange API is given below:

>**Note**: that the `rates` are truncated to keep the problem statement short

```json
{
    "base": "EUR",
    "rates": {
        "AED": 4.167972,
        "AFN": 118.91123,
        "ALL": 121.353306,
        "AMD": 546.175542,
        "ANG": 2.035384,
        "AOA": 629.106987,
        "ARS": 116.624363,
        "AUD": 1.580224,
        "AWG": 2.042846,
        "AZN": 1.92923,
        "BAM": 1.95468,
        "BBD": 2.26981,
        "BDT": 97.024176,
        "BGN": 1.956957,
        "BHD": 0.428175,
        "BIF": 2257.462616,
        "BMD": 1.134504,
        "BND": 1.534691,
        "BOB": 7.773946,
        "BRL": 6.394753,
        "BSD": 1.134916,
        "BTC": 0.000027,
        "BTN": 83.896824,
        "BWP": 13.167125,
    }
}
```

An example request to get exchange rates for `USD`:

```http
GET http://localhost:8080/rates/USD

{
  "base": "USD",
  "rates": {
    "AED": 3.672477643818380310171368756063197,
    "AFN": 104.7748962262571579872079201988508,
    "ALL": 106.9266548068103418652047146893925,
    "AMD": 481.2454276553169780099231576470961,
    "ANG": 1.793414695824643301163693331958788,
    "AOA": 554.3178661774696212335484143830275,
    "ARS": 102.7598951821316197338845611280433,
    "AUD": 1.392364754903645275750470737610812,
    "AWG": 1.799989602800547350872880138793802,
    "AZN": 1.699880432206294535038121596128717,
    "BAM": 1.722304900517304728699178180683942,
    "BBD": 1.999971804204874171858658003508614,
    "BDT": 85.48980589838411660371074286227852,
    "BGN": 1.724311207564226937381544618524109,
    "BHD": 0.3772729555625457630971671155965922,
    "BIF": 1989.092294529751410010035940827599,
    "BMD": 0.9996325735447665520331371082216295
    // omitted
  }
}
```

## Constraints

* 500 <= `salary` <= 10000
* `sourceCurrency` and `targetCurrency` are [ISO currency codes (alphabetic)](https://en.wikipedia.org/wiki/ISO_4217#Active_codes). Test input may contain invalid currency codes.
* The accuracy of the returned rate will be measured up to `5` decimal points.

## Definition

You have to write your code inside the following function.

```ballerina
# The exchange rate API base URL
configurable string apiUrl = "http://localhost:8080";

# Convert provided salary to local currency.
#
# + salary - Salary in source currency
# + sourceCurrency - Soruce currency
# + localCurrency - Employee's local currency
# + return - Salary in local currency or error
public function convertSalary(decimal salary, string sourceCurrency, string localCurrency) returns decimal|error {
    // TODO: Write your code here
    return 0;
}
```

## Test Environment

* You can either test it manually by running the project with `bal run` or by writing unit tests and running them with `bal test`. The backend service will start automatically at `http://localhost:8080`

## Example

### Example 1

#### Input:
* Salary: 1000
* Source Currency: USD
* Target Currency: GBP

#### Output:

Assuming the external currency exchange returned the following conversion rates response (omitted):

```json
{
    "base": "USD",
    "rates": {
        // omitted
        "GBP": 0.73584,
        // omitted
    }
}
```

The function must return `735.84000`

### Example 2

#### Input:

* Salary: 1000
* Source Currency: GBP
* Target Currency: USD

#### Output:

Assuming the external currency exchange returned following conversion rates response (omitted):

```json
{
    "base": "GBP",
    "rates": {
        // omitted
        "USD": 1.35898,
        // omitted
    }
}
```

The function must return `1358.9800`

## Hints

* You can convert the JSON response received to a user-defined type (more specifically an open record) for easy access. See [http client data binding](https://ballerina.io/learn/by-example/http-client-data-binding/) for more information

## Source

### Created by

- @LakshanWeerasinghe

### Contributed to by

- @LakshanWeerasinghe

### Based on

This is an exercise to introduce users to using Ballerina HTTP service data binding - https://ballerina.io/learn/by-example/http-service-data-binding/