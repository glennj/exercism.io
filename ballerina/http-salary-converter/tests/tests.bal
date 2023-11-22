import ballerina/test;

@test:Config {
    dataProvider: validData,
    groups: ["sample"]
}
function testConversion(decimal salary, string sourceCurrency, string localCurrency, decimal expected) returns error? {
    decimal convertedSalary = check convertSalary(salary, sourceCurrency, localCurrency);
    test:assertTrue(convertedSalary.toString().startsWith(expected.toString()));
}

@test:Config {
    dataProvider: invalidData,
    groups: ["sample"]
}
function testInvalidInput(string sourceCurrency, string localCurrency) {
    decimal|error convertedSalary = convertSalary(1000, sourceCurrency, localCurrency);
    test:assertTrue(convertedSalary is error);
}

function validData() returns map<[decimal, string, string, decimal]> {
    return {
        "case1": [1350.25, "USD", "GBP", 1028.50432],
        "case2": [1300, "GBP", "USD", 1706.67730]
    };
}

function invalidData() returns string[][] {
    return [
        ["ABC", "GBP"],
        ["", ""]
    ];
}
