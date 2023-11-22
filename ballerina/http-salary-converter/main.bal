import ballerina/http;

# The exchange rate API base URL
configurable string apiUrl = "http://localhost:8080";

type ConversionRates record {|
    string base;
    map<decimal> rates;
|};

# Convert provided salary to local currency.
#
# + salary - Salary in source currency
# + sourceCurrency - Soruce currency
# + localCurrency - Employee's local currency
# + return - Salary in local currency or error
public function convertSalary(decimal salary, string sourceCurrency, string localCurrency) returns decimal|error {
    http:Client aClient = check new(apiUrl);
    ConversionRates response = check aClient->get("/rates/" + sourceCurrency);
    decimal? rate = response.rates[localCurrency];
    if rate is () {
        return error("Can't find local currency");
    }
    return salary * rate;
}
