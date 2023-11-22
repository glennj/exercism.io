import ballerina/io;
import ballerina/http;

type Rates record {|
    string base;
    map<decimal> rates;
|};

service / on new http:Listener(8080) {

    private final Rates rates;

    function init() returns error? {
        json ratesJson = check io:fileReadJson("tests/resources/rates.json");
        self.rates = check ratesJson.fromJsonWithType(Rates);
    }

    resource function get rates/[string baseCurrency]() returns Rates|error {
        decimal? rate = self.rates.rates[baseCurrency];
        if rate is () {
            return error("Invalid base currency");
        }

        Rates response = {
            base: baseCurrency,
            rates: {}
        };

        foreach [string, decimal] entry in self.rates.rates.entries() {
            string currency = entry[0];
            decimal rateInEuro = entry[1];
            decimal rateInBaseCurrency = rateInEuro / rate;
            response.rates[currency] = rateInBaseCurrency;
        }

        return response;
    }
}
