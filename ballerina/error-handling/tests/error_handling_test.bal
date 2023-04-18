import ballerina/test;

string[] outputs = [];

@test:Mock {
    moduleName: "ballerina/io",
    functionName: "println"
}
test:MockFunction printlnMockFn = new();

public function mockPrint(any... val) {
    outputs.push(val.reduce(function (string a, any b) returns string => a + b.toString(), ""));
}

@test:Config
function testFunc() {
    // Invoking the main function
    test:when(printlnMockFn).call("mockPrint");
    error? result = main();
    test:assertEquals(outputs.length(), 2);
    string errorMessage = "Something wrong with the connection";
    test:assertEquals(outputs[0], "Many of life's failures are people who did not realize how close they were to success when they gave up. - Thomas A. Edison\n");
    test:assertEquals(outputs[1], errorMessage);
    test:assertTrue(result is error, "The main function must return an error");
    test:assertEquals((<error>result).message(), errorMessage);
}
