import ballerina/test;

// This is the mock function which will replace the real function
string[] outputs = [];

// This is the mock function which will replace the real function
@test:Mock {
    moduleName: "ballerina/io",
    functionName: "println"
}
test:MockFunction printlnMockFn = new;

public function mockPrint(any... val) {
    outputs.push(val.reduce(function (string a, any b) returns string => a + b.toString(), ""));
}

@test:Config
function testFunc() {
    test:when(printlnMockFn).call("mockPrint");
    // Invoking the main function
    main();
    test:assertEquals(outputs[0], "Hello, World!");
}
