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

@test:Config {}
function testFunc() {
    test:when(printlnMockFn).call("mockPrint");
    main();

}
