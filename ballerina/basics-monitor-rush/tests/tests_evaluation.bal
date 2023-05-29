import ballerina/test;

@test:Config {
    dataProvider: dataEval,
    groups: ["evaluation"]
}
function allocateCubiclesTestEval(int[] input, int[] expected) returns error? {
    test:assertEquals(allocateCubicles(input), expected);
}
