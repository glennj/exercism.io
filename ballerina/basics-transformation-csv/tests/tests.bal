import ballerina/file;
import ballerina/io;
import ballerina/test;

@test:Config {
    dataProvider: data
}
function processFuelRecordsTest(string inputFile, string outputFileActual, string outputFileExpected) returns error? {
    error? e = processFuelRecords(inputFile, outputFileActual);
    if e is error {
        io:println(e);
        test:assertFail(e.message());
    }

    check checkIfFileExists(outputFileActual);
    string[][] outputActual = check io:fileReadCsv(outputFileActual);
    string[][] outputExpected = check io:fileReadCsv(outputFileExpected);
    test:assertEquals(outputActual, outputExpected);
}

function checkIfFileExists(string path) returns error? {
    boolean result = check file:test(path, file:EXISTS);
    if !result {
        return error(string `File ${path} not found`);
    }
}
