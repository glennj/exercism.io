// Should the return type be [int[], int[]][]
function data() returns string[][]|error {
    return [
        //[inputFile, actualOutputFile, expectedOutputFile]
        [
            "tests/resources/example01_input.xml",
            "target/example01_output.xml",
            "tests/resources/example01_output_expected.xml"
        ],
        [
            "tests/resources/example02_input.xml",
            "target/example02_output.xml",
            "tests/resources/example02_output_expected.xml"
        ],
        [
            "tests/resources/example03_input.xml",
            "target/example03_output.xml",
            "tests/resources/example03_output_expected.xml"
        ],
        [
            "tests/resources/example04_input.xml",
            "target/example04_output.xml",
            "tests/resources/example04_output_expected.xml"
        ],
        [
            "tests/resources/example05_input.xml",
            "target/example05_output.xml",
            "tests/resources/example05_output_expected.xml"
        ]
    ];
}
