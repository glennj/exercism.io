// Should the return type be [int[], int[]][]
function data() returns string[][]|error {
    return [
        //[inputFile, actualOutputFile, expectedOutputFile]
        [
            "tests/resources/example01_input.json",
            "target/example01_output.json",
            "tests/resources/example01_output_expected.json"
        ],
        [
            "tests/resources/example02_input.json",
            "target/example02_output.json",
            "tests/resources/example02_output_expected.json"
        ],
        [
            "tests/resources/example03_input.json",
            "target/example03_output.json",
            "tests/resources/example03_output_expected.json"
        ],
        [
            "tests/resources/example04_input.json",
            "target/example04_output.json",
            "tests/resources/example04_output_expected.json"
        ],
        [
            "tests/resources/example05_input.json",
            "target/example05_output.json",
            "tests/resources/example05_output_expected.json"
        ]
    ];
}
