// Should the return type be [int[], int[]][]
function data() returns string[][]|error {
    return [
        //[inputFile, actualOutputFile, expectedOutputFile]
        [
            "tests/resources/example01_input.csv",
            "target/example01_output.csv",
            "tests/resources/example01_output_expected.csv"
        ],
        [
            "tests/resources/example02_input.csv",
            "target/example02_output.csv",
            "tests/resources/example02_output_expected.csv"
        ],
        [
            "tests/resources/example03_input.csv",
            "target/example03_output.csv",
            "tests/resources/example03_output_expected.csv"
        ],
        [
            "tests/resources/example04_input.csv",
            "target/example04_output.csv",
            "tests/resources/example04_output_expected.csv"
        ],
        [
            "tests/resources/example05_input.csv",
            "target/example05_output.csv",
            "tests/resources/example05_output_expected.csv"
        ]
    ];
}
