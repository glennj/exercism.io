import ballerina/test;

@test:Config {}
function testCanIdentifySingleSaddlePoint() {
    int[][] matrix = [
        [9, 8, 7],
        [5, 3, 2],
        [6, 6, 7]
    ];

    map<int>[] expected = [{"row": 2, "column": 1}];
    test:assertEquals(saddlePoints(matrix), expected);
}

@test:Config {
    //enable: false
}
function testCanIdentifyThatEmptyMatrixHasNoSaddlePoints() {
    int[][] matrix = [
        []
    ];
    map<int>[] expected = [];
    test:assertEquals(saddlePoints(matrix), expected);
}

@test:Config {
    //enable: false
}
function testCanIdentifyLackOfSaddlePointsWhenThereAreNone() {
    int[][] matrix = [
        [1, 2, 3],
        [3, 1, 2],
        [2, 3, 1]
    ];
    map<int>[] expected = [];
    test:assertEquals(saddlePoints(matrix), expected);
};

@test:Config {
    //enable: false
}
function testCanIdentifyMultipleSaddlePointsInAColumn() {
    int[][] matrix = [
        [4, 5, 4],
        [3, 5, 5],
        [1, 5, 4]
    ];
    map<int>[] expected = [
        {"row": 1, "column": 2},
        {"row": 2, "column": 2},
        {"row": 3, "column": 2}
    ];
    test:assertEquals(saddlePoints(matrix), expected);
};

@test:Config {
    //enable: false
}
function testCanIdentifyMultipleSaddlePointsInARow() {
    int[][] matrix = [
        [6, 7, 8],
        [5, 5, 5],
        [7, 5, 6]
    ];
    map<int>[] expected = [
        {"row": 2, "column": 1},
        {"row": 2, "column": 2},
        {"row": 2, "column": 3}
    ];
    test:assertEquals(saddlePoints(matrix), expected);
};

@test:Config {
    //enable: false
}
function testCanIdentifySaddlePointInBottomRightCorner() {
    int[][] matrix = [
        [8, 7, 9],
        [6, 7, 6],
        [3, 2, 5]
    ];
    map<int>[] expected = [
        {"row": 3, "column": 3}
    ];
    test:assertEquals(saddlePoints(matrix), expected);
};

@test:Config {
    //enable: false
}
function testCanIdentifySaddlePointsInANonSquareMatrix() {
    int[][] matrix = [
        [3, 1, 3],
        [3, 2, 4]
    ];
    map<int>[] expected = [
        {"row": 1, "column": 1},
        {"row": 1, "column": 3}
    ];
    test:assertEquals(saddlePoints(matrix), expected);
};

@test:Config {
    //enable: false
}
function testCanIdentifyThatSaddlePointsInASingleColumnMatrixAreThoseWithTheMinimumValue() {
    int[][] matrix = [
        [2],
        [1],
        [4],
        [1]
    ];
    map<int>[] expected = [
        {"row": 2, "column": 1},
        {"row": 4, "column": 1}
    ];
    test:assertEquals(saddlePoints(matrix), expected);
};

@test:Config {
    //enable: false
}
function testCanIdentifyThatSaddlePointsInASingleRowMatrixAreThoseWithTheMaximumValue() {
    int[][] matrix = [
        [2, 5, 3, 5]
    ];
    map<int>[] expected = [
        {"row": 1, "column": 2},
        {"row": 1, "column": 4}
    ];
    test:assertEquals(saddlePoints(matrix), expected);
}
