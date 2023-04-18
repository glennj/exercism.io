import ballerina/test;
import ballerina/http;

final http:Client httpEndpoint = check new ("http://localhost:9090");

@test:Config
function testFunc() returns error? {
    float opr1 = 10.1;
    float opr2 = 4.2;
    string operator = "+";

    ResponseType response = check getResponse(opr1, opr2, operator);
    test:assertEquals(response.result, opr1 + opr2);
    test:assertEquals(response.expression, string `${opr1}${operator}${opr2}`);

    opr1 = 1f;
    opr2 = 1f;
    operator = "$";
    response = check getResponse(opr1, opr2, operator);
    test:assertEquals(response.result, 0f);
    test:assertEquals(response.expression, "1.0$1.0");
}

function getResponse(float operand1, float operand2, string operator) returns ResponseType|error {
    return httpEndpoint->post("/calc", {"operand1": operand1, "operand2": operand2, "operator": operator});
}

type ResponseType record {|
    string expression;
    float result;
|};
