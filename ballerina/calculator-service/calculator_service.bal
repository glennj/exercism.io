import ballerina/http;

public type Calculation record {|
    float operand1;
    float operand2;
    string operator;
|};

public type Response record {|
    float result = 0f;
    string expression = "";
|};

service / on new http:Listener(9090) {

    resource function post calc(@http:Payload Calculation 'c) returns json {
        Response resp = {};

        match 'c.operator {
            "+" => {
                resp.result = 'c.operand1 + c.operand2;
            }
            "-" => {
                resp.result = 'c.operand1 - c.operand2;
            }
            "x"|"*" => {
                resp.result = 'c.operand1 * c.operand2;
            }
            "/" => {
                resp.result = 'c.operand1 / c.operand2;
            }
        }

        resp.expression = string`${'c.operand1}${c.operator}${c.operand2}`;

        return resp.toJson();
    }
}
