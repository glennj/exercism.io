string[] actions = ["wink", "double blink", "close your eyes", "jump"];

function bitAt(int num, int idx) returns int => num >> idx & 1;

public function commands(int code) returns string[] {
    boolean reversed = bitAt(code, actions.length()) == 1;
    return actions
        .enumerate()
        .reduce(
            function(string[] accum, [int, string] item) returns string[] {
                match [bitAt(code, item[0]), reversed] {
                    [1, false] => { accum.push(item[1]); }
                    [1, true]  => { accum.unshift(item[1]); }
                }
                return accum;
            },
            []
        );
}
