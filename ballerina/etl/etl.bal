public function transform(map<string[]> old) returns map<int> {
    var transformer = function(map<int> etl, [string, string[]] entry) returns map<int> {
        var value = int:fromString(entry[0]);
        if value is int {
            foreach string letter in entry[1] {
                etl[letter.toLowerAscii()] = value;
            }
        }
        return etl;
    };

    return old.entries().reduce(transformer, {});
}
