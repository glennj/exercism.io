public function isPangram(string sentence) returns boolean {
    map<boolean> seen = {};
    foreach string:Char c in "ABCDEFGHIJKLMNOPQRSTUVWXYZ" {
        seen[c] = false;
    }

    foreach string:Char c in sentence.toUpperAscii() {
        if seen.hasKey(c) {
            seen[c] = true;
        }
    }
    return seen.toArray().every(isSeen => isSeen);
}
