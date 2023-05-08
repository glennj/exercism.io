map<string> PROTEINS = {
    "AUG": "Methionine",
    "UUU": "Phenylalanine", "UUC": "Phenylalanine",
    "UUA": "Leucine", "UUG": "Leucine",
    "UCU": "Serine", "UCC": "Serine", "UCA": "Serine", "UCG": "Serine",
    "UAU": "Tyrosine", "UAC": "Tyrosine",
    "UGU": "Cysteine", "UGC": "Cysteine",
    "UGG": "Tryptophan",
    "UAA": "STOP", "UAG": "STOP", "UGA": "STOP"
};

# Translate a sequence of codons to a list of proteins.
#
# + strand - an RNA strand as a string
# + return - array of protein names, or error
public function proteins(string strand) returns string[]|error {
    int offset = 0;
    string[] proteins = [];

    while strand.length() - offset >= 3 {
        string codon = strand.substring(offset, offset + 3);
        match PROTEINS[codon] {
            ()     => { return error("Invalid codon"); }
            "STOP" => { return proteins; }
            var p  => {
                proteins.push(p ?: "");  // p is a string? but not nil
                offset += 3;
            }
        }
    }

    if offset < strand.length() { // there are leftover characters
        return error("Invalid codon");
    }
    return proteins;
}
