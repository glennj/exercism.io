import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.stream.IntStream;
import java.util.stream.Collectors;

class ProteinTranslator {

    private static final Map<String,String> Codon;
    static {
        Codon = new HashMap<>();
        Codon.put("AUG","Methionine");
        Codon.put("UUU","Phenylalanine");
        Codon.put("UUC","Phenylalanine");
        Codon.put("UUA","Leucine");
        Codon.put("UUG","Leucine");
        Codon.put("UCU","Serine");
        Codon.put("UCC","Serine");
        Codon.put("UCA","Serine");
        Codon.put("UCG","Serine");
        Codon.put("UAU","Tyrosine");
        Codon.put("UAC","Tyrosine");
        Codon.put("UGU","Cysteine");
        Codon.put("UGC","Cysteine");
        Codon.put("UGG","Tryptophan");
        Codon.put("UAA","STOP");
        Codon.put("UAG","STOP");
        Codon.put("UGA","STOP");
    }

    List<String> translate(String rnaSequence) {
        List<String> proteins = new ArrayList<>();
        for (int i = 0; i < rnaSequence.length(); i += 3) {
            String codon = rnaSequence.substring(i, i + 3);
            String protein = Codon.get(codon);
            if (protein == null)
                throw new IllegalArgumentException("Invalid codon");
            if (protein == "STOP")
                break;
            proteins.add(protein);
        }
        return proteins;
    }
}