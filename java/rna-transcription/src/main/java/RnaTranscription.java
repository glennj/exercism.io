import java.util.HashMap;
import java.util.Map;
import static java.util.stream.Collectors.joining;

class RnaTranscription {

    private static Map<Character,String> TRANSCRIPTION;
    static {
        TRANSCRIPTION = new HashMap<>();
        TRANSCRIPTION.put('G', "C");
        TRANSCRIPTION.put('C', "G");
        TRANSCRIPTION.put('T', "A");
        TRANSCRIPTION.put('A', "U");
    }

    String transcribe(String dnaStrand) {
        // StringBuilder sb = new StringBuilder(); 
        // dnaStrand
        //     .chars()
        //     .mapToObj(c -> Character.valueOf((char) c))
        //     .map(c -> TRANSCRIPTION.get(c))
        //     .forEach(c -> sb.append(c));
        // return sb.toString();

        return dnaStrand
            .chars()
            .mapToObj(c -> TRANSCRIPTION.get(Character.valueOf((char) c)))
            .collect(joining());
    }

}
