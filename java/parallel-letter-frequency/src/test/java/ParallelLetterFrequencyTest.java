import org.junit.Ignore;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

public class ParallelLetterFrequencyTest {

    // American national anthem
    private String starSpangledBanner =
            "O say can you see by the dawn's early light,\n" +
                    "What so proudly we hailed at the twilight's last gleaming,\n" +
                    "Whose broad stripes and bright stars through the perilous fight,\n" +
                    "O'er the ramparts we watched, were so gallantly streaming?\n" +
                    "And the rockets' red glare, the bombs bursting in air,\n" +
                    "Gave proof through the night that our flag was still there;\n" +
                    "O say does that star-spangled banner yet wave,\n" +
                    "O'er the land of the free and the home of the brave?\n";


    @Test
    public void noTextsMeansNoLetters() {
        String input = "";
        Map<Integer, Integer> expectedOutput = new HashMap<Integer, Integer>();
        ParallelLetterFrequency p = new ParallelLetterFrequency(input);

        assertThat(p.letterCounts()).isEqualTo(expectedOutput);
    }

    //@Ignore("Remove to run test")
    @Test
    public void oneLetterIsCorrectlyCounted() {
        String input = "a";
        Map<Integer, Integer> expectedOutput = new HashMap<Integer, Integer>() {
            {
                put((int) 'a', 1);
            }
        };
        ParallelLetterFrequency p = new ParallelLetterFrequency(input);

        assertThat(p.letterCounts()).isEqualTo(expectedOutput);
    }

    //@Ignore("Remove to run test")
    @Test
    public void resultsAreCaseInsensitive() {
        String input = "Aa";
        Map<Integer, Integer> expectedOutput = new HashMap<Integer, Integer>() {
            {
                put((int) 'a', 2);
            }
        };
        ParallelLetterFrequency p = new ParallelLetterFrequency(input);

        assertThat(p.letterCounts()).isEqualTo(expectedOutput);
    }


    //@Ignore("Remove to run test")
    @Test
    public void biggerEmptyTextsStillReturnNoResults() {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < 10000; i++) {
            b.append(" ");
        }

        Map<Integer, Integer> expectedOutput = new HashMap<Integer, Integer>();
        ParallelLetterFrequency p = new ParallelLetterFrequency(b.toString());

        assertThat(p.letterCounts()).isEqualTo(expectedOutput);
    }

    //@Ignore("Remove to run test")
    @Test
    public void manyRepetitionsOfTheSameTextGiveAPredictableResult() {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < 10000; i++) {
            b.append("abc");
        }

        Map<Integer, Integer> expectedOutput = new HashMap<Integer, Integer>() {
            {
                put((int) 'a', 10000);
                put((int) 'b', 10000);
                put((int) 'c', 10000);
            }
        };
        ParallelLetterFrequency p = new ParallelLetterFrequency(b.toString());

        assertThat(p.letterCounts()).isEqualTo(expectedOutput);
    }


    //@Ignore("Remove to run test")
    @Test
    public void punctuationDoesntCount() {
        ParallelLetterFrequency p = new ParallelLetterFrequency(starSpangledBanner);

        assertThat(p.letterCounts().containsKey((int) ',')).isFalse();
    }

    //@Ignore("Remove to run test")
    @Test
    public void numbersDontCount() {
        ParallelLetterFrequency p = new ParallelLetterFrequency("Testing, 1, 2, 3");

        assertThat(p.letterCounts().containsKey((int) '1')).isFalse();
    }
}
