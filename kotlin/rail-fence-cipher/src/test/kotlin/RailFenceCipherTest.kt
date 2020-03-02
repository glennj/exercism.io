import org.junit.Ignore
import org.junit.Test
import kotlin.test.assertEquals

class RailFenceCipherTest {

    @Test
    fun encodeWithTwoRails() {
        val railFenceCipher = RailFenceCipher(2)
        assertEquals(
                "XXXXXXXXXOOOOOOOOO",
                railFenceCipher.getEncryptedData("XOXOXOXOXOXOXOXOXO")
        )
    }

   // @Ignore
    @Test
    fun encodeWithThreeRails() {
        val railFenceCipher = RailFenceCipher(3)
        assertEquals(
                "WECRLTEERDSOEEFEAOCAIVDEN",
                railFenceCipher.getEncryptedData("WEAREDISCOVEREDFLEEATONCE")
        )
    }

 //   @Ignore
    @Test
    fun encodeWithEndingInTheMiddle() {
        val railFenceCipher = RailFenceCipher(4)
        assertEquals(
                "ESXIEECSR",
                railFenceCipher.getEncryptedData("EXERCISES")
        )
    }

    //   @Ignore
    @Test
    fun encodeJackman() {
        val railFenceCipher = RailFenceCipher(5)
        assertEquals(
                "tkfshdhc  op te oeibnxmo  ygs urw uvrlzgeqojeai",
                railFenceCipher.getEncryptedData("the quick brown fox jumps over the lazy doggies")
        )
    }

  //  @Ignore
    @Test
    fun decodeWithThreeRails() {
        val railFenceCipher = RailFenceCipher(3)
        assertEquals(
                "THEDEVILISINTHEDETAILS",
                railFenceCipher.getDecryptedData("TEITELHDVLSNHDTISEIIEA")
        )
    }

  //  @Ignore
    @Test
    fun decodeWithFiveRails() {
        val railFenceCipher = RailFenceCipher(5)
        assertEquals(
                "EXERCISMISAWESOME",
                railFenceCipher.getDecryptedData("EIEXMSMESAORIWSCE")
        );
    }

  //  @Ignore
    @Test
    fun decodeWithSixRails() {
        val railFenceCipher = RailFenceCipher(6)
        assertEquals(
                "112358132134558914423337761098715972584418167651094617711286",
                railFenceCipher.getDecryptedData("133714114238148966225439541018335470986172518171757571896261")
        )
    }

    //   @Ignore
    @Test
    fun decodeJackman() {
        val railFenceCipher = RailFenceCipher(5)
        assertEquals(
                "the quick brown fox jumps over the lazy doggies",
                railFenceCipher.getDecryptedData("tkfshdhc  op te oeibnxmo  ygs urw uvrlzgeqojeai")
        )
    }
}
