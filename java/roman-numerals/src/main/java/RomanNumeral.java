public class RomanNumeral {
    private final String romanNumeral;

    RomanNumeral(int decimal) {
        StringBuilder roman = new StringBuilder();

        while (decimal >= 1000) { roman.append( "M"); decimal -= 1000; }
        if    (decimal >=  900) { roman.append("CM"); decimal -=  900; }
        if    (decimal >=  500) { roman.append( "D"); decimal -=  500; }
        if    (decimal >=  400) { roman.append("CD"); decimal -=  400; }
        while (decimal >=  100) { roman.append( "C"); decimal -=  100; }
        if    (decimal >=   90) { roman.append("XC"); decimal -=   90; }
        if    (decimal >=   50) { roman.append( "L"); decimal -=   50; }
        if    (decimal >=   40) { roman.append("XL"); decimal -=   40; }
        while (decimal >=   10) { roman.append( "X"); decimal -=   10; }
        if    (decimal >=    9) { roman.append("IX"); decimal -=    9; }
        if    (decimal >=    5) { roman.append( "V"); decimal -=    5; }
        if    (decimal >=    4) { roman.append("IV"); decimal -=    4; }
        while (decimal >=    1) { roman.append( "I"); decimal -=    1; }

        romanNumeral = roman.toString();
    }

    String getRomanNumeral() { return romanNumeral; }
}
