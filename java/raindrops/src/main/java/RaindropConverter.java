class RaindropConverter {

    String convert(int number) {
        StringBuilder sb = new StringBuilder();
        if (number % 3 == 0) {
            sb.append("Pling");
        }
        if (number % 5 == 0) {
            sb.append("Plang");
        }
        if (number % 7 == 0) {
            sb.append("Plong");
        }
        if (sb.length() == 0) {
            sb.append(String.valueOf(number));
        }
        return sb.toString();
    }
}

/* for further study

https://exercism.io/tracks/java/exercises/raindrops/solutions/2a8a78012de94eed95f82078449e4e93
https://exercism.io/tracks/java/exercises/raindrops/solutions/8833695c7d864d829c9db80fa79f6a3a

*/