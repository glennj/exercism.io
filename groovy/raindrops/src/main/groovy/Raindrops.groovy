class Raindrops {

    def convert(num) {

        /*  Take 1
        def rain = ""
        if (num % 3 == 0) rain += "Pling"
        if (num % 5 == 0) rain += "Plang"
        if (num % 7 == 0) rain += "Plong"
        if (rain == "") rain = "${num}"
        return rain
        */

        // after reviewing top rated answers

        def map = [3: 'Pling', 5: 'Plang', 7: 'Plong']
        def rain = map.findResults {
            if (num % it.key == 0) it.value
        }
        rain.isEmpty() ? num as String : rain.join('')
    }
}
