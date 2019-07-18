class Darts {

    static int score(x, y) {
        def dist = Math.hypot(x, y)
        if (dist <=  1) return 10
        if (dist <=  5) return 5
        if (dist <= 10) return 1
        return 0
    }
}

/*
// interesting community solution    
// https://exercism.io/tracks/groovy/exercises/darts/solutions/f23bdd357e7e4d4b8d689863ef76075c

        class Darts {

            class Circle {
                String name
                int innerRadius
                int outerRadius
                int val

                def contains(x, y) {
                    ((x*x) + (y*y) <= (outerRadius * outerRadius)) && ((x*x) + (y*y) >= (innerRadius * innerRadius))
                }
            }

            def circles = [
                new Circle(name: "inner", innerRadius: 0, outerRadius: 1, val: 10),
                new Circle(name: "middle", innerRadius: 1, outerRadius: 5, val: 5),
                new Circle(name: "outer", innerRadius: 5, outerRadius: 10, val: 1),
            ]

            static int score(x, y) {
                def darts = new Darts()
                def circle = darts.circles.find { it.contains(x,y) }

                circle?.val ?: 0
            }
        }

*/
