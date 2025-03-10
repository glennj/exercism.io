import spock.lang.*

class RobotSimulatorSpec extends Specification {

    @Unroll
    def "Create robot at origin #label"() {
        when:
        def results = new RobotSimulator(x, y, direction)

        then:
        results.x == x
        results.y == y
        results.direction == direction

        where:
        label                               || x  | y  | direction
        "at origin facing north"            || 0  | 0  | "north"
        "at negative position facing south" || -1 | -1 | "south"
    }

    //@Ignore
    @Unroll
    def "Rotating clockwise #label"() {
        given:
        def results = new RobotSimulator(x1, y1, direction1)

        when:
        results.move("R")

        then:
        results.x == x2
        results.y == y2
        results.direction == direction2

        where:
        label                   || x1 | y1 | direction1 || x2 | y2 | direction2
        "changes north to east" || 0  | 0  | "north"    || 0  | 0  | "east"
        "changes east to south" || 0  | 0  | "east"     || 0  | 0  | "south"
        "changes south to west" || 0  | 0  | "south"    || 0  | 0  | "west"
        "changes west to north" || 0  | 0  | "west"     || 0  | 0  | "north"
    }

    //@Ignore
    @Unroll
    def "Rotating counter-clockwise #label"() {
        given:
        def results = new RobotSimulator(x1, y1, direction1)

        when:
        results.move("L")

        then:
        results.x == x2
        results.y == y2
        results.direction == direction2

        where:
        label                   || x1 | y1 | direction1 || x2 | y2 | direction2
        "changes north to west" || 0  | 0  | "north"    || 0  | 0  | "west"
        "changes west to south" || 0  | 0  | "west"     || 0  | 0  | "south"
        "changes south to east" || 0  | 0  | "south"    || 0  | 0  | "east"
        "changes east to north" || 0  | 0  | "east"     || 0  | 0  | "north"
    }

    @Unroll
    //@Ignore
    def "Moving forward one #label"() {
        given:
        def results = new RobotSimulator(x1, y1, direction)

        when:
        results.move("A")

        then:
        results.x == x2
        results.y == y2
        results.direction == direction

        where:
        label                       || x1 | y1 | direction || x2 | y2
        "facing north increments Y" || 0  | 0  | "north"   || 0  | 1
        "facing south decrements Y" || 0  | 0  | "south"   || 0  | -1
        "facing east increments X"  || 0  | 0  | "east"    || 1  | 0
        "facing west decrements X"  || 0  | 0  | "west"    || -1 | 0
    }

    //@Ignore
    @Unroll
    def "Follow series of instructions #label"() {
        given:
        def results = new RobotSimulator(x1, y1, direction1)

        when:
        results.move(commands)

        then:
        results.x == x2
        results.y == y2
        results.direction == direction2

        where:
        label                               || x1 | y1 | direction1 || x2 | y2 | direction2 || commands
        "moving east and north from README" || 7  | 3  | "north"    || 9  | 4  | "west"     || "RAALAL"
        "moving west and north"             || 0  | 0  | "north"    || -4 | 1  | "west"     || "LAAARALA"
        "moving west and south"             || 2  | -7 | "east"     || -3 | -8 | "south"    || "RRAAAAALA"
        "moving east and north"             || 8  | 4  | "south"    || 11 | 5  | "north"    || "LAAARRRALLLL"
    }
}
