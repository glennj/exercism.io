import java.util.Random

class DndCharacter {
    private static final ALL_CHARACTERISTICS = ['strength', 'dexterity', 'constitution',
                                                'intelligence', 'wisdom', 'charisma', 'hitpoints']
    private final characteristics
    private final rand

    DndCharacter() {
        this.rand = new Random()
        this.characteristics = [:]
        for (c in ALL_CHARACTERISTICS) this.characteristics[c] = ability()
        this.characteristics['hitpoints'] = 10 + modifier(this.characteristics['constitution'])
    }

    def getProperty(String property) {
        if (!(property in ALL_CHARACTERISTICS))
            throw new NoSuchFieldException("Unknown property ${property}")
        return this.characteristics[property]
    }

    def d6() { this.rand.nextInt(6) + 1 }

    def ability() {
        def sum = 0
        def min = Integer.MAX_VALUE
        4.times {
            def die = d6()
            sum += die
            min = Math.min(min, die)
        }
        return sum - min
    }

    def modifier(int value) { Math.floor((value - 10) / 2) }
}
