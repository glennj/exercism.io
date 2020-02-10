/* a hacky ternary function inspired by
 * https://stackoverflow.com/questions/16336500/kotlin-ternary-conditional-operator
 *
 * I'm passing 2 lambdas to delay evaluation until we decide which is needed
 */
fun <T> Boolean.ifelse(a: () -> T, b: () -> T): T = if (this) a() else b()

object HandshakeCalculator {

    private val signals = Signal.values()

    fun calculateHandshake(number: Int): List<Signal> {
        val actions = signals.filter { (number and (1 shl it.ordinal)) != 0 }
        return (number < (1 shl signals.size))
                .ifelse({ actions }, { actions.reversed() })
    }
}

// note to self: `shl` => shift left
