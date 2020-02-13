import kotlin.math.pow
import kotlin.math.round

fun Double.roundTo(places: Int): Double {
    val multiplier = 10.0.pow(places)
    return round(this * multiplier) / multiplier
}

class SpaceAge(private val ageInSeconds: Long) {

    enum class Planet(private val relativeOrbit: Double) {

        Mercury(  0.2408467 ),
        Venus  (  0.61519726),
        Earth  (  1.0       ),
        Mars   (  1.8808158 ),
        Jupiter( 11.862615  ),
        Saturn ( 29.447498  ),
        Uranus ( 84.016846  ),
        Neptune(164.79132   );

        private val secondsPerEarthYear = 31_557_600

        fun age(seconds: Long): Double =
            (seconds.toDouble() / secondsPerEarthYear / relativeOrbit).roundTo(2)
    }

    fun onMercury()  = ageOn(Planet.Mercury)
    fun onVenus()    = ageOn(Planet.Venus)
    fun onEarth()    = ageOn(Planet.Earth)
    fun onMars()     = ageOn(Planet.Mars)
    fun onJupiter()  = ageOn(Planet.Jupiter)
    fun onSaturn()   = ageOn(Planet.Saturn)
    fun onUranus()   = ageOn(Planet.Uranus)
    fun onNeptune()  = ageOn(Planet.Neptune)

    private fun ageOn(planet: Planet): Double = planet.age(ageInSeconds)
}
