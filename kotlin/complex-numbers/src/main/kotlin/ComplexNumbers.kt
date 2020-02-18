import kotlin.math.*

data class ComplexNumber(val real: Double = 0.0, val imag: Double = 0.0) {

    operator fun plus(other: ComplexNumber): ComplexNumber =
        ComplexNumber(
            real + other.real,
            imag + other.imag
        )

    operator fun minus(other: ComplexNumber): ComplexNumber =
        ComplexNumber(
            real - other.real,
            imag - other.imag
        )

    operator fun times(other: ComplexNumber): ComplexNumber =
        ComplexNumber(
            real * other.real - imag * other.imag,
            imag * other.real + real * other.imag
        )

    operator fun div(other: ComplexNumber): ComplexNumber {
        val denom = other.real * other.real + other.imag * other.imag
        require(denom != 0.0)
        return ComplexNumber(
            (real * other.real + imag * other.imag) / denom,
            (imag * other.real - real * other.imag) / denom
        )
    }

    val abs: Double = hypot(real, imag)

    fun conjugate(): ComplexNumber = ComplexNumber(real, -1 * imag)
}

fun exponential(c: ComplexNumber): ComplexNumber =
    ComplexNumber(E.pow(c.real), 0.0) * ComplexNumber(cos(c.imag), sin(c.real))
