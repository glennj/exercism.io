// non-custom list functionality used: forEach, plus

// All the other custom functions are implemented with foldLeft
fun <T, U> List<T>.customFoldLeft(initial: U, f: (U, T) -> U): U {
    var result = initial
    this.forEach { result = f(result, it) }
    return result
}

fun <T> List<T>.customAppend(list: List<T>): List<T> =
    list.customFoldLeft(this) { acc, elem ->
        acc.plus(elem)
    }

// Thanks to @avocado-shrimp for the coercion syntax
// https://exercism.io/tracks/kotlin/exercises/list-ops/solutions/33c85ffa3436462789876690b3c5d60d
fun List<Any>.customConcat(): List<Any> =
    this.customFoldLeft(emptyList()) { acc, elem ->
        when (elem) {
            is List<*> -> acc.customAppend((elem as List<Any>).customConcat())
            else -> acc.plus(elem)
        }
    }

fun <T> List<T>.customFilter(predicate: (T) -> Boolean): List<T> =
    this.customFoldLeft(emptyList()) { acc, elem ->
        if (predicate(elem)) acc.plus(elem) else acc
    }

val List<Any>.customSize: Int
    get() = this.customFoldLeft(0) { size, _ -> size + 1}

fun <T, U> List<T>.customMap(transform: (T) -> U): List<U> =
    this.customFoldLeft(emptyList()) { acc, elem ->
        acc.plus(transform(elem))
    }

fun <T> List<T>.customReverse(): List<T> =
    this.customFoldLeft(emptyList()) { acc, elem ->
        listOf(elem).plus(acc)
    }

fun <T, U> List<T>.customFoldRight(initial: U, f: (T, U) -> U): U =
    this.customReverse()
        .customFoldLeft(initial) { acc, elem ->
            f(elem, acc)
        }
