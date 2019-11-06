class Strain {

    static Collection keep(Collection collection, Closure predicate) {
        def result = []
        collection.each {
            if (predicate(it)) 
                result.add(it)
        }
        return result
    }

    static Collection discard(Collection collection, Closure predicate) {
        return keep(collection, { ! predicate(it) })
    }
}
