class Accumulate {

    static accumulate(Collection collection, Closure func) {
        // The built-in answer:
        /*
        collection.collect(func)
        */

        // Spelled-out 
        Collection result = []
        collection.each { result.add( func.call( it )) }
        return result
    }

}
