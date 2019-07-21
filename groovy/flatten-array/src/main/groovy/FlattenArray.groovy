class FlattenArray {
    def flatten(input) {
        input.inject([]) { result, elem ->
            if (elem != null) {
                if (elem instanceof List) {
                    flatten(elem).each {result << it}
                } else {
                    result << elem
                }
            }
            result
        }
    }
}
