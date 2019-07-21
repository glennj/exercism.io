class RunLengthEncoding {

    static encode(input) {
        /*
        input.replaceAll('(.)\\1*') {
            def len = it[0].length()
            (len == 1 ? '' : len as String) + it[1]
        }
        */
        input.replaceAll('(.)\\1+') {
            "${it[0].length()}${it[1]}"
        }
    }

    static decode(input) {
        /*
        input.replaceAll('(\\d*)(.)') {
            it[2] * (it[1] == '' ? 1 : it[1] as int)
        }
        */
        input.replaceAll('(\\d+)(.)') {
            it[2] * (it[1] as int)
        }
    }
}
