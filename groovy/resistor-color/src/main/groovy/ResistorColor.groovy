class ResistorColor {

    static colors =  [
        'black', 'brown', 'red', 'orange', 'yellow', 
        'green', 'blue', 'violet', 'grey', 'white'
    ]

    static colorCode(color) {
        // returns -1 if color not found
        colors.findIndexOf { it == color }
    }

}
