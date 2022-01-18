class ResistorColorDuo {

    static colors =  [
        'black', 'brown', 'red', 'orange', 'yellow', 
        'green', 'blue', 'violet', 'grey', 'white'
    ]
    
    // only interested in first 2 elements
    static int value(List<String> colorsInput) {
        10 * colors.indexOf(colorsInput.pop()) +
             colors.indexOf(colorsInput.pop())
    }
}
