class ResistorColorDuo {

    static colors =  [
        'black', 'brown', 'red', 'orange', 'yellow', 
        'green', 'blue', 'violet', 'grey', 'white'
    ]
    
    /* from the requirements:
     * "The program will take two colors as input, and output
     * the correct number."
     */
    static int value(List<String> colorsInput) {
        10 * colors.indexOf(colorsInput.first()) +
             colors.indexOf(colorsInput.last())
    }
}
