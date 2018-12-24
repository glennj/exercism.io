class TwelveDays {
    private static gifts: string[] = [
        'a Partridge in a Pear Tree', 'two Turtle Doves',
        'three French Hens',          'four Calling Birds',
        'five Gold Rings',            'six Geese-a-Laying',
        'seven Swans-a-Swimming',     'eight Maids-a-Milking',
        'nine Ladies Dancing',        'ten Lords-a-Leaping',
        'eleven Pipers Piping',       'twelve Drummers Drumming',
    ]

    private static ordinals: string[] = [
        'first', 'second', 'third', 'fourth', 'fifth', 'sixth',
        'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth',
    ]

    static verse(n: number): string {
        const gifts = this.gifts.slice(1, n).reverse().join(', ')
        const and = n > 1 ? ', and ' : ''
        return `On the ${this.ordinals[n - 1]} day of Christmas my true love gave to me: `
             + `${gifts}${and}${this.gifts[0]}.\n`
    }

    static recite(from: number, to: number): string {
        const verseNums = new Array(to - from + 1).fill(0).map((_, i) => i + from)
        return verseNums.map((i) => this.verse(i)).join('')
    }
}

export default TwelveDays
