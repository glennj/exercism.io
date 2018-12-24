class ReverseString {
    static reverse( text: string ): string {
        return Array.from(text).reverse().join('')
    }
}

export default ReverseString
