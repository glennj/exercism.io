class HelloWorld {
/*
    static hello( ...name: string[] ): string {
        name.push('World')
        return `Hello, ${name[0]}!`
    }
*/
/*
    static hello( name?: string ): string {
        return `Hello, ${name || 'World'}!`
    }
*/
    static hello( name: string = 'World' ): string {
        return `Hello, ${name}!`
    }
}

export default HelloWorld
