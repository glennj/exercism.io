public function colors()
     return {
        "black", "brown", "red", "orange", "yellow",
        "green", "blue", "violet", "grey", "white"
    }
end function

public function colorCode( sequence color )
    return find(color, colors()) - 1
end function
