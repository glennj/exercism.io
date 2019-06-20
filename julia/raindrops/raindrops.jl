function raindrops(number::Int)
    sound = (m,s) -> number % m == 0 ? s : ""
    drops = join([
        sound(3, "Pling"),
        sound(5, "Plang"),
        sound(7, "Plong"),
    ])
    isempty(drops) ? string(number) : drops
end
