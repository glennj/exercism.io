module MariosMarvellousLasagna exposing (remainingTimeInMinutes)


remainingTimeInMinutes : Int -> Int -> Int
remainingTimeInMinutes numberOfLayers timeInOven =
    let
        expectedMinutesInOven =
            40

        prepTimePerLayer =
            2

        preparationTimeInMinutes =
            prepTimePerLayer * numberOfLayers
    in
    preparationTimeInMinutes + expectedMinutesInOven - timeInOven
