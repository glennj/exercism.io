module LuciansLusciousLasagna exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)


expectedMinutesInOven =
    40


minutesPerLayer =
    2


preparationTimeInMinutes numberOfLayers =
    minutesPerLayer * numberOfLayers


elapsedTimeInMinutes numberOfLayers timeInOven =
    preparationTimeInMinutes numberOfLayers + timeInOven
