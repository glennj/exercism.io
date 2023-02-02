module LuciansLusciousLasagna

let expectedMinutesInOven = 40

let remainingMinutesInOven timeInOven = expectedMinutesInOven - timeInOven

let preparationTimePerLayer = 2    // minutes
let preparationTimeInMinutes numLayers = numLayers * preparationTimePerLayer

let elapsedTimeInMinutes numLayers timeInOven =
    preparationTimeInMinutes numLayers + timeInOven