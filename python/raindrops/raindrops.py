sounds = ((3,"Pling"), (5,"Plang"), (7,"Plong"))

def raindrops(number):
    '''
    sound = ""
    if number % 3 == 0: sound += "Pling"
    if number % 5 == 0: sound += "Plang"
    if number % 7 == 0: sound += "Plong"
    if sound == "": sound = str(number)
    return sound
    '''

    sound = "".join([s for f, s in sounds if number % f == 0])

    return sound if sound else str(number)
