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

    drops = [drop for factor, drop in sounds if number % factor == 0]
    return "".join(drops) if drops else str(number)
