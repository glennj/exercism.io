sounds = ((3,"Pling"), (5,"Plang"), (7,"Plong"))

def convert(number):
    drops = [drop for factor, drop in sounds if number % factor == 0]
    return "".join(drops) if drops else str(number)
