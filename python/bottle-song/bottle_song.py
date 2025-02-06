Ordinals = ['No', 'One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten']

def recite(start, take=1):
    song = verse(start)
    for i in range(start - 1, start - take, -1):
        song += ["", *verse(i)]
    return song

def verse(n):
    return [ f'{bottle(n)} hanging on the wall,',
             f'{bottle(n)} hanging on the wall,',
             'And if one green bottle should accidentally fall,',
             f"There'll be {bottle(n - 1, lower=True)} hanging on the wall." ]

def bottle(n, lower=False):
    nth = Ordinals[n]
    return f'{nth.lower() if lower else nth} green bottle{"" if n == 1 else "s"}'
