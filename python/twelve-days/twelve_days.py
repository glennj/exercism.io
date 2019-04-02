DAYS = [
    None,
    ('first',    'a Partridge in a Pear Tree'),
    ('second',   'two Turtle Doves'),
    ('third',    'three French Hens'),
    ('fourth',   'four Calling Birds'),
    ('fifth',    'five Gold Rings'),
    ('sixth',    'six Geese-a-Laying'),
    ('seventh',  'seven Swans-a-Swimming'),
    ('eighth',   'eight Maids-a-Milking'),
    ('ninth',    'nine Ladies Dancing'),
    ('tenth',    'ten Lords-a-Leaping'),
    ('eleventh', 'eleven Pipers Piping'),
    ('twelfth',  'twelve Drummers Drumming')
]

FMT = "On the {} day of Christmas my true love gave to me: {}."


def recite(start_verse, end_verse):
    return [verse(i) for i in range(start_verse, end_verse+1)]


def verse(n):
    gifts = [day[1] for day in DAYS[n:0:-1]]
    if n > 1:
        gifts.append(f"and {gifts.pop()}")
    return FMT.format(DAYS[n][0], ", ".join(gifts))
