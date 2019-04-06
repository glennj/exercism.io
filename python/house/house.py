from collections import namedtuple

Item = namedtuple('Item', ['name', 'that'], defaults=[''])

items = [
    Item(name="house that Jack built."),
    Item(name="malt", that="lay in"),
    Item(name="rat", that="ate"),
    Item(name="cat", that="killed"),
    Item(name="dog", that="worried"),
    Item(name="cow with the crumpled horn", that="tossed"),
    Item(name="maiden all forlorn", that="milked"),
    Item(name="man all tattered and torn", that="kissed"),
    Item(name="priest all shaven and shorn", that="married"),
    Item(name="rooster that crowed in the morn", that="woke"),
    Item(name="farmer sowing his corn", that="kept"),
    Item(name="horse and the hound and the horn", that="belonged to")
]


def _verse(n):
    text = f"This is the {items[n].name}"
    for i in range(n, 0, -1):
        text += f" that {items[i].that} the {items[i-1].name}"
    return text


def recite(start_verse, end_verse):
    return [_verse(i) for i in range(start_verse-1, end_verse)]
