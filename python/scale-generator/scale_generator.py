class Scale(object):
    CHROMATIC = {
        'sharps': ['A','A#','B','C','C#','D','D#','E','F','F#','G','G#'],
        'flats':  ['A','Bb','B','C','Db','D','Eb','E','F','Gb','G','Ab'],
    }

    FLATS = ['F','Bb','Eb','Ab','Db','Gb','d','g','c','f','bb','eb']

    INTERVAL = {'m': 1, 'M': 2, 'A': 3}

    def __init__(self, tonic):
        self.type = 'flats' if tonic in self.FLATS else 'sharps'
        self.tonic = tonic.capitalize()

    def chromatic(self):
        notes = self.CHROMATIC[self.type]
        idx = notes.index(self.tonic)
        return notes[idx:] + notes[:idx]

    def interval(self, intervals):
        notes = self.chromatic()
        idx = 0
        result = []
        for int in intervals:
            result.append(notes[idx])
            idx += self.INTERVAL[int]
        return result
