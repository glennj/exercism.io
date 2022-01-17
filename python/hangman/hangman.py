# Game status categories
STATUS_WIN = "win"
STATUS_LOSE = "lose"
STATUS_ONGOING = "ongoing"


class Hangman(object):
    def __init__(self, word):
        self._guesses = set()
        self._mask = '_' * len(word)
        self.word = word
        self.remaining_guesses = 9
        self.status = STATUS_ONGOING

    def guess(self, char):
        if self.status != STATUS_ONGOING:
            raise ValueError('The game has already ended.')
        good_guess = char in self.word and \
                     char not in self._guesses
        self._guesses.add(char)
        if good_guess:
            self._mask = ''.join(
                char if char in self._guesses else '_'
                for char in self.word
            )
            if self._mask == self.word:
                self.status = STATUS_WIN
        else:
            self.remaining_guesses -= 1
            if self.remaining_guesses == -1:
                self.status = STATUS_LOSE

    def get_masked_word(self):
        return self._mask

    def get_status(self):
        return self.status
