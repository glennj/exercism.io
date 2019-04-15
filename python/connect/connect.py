class ConnectGame:
    def __init__(self, board):
        if len(board) == 0:
            raise ValueError
        self.board = [
            row.strip().split()
            for row in board.splitlines()
        ]

    def get_winner(self):
        # the board gets modified while checking, so copy it
        if self.is_winner("O", [row[:] for row in self.board]):
            return "O"
        if self.is_winner("X", transpose(self.board)):
            return "X"
        return ""

    def is_winner(self, player, board):
        # players in the top row
        stack = [
            (0, c)
            for c, piece in enumerate(board[0])
            if piece == player
        ]
        if len(stack) == 0:
            return False
        if len(board) == 1:
            return True

        last_row_idx = len(board) - 1
        while len(stack) > 0:
            r, c = stack.pop()
            for rr, cc in self.neighbours(player, board, r, c):
                if rr == last_row_idx:
                    return True
                board[r][c] = 'seen'   # prevent backtracking
                stack.append((rr, cc))
        return False

    def neighbours(self, player, board, r, c):
        for dr in range(-1, 2):
            for dc in range(-1, 2):
                if (
                    dr != dc
                    and 0 <= r+dr < len(board)
                    and 0 <= c+dc < len(board)
                    and board[r+dr][c+dc] == player
                ):
                    yield r+dr, c+dc


def transpose(matrix):
    '''
    need to add a couple of layers of list() in here because
    `zip(*matrix)` returns an array of _tuples_. I need
    a mutable list
    '''
    return list(map(list, zip(*matrix)))
