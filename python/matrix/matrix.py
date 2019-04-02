class Matrix(object):
    def __init__(self, matrix_string):
        self.rows = []
        self.cols = []
        for line in matrix_string.split('\n'):
            row = [int(n) for n in line.split()]
            self.rows.append(row)
        '''
        for i in range(len(self.rows[0])):
            col = [row[i] for row in self.rows]
            self.cols.append(col)
        '''
        self.cols = [list(col) for col in zip(*self.rows)]

    def row(self, index):
        return self.rows[index - 1]

    def column(self, index):
        return self.cols[index - 1]
