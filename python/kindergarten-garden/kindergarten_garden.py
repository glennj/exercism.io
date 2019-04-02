class Garden(object):
    PLANTS = {
        'C': 'Clover', 'G': 'Grass', 
        'R': 'Radishes', 'V': 'Violets'
    }

    STUDENTS = [
        'Alice', 'Bob', 'Charlie', 'David', 
        'Eve', 'Fred', 'Ginny', 'Harriet', 
        'Ileana', 'Joseph', 'Kincaid', 'Larry'
    ]

    def __init__(self, diagram, students=STUDENTS):
        # TODO validate input here

        self.patches = {}
        rows = diagram.split("\n")
        for i, student in enumerate(sorted(students)):
            a, b = 2*i, 2*(i+1)
            patch = "".join([row[a:b] for row in rows])
            plants = [self.PLANTS[p] for p in patch]
            self.patches[student] = plants

    def plants(self, student):
        return self.patches[student]
