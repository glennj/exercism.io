from collections import defaultdict
from functools import reduce


class School(object):
    def __init__(self):
        self.directory = {}
        self.was_added = []

    def add_student(self, name, grade):
        if name in self.directory:
            self.was_added.append(False)
        else:
            self.directory[name] = grade
            self.was_added.append(True)

    def roster(self):
        roster = []
        for grade in sorted(set(self.directory.values())):
            roster += self.grade(grade)
        return roster

    def grade(self, grade_number):
        names = [
            name for name, grade in self.directory.items()
                 if grade == grade_number
        ]
        return sorted(names)

    def added(self):
        return self.was_added
