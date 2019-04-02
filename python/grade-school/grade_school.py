from collections import defaultdict
from functools import reduce


class School(object):
    def __init__(self):
        self.classes = defaultdict(list)

    def add_student(self, name, grade):
        self.classes[grade] = sorted(self.classes[grade] + [name])

    def roster(self):
        def build_student_body(sb, grade):
            return sb + self.grade(grade)

        return reduce(build_student_body, sorted(self.classes), [])

    def grade(self, grade_number):
        return self.classes[grade_number].copy()
