'''docstring'''

def round_scores(student_scores):
    """
    :param student_scores: list of student exam scores as float or int.
    :return: list of student scores *rounded* to nearest integer value.
    """

    # return [round(x) for x in student_scores]

    rounded = []
    for score in student_scores:
        rounded.append(round(score))
    return rounded


def count_failed_students(student_scores):
    """
    :param student_scores: list of integer student scores.
    :return: integer count of student scores at or below 40.
    """

    # return len([s for s in student_scores if s <= 40])

    fails = 0
    for score in student_scores:
        if score <= 40:
            fails += 1
    return fails


def above_threshold(student_scores, threshold):
    """
    :param student_scores: list of integer scores
    :param threshold :  integer
    :return: list of integer scores that are at or above the "best" threshold.
    """

    return [score for score in student_scores if score >= threshold]


def letter_grades(highest):
    """
    :param highest: integer of highest exam score.
    :return: list of integer score thresholds for each F-A letter grades.
    """

    increment = int((highest - 40) / 4)

    # return [41 + increment * i for i in range(4)]

    thresholds = [41]
    while len(thresholds) < 4:
        thresholds.append(thresholds[-1] + increment)
    return thresholds


def student_ranking(student_scores, student_names):
    """
     :param student_scores: list of scores in descending order.
     :param student_names: list of names in descending order by exam score.
     :return: list of strings in format ["<rank>. <student name>: <score>"].
     """

    # return [f'{i+1}. {student_names[i]}: {s}' for i, s in enumerate(student_scores)]

    ranking = []
    for i, score in enumerate(student_scores):
        rank = f'{i+1}. {student_names[i]}: {score}'
        ranking.append(rank)
    return ranking


def perfect_score(student_info):
    """
    :param student_info: list of [<student name>, <score>] lists
    :return: first `[<student name>, 100]` or `[]` if no student score of 100 is found.
    """

    #perfectos = [info for info in student_info if info[1] == 100]
    #return perfectos[0] if len(perfectos) > 0 else []

    for info in student_info:
        if info[1] == 100:
            return info
    return []
