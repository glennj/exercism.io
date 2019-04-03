from calendar import day_name, monthrange, weekday
from datetime import date


class MeetupDayException(Exception):
    pass


START_DAY = {
    '1st': 1,
    '2nd': 8,
    '3rd': 15,
    '4th': 22,
    '5th': 29,
    'teenth': 13,
    'last': None
}


def meetup_day(year, month, day_of_the_week, which):
    if which not in START_DAY:
        raise MeetupDayException(f'What is {which}?')

    if day_of_the_week not in day_name:
        raise MeetupDayException(f'Unknown day: {day_of_the_week}')

    start_day = START_DAY[which] or monthrange(year, month)[1] - 6

    # Both calendar.weekday() and datetime.date() can throw
    # ValueError: day is out of range for month
    try:
        start_day_wday = weekday(year, month, start_day)
        wanted_wday = list(day_name).index(day_of_the_week)
        delta = (wanted_wday - start_day_wday) % 7
        return date(year, month, start_day + delta)

    except ValueError as e:
        raise MeetupDayException(e)
