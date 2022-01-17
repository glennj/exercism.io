import datetime

GIGASECOND = datetime.timedelta(seconds=1e9)

def add(moment):
    return moment + GIGASECOND
