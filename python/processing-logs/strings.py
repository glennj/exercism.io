import re

Levels = ('INFO', 'WARN', 'ERROR')
EntryRegex = r'^\[(' + "|".join(Levels) + r')\]\s+(.+)'


def parse_log_entry(entry):
    m = re.match(EntryRegex, entry)
    if not m:
        raise ValueError("Can't parse log entry")
    return (m.group(1), m.group(2).strip())


def validate_level(level):
    if level.upper() not in Levels:
        raise ValueError(f'Invalid level: {level}')


def write_log_entry(level, entry):
    validate_level(level)
    return f'[{level.upper()}] {entry}'


def extract_message(entry):
    level, message = parse_log_entry(entry)
    return message


def change_log_level(entry, new_level):
    level, message = parse_log_entry(entry)
    return write_log_entry(new_level, message)


def reformat(entry):
    level, message = parse_log_entry(entry)
    return f'{message} ({level.lower()})'
