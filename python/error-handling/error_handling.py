def handle_error_by_throwing_exception():
    raise NotImplementedError('an Exception subclass')


def handle_error_by_returning_none(input_data):
    result = None
    if input_data.isdecimal():
        result = int(input_data)
    return result


def handle_error_by_returning_tuple(input_data):
    result, data = False, None
    if input_data.isdecimal():
        result = True
        data = int(input_data)
    return result, data


def filelike_objects_are_closed_on_exception(filelike_object):
    '''
    try:
        filelike_object.open()
        filelike_object.do_something()
    finally:
        filelike_object.close()
    '''
    with filelike_object as f:
        f.do_something()
