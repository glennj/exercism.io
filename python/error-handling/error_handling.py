def handle_error_by_throwing_exception():
    raise NotImplementedError('an Exception subclass')


def handle_error_by_returning_none(input_data):
    result = None
    if input_data.isdecimal():
        result = int(input_data)
    return result
    '''
    #or:

    try:
        return int(input_data)
    except ValueError:
        pass
    '''


def handle_error_by_returning_tuple(input_data):
    result, data = False, None
    if input_data.isdecimal():
        result = True
        data = int(input_data)
    return result, data
    '''
    # Nice implementation
    # https://exercism.io/tracks/python/exercises/error-handling/solutions/eaaac7ba838c49ecbdf55292de990242

    result = handle_error_by_returning_none(input_data)
    return (result is not None, result)
    '''


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
