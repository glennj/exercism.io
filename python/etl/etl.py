def transform(legacy_data):
    '''
    result = {}
    for score, words in legacy_data.items():
        for word in words:
            result[word.lower()] = score
    return result
    ''' 

    return {
        word.lower(): score
        for score, words in legacy_data.items()
        for word in words
    }
