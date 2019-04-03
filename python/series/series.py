def slices(series, length):
    if series == "":
        raise ValueError('Series must not be empty.')
    if length <= 0:
        raise ValueError('Length must be positive.')
    if len(series) < length:
        raise ValueError('Length too long.')

    return [
        series[i:i+length]
        for i in range(len(series) - length + 1)
    ]
