from itertools import zip_longest


def transpose(input):
    '''
    The ASCII "file separator" is unlikely to appear
    in the input text. That character is used by `awk`
    as the default SUBSEP variable value.
    '''
    fill = chr(0o34)
    lines = input.splitlines()
    return "\n".join(
        "".join(row).rstrip(fill).replace(fill, " ")
        for row in zip_longest(*lines, fillvalue=fill)
    )

''' take 1

def transpose(input):
    lines = pad_lines(input.splitlines())
    return \
        '\n'.join(
            ''.join(
                line[i] if i < len(line) else ""
                for line in lines
            )
            for i in range(len(lines[0]))
        )


def pad_lines(lines):
    # a line cannot be _shorter_ than any _succeeding_ line
    width = len(lines[-1])
    for i in range(len(lines) - 2, -1, -1):
        lines[i] = lines[i].ljust(width)
        width = len(lines[i])
    return lines
'''
