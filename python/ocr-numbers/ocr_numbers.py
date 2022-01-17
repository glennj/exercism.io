DIGIT_STRINGS = [
    ' _ | ||_|   ',  # 0
    '     |  |   ',  # 1
    ' _  _||_    ',  # 2
    ' _  _| _|   ',  # 3
    '   |_|  |   ',  # 4
    ' _ |_  _|   ',  # 5
    ' _ |_ |_|   ',  # 6
    ' _   |  |   ',  # 7
    ' _ |_||_|   ',  # 8
    ' _ |_| _|   ',  # 9
]

W, H = 3, 4


def convert(input_grid):
    # an OCR "line" is a list of 4 lines of text
    ocr_lines = validate(input_grid)
    return ','.join(
        scan_ocr_line(line)
        for line in ocr_lines
    )


def scan_ocr_line(line):
    digit_strings = [
        ''.join(l[i:i+W] for l in line)
        for i in range(0, len(line[0]), W)
    ]
    return ''.join(
        str(DIGIT_STRINGS.index(d)) if d in DIGIT_STRINGS else '?'
        for d in digit_strings
    )


def validate(grid):
    if len(grid) % H != 0:
        raise ValueError('Number of input lines is not a multiple of four')
    ocr_lines = [grid[i:i+H] for i in range(0, len(grid), H)]
    for lines in ocr_lines:
        if any(map(lambda x: len(x) % W != 0, lines)):
            raise ValueError(f'Number of input columns is not a multiple of three')
    return ocr_lines
