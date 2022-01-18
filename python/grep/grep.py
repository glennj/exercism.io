import re


def grep(pattern, flags, files):
    match, output_fmt, opt_l = process_args(pattern, files, flags)
    results = []
    for file in files:
        nr = 0
        with open(file) as f:
            for line in f.readlines():
                nr += 1
                line = line.rstrip("\n")
                if match(line):
                    results.append(
                        output_fmt.format(F=file, N=nr, L=line)
                    )
                    if opt_l:
                        break

    return "".join(results)


def process_args(pattern, files, flags):
    opts = flags.split()

    # how to match the pattern against the line
    if '-x' in opts:
        pattern = f'^{pattern}$'
    
    re_opt = re.I if '-i' in opts else 0
    patt = re.compile(pattern, re_opt)
    match = lambda line: re.search(patt, line)

    if '-v' in opts:
        pos_match = match
        match = lambda line: not pos_match(line)

    # control the output:
    # F = filename, N = line number, L = line contents
    if '-l' in opts:
        fmt = "{F}"
    elif len(files) > 1:
        fmt = "{F}:{N}:{L}" if '-n' in opts else "{F}:{L}"
    else:
        fmt = "{N}:{L}" if '-n' in opts else "{L}"
    fmt += "\n"

    return match, fmt, '-l' in opts
