import re


def decode(string):
    return re.sub(
        r'(\d+)(.)',
        lambda m: m[2] * int(m[1]),
        string
    )


def encode(string):
    return re.sub(
        r'(.)\1+',
        lambda m: "{}{}".format(len(m[0]), m[1]),
        string
    )


# take 1:
#
# from functools import reduce
#
# def decode(string):
#     runs = re.findall(r'(\d*)(.)', string)
#     return "".join([decode_run(*r) for r in runs])
#
# def decode_run(count, char):
#     return char * (count and int(count) or 1)
#
# def encode(string):
#     runs = [r[0] for r in re.findall(r'((.)\2*)', string)]
#     return reduce(encoder, runs, '')
#
# def encoder(acc, run):
#     if len(run) == 1:
#         return acc + run
#     else:
#         return acc + "{}{}".format(len(run), run[0])
