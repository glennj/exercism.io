ACTIONS = ['wink', 'double blink', 'close your eyes', 'jump', 'reverse']


def handshake(code):
    actions = []
    for i, a in enumerate(ACTIONS):
        if code & (1 << i) != 0:
            actions.append(a)
    if 'reverse' in actions:
        actions = actions[-2::-1]
    return actions


def secret_code(actions):
    codes = [(1 << ACTIONS.index(a)) for a in actions]
    if len(codes) > 1 and codes[0] > codes[1]:
        codes.append(1 << ACTIONS.index('reverse'))
    return sum(codes)
