ACTIONS = ['wink', 'double blink', 'close your eyes', 'jump', 'reverse']


def commands(code):
    code = int(code, 2)
    actions = []
    for i, a in enumerate(ACTIONS):
        if code & (1 << i) != 0:
            actions.append(a)
    if 'reverse' in actions:
        actions = actions[-2::-1]
    return actions
