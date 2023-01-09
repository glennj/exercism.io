def proverb(*items, qualifier):
    stanzas = [ f'For want of a {a} the {b} was lost.'
                for a, b
                in zip(items[:-1], items[1:]) ] 

    if len(items) > 0:
        item = (qualifier + " " if qualifier else "") + items[0]
        stanzas.append(f'And all for the want of a {item}.')

    return stanzas
