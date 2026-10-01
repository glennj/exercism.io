func two_fer(name := "you"):
    # return "One for " + name + ", one for me."
    # return "One for %s, one for me." % name
    return "One for {0}, one for me.".format([name])
