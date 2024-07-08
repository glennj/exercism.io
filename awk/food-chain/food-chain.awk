# Variables declared on the command line
#       start
#       end

BEGIN {
    split("fly spider bird cat dog goat cow horse", animals)
    first = "I know an old lady who swallowed a %s.\n"
    chase = "She swallowed the %s to catch the %s%s.\n"
    phrase["fly"]    = "I don't know why she swallowed the fly. Perhaps she'll die."
    phrase["spider"] = "It wriggled and jiggled and tickled inside her."
    desc["spider"]   = " that wriggled and jiggled and tickled inside her"
    phrase["bird"]   = "How absurd to swallow a bird!"
    phrase["cat"]    = "Imagine that, to swallow a cat!"
    phrase["dog"]    = "What a hog, to swallow a dog!"
    phrase["goat"]   = "Just opened her throat and swallowed a goat!"
    phrase["cow"]    = "I don't know how she swallowed a cow!"
    phrase["horse"]  = "She's dead, of course!"

    for (v = start; v <= end; v++) {
        printf "%s", sep
        verse(v)
        sep = "\n"
    }
}

function verse(v,    animal, i) {
    animal = animals[v]
    printf first, animal
    if (animal != "fly") print phrase[animal]
    if (animal != "horse") {
        for (i = v; i > 1; i--) {
            predator = animals[i]
            prey = animals[i - 1]
            printf chase, predator, prey, desc[prey]
        }
        print phrase["fly"]
    }
}
