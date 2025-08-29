# These variables are initialized on the command line (using '-v'):
# - start, end

@include "assert"

BEGIN {
    assert(1 <= start && start <= 12, "invalid start")
    assert(1 <= end   && end   <= 12, "invalid end")

    item[1]  = "house";                            action[1]  = "Jack built"
    item[2]  = "malt";                             action[2]  = "lay in"
    item[3]  = "rat";                              action[3]  = "ate"
    item[4]  = "cat";                              action[4]  = "killed"
    item[5]  = "dog";                              action[5]  = "worried"
    item[6]  = "cow with the crumpled horn";       action[6]  = "tossed"
    item[7]  = "maiden all forlorn";               action[7]  = "milked"
    item[8]  = "man all tattered and torn";        action[8]  = "kissed"
    item[9]  = "priest all shaven and shorn";      action[9]  = "married"
    item[10] = "rooster that crowed in the morn";  action[10] = "woke"
    item[11] = "farmer sowing his corn";           action[11] = "kept"
    item[12] = "horse and the hound and the horn"; action[12] = "belonged to"

    for (i = start; i <= end; i++) verse(i)
}

function verse(n,     i) {
    printf "This is"
    for (i = n; i >= 1; i--) printf(" the %s that %s", item[i], action[i])
    print "."
}
