#!/usr/bin/env gawk -f

BEGIN {
    # As discussed at:
    #   https://www.gnu.org/software/gawk/manual/html_node/gawk-split-records.html 
    # for the context of the `RS` variable, `^` and `$` match at the
    # beginning and end of the __file__! So the record separator "^$" can
    # only match for empty files. Therefore, for non-empty files, no record
    # separator is found and the whole file is a single record.
    RS = "^$"
}

END {
    gsub(/[[:space:]]/, "")
    if ($0 == "")
        print "Fine. Be that way!"
    else {
        yelling = /[A-Z]/ && !/[a-z]/
        asking  = /\?$/
        switch (yelling "" asking) {
            case "11": print "Calm down, I know what I'm doing!"; break
            case "10": print "Whoa, chill out!"; break
            case "01": print "Sure."; break
            case "00": print "Whatever."; break
        }
    }
}

