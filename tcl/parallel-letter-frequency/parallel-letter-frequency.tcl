package require Thread

proc calculate {input} {
    # create 2 thread-safe variables:
    # * the list of lines in the input,
    #   each worker thread can pull work from it.
    # * the dictionary of the letter counts.
    #
    # First destroy the shared vars if they exist from a previous run
    catch {tsv::unset input}
    catch {tsv::unset count}

    tsv::set input lines $input
    tsv::array set count {}

    # create a pool for worker threads
    set poolid [tpool::create -initcmd {
        proc countOneLine {} {
            # Another thread may have grabbed the last line here.
            # No problem though, tsv::lpop returns an empty string
            # if the list is empty: we add nothing to the count.
            set line [tsv::lpop input lines]

            #puts [list [thread::id] $line]

            foreach char [split [string tolower $line] ""] {
                if {[string is alpha $char]} {
                    # frustratingly, tsv::incr is apparently not atomic
                    # See below for commentary
                    tsv::lock count {
                        tsv::incr count $char
                    }
                }
            }
        }
    }]

    # start working
    set jobs {}
    while {[tsv::llength input lines] > 0} {
        lappend jobs [tpool::post $poolid {countOneLine}]
    }

    # now that the input is consumed, wait for each job to complete
    while {[llength $jobs] > 0} {
        tpool::wait $poolid $jobs jobs
    }

    tsv::array get count
}


# Demonstrating need to lock the shared var for tsv::incr
#
# test without locking: test suite fails periodically
#
# $ for i in {1..1000}; do tclsh parallel-letter-frequency.test >/dev/null; echo $?; done | sort | uniq -c
#     944 0
#      56 1
#
# With locking (and shell timing thrown in)
#
# $ time for i in {1..1000}; do tclsh parallel-letter-frequency.test >/dev/null; echo $?; done | sort | uniq -c
#    1000 0
# 
# real	2m2.721s
# user	1m4.420s
# sys	1m3.705s
