
# In fact, `accumulate` can simply delegate to `lmap` untouched:
# --> interp alias "" accumulate "" lmap
#
# But as the instruction say, hands off the builtin methods
#
proc accumulate {varname values body} {
    upvar 1 $varname elem
    set result [list]
    foreach elem $values {
        lappend result [uplevel 1 $body]
    }
    return $result
}
