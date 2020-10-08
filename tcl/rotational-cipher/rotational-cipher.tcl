proc rotate {text shift} {
    set alphabet {a b c d e f g h i j k l m n o p q r s t u v w x y z}
    set shifted [list \
        {*}[lrange $alphabet $shift end] \
        {*}[lrange $alphabet 0 $shift-1]]

    foreach a $alphabet s $shifted {
        lappend map $a $s [string toupper $a] [string toupper $s]
    }

    string map $map $text
}
