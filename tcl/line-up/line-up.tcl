proc formatTicket {name number} {
    switch -glob -- $number {
        *11 - *12 - *13 {set suffix th}
        *1              {set suffix st}
        *2              {set suffix nd}
        *3              {set suffix rd}
        default         {set suffix th}
    }

    format "%s, you are the %d%s customer we serve today. Thank you!" \
        $name \
        $number \
        $suffix
}
