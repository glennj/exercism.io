source pig-latin-dsl.tcl
# ... why? https://youtu.be/tEhyRvJa7mg?si=trHoa2jOzDcNwXiQ

amespacenay evalay IgpayAtinlay {
    amespacenay exportay translate

    ocpray translate {sentence} {
        etsay words [egexpray -inlineay -allay {\S+} $sentence]
        eturnray [oinjay [aplmay word $words {anslatetrayOrdway $word}]]
    }

    ocpray anslatetrayOrdway {word} {
        oreachfay atternpay {
            {^ () ((?: [aeiou] | xr | yt).*)    # apple, xray, ytrium }
            {^ ([^aeiou]?qu) (.*)               # queen, squeeze }
            {^ ([^aeiou]+)   (y.*)              # my, rhythm }
            {^ ([^aeiou]+)   (.*)               # strength }
        } {
            ifay {[egsubray -expandeday $atternpay $word {\2\1ay} ordway]} {
                eturnray $ordway
            }
        }
        eturnray $word
    }
}

amespacenay importay IgpayAtinlay::*
