function! HighScores(scores) abort
    let o = {'scores': a:scores}

    function o.Scores()
        return self.scores
    endfunction

    function o.Latest()
        return self.scores[-1]
    endfunction

    function o.PersonalBest()
        return self.scores->max()
    endfunction

    function o.PersonalTopThree()
        return self.scores
                    \ ->copy()
                    \ ->sort({a, b -> b - a})
                    \ ->slice(0,3)
    endfunction

    return o
endfunction
