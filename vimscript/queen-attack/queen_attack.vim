const s:BOARD_SIZE = 8

function! Queen(row, column) abort
    if a:row < 0                | throw 'row not positive'    | endif
    if a:column < 0             | throw 'column not positive' | endif
    if a:row >= s:BOARD_SIZE    | throw 'row not on board'    | endif
    if a:row >= s:BOARD_SIZE    | throw 'row not on board'    | endif
    if a:column >= s:BOARD_SIZE | throw 'column not on board' | endif

    let q = #{r: a:row, c: a:column}

    function q.CanAttack(other)
        let dr = self.r - a:other.r
        let dc = self.c - a:other.c
        return dr == 0 || dc == 0 || abs(dr) == abs(dc)
    endfunction

    lockvar! q
    return q
endfunction
