function! Matrix(string) abort
    let o = #{rows: a:string->split('\n')->map({_, row -> row->split()})}
    
    function o.Row(n)
        return self.rows[a:n - 1]
    endfunction
    
    function o.Column(n)
        " `copy` needed because `map` updates in-place
        return self.rows->copy()->map({_, row -> row[a:n - 1]})
    endfunction

    lockvar! o
    return o
endfunction
