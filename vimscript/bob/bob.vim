function! Response(remark) abort
    let rem = substitute(a:remark, '[[:space:]]', '', 'g')

    let is_silent   = len(rem) == 0
    let is_question = rem =~ '[?]$'
    let is_yelling  = rem =~# '\u' && rem !~# '\l'

    if is_question && is_yelling | return 'Calm down, I know what I''m doing!' 
    elseif is_question           | return 'Sure.' 
    elseif is_yelling            | return 'Whoa, chill out!' 
    elseif is_silent             | return 'Fine. Be that way!' 
    else                         | return 'Whatever.'
    endif

endfunction
