execute 'source' expand('<sfile>:p:h') . '/resistor_color.vim'

function! Value(colors) abort
  return ColorCode(a:colors[0]) * 10 + ColorCode(a:colors[1])
endfunction
