execute 'source' expand('<sfile>:p:h') . '/resistor_color.vim'
execute 'source' expand('<sfile>:p:h') . '/resistor_color_duo.vim'

" Returns a formatted label showing the encoded resistance value with units
"
function! Label(colors) abort
  let value = Value(a:colors[0:1]) * float2nr(pow(10, ColorCode(a:colors[2])))

  let prefixes = ["", "kilo", "mega", "giga"]
  let idx = 0

  while value > 0 && value % 1000 == 0
    let value /= 1000
    let idx += 1
  endwhile

  return printf("%d %sohms", value, prefixes[idx])
endfunction
