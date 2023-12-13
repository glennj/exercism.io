function! Clock(hours, minutes) abort
  let clock = #{minutes: s:normalize(a:hours, a:minutes)}
  let clock.Add = function("s:clockAdd")
  let clock.Subtract = function("s:clockSubtract")
  let clock.ToString = function("s:clockToString")
  return clock
endfunction

" ------------------------------------------------------------
function! s:clockToString() dict abort
  return printf('%02d:%02d', self.minutes / 60, self.minutes % 60)
endfunction

function! s:clockAdd(mins) dict abort
  let self.minutes = s:normalize(0, self.minutes + a:mins)
endfunction

function! s:clockSubtract(mins) dict abort
  call self.Add(-a:mins)
endfunction

function! s:normalize(hrs, mins) abort
  let clock_mins = a:hrs * 60 + a:mins
  let day_mins = 24 * 60
  return (clock_mins % day_mins + day_mins) % day_mins
endfunction
