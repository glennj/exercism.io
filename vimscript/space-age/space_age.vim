const s:earth_years_per_planet_year = {
        \ 'Mercury':   0.2408467,
        \ 'Venus':     0.61519726,
        \ 'Earth':     1.0,
        \ 'Mars':      1.8808158,
        \ 'Jupiter':  11.862615,
        \ 'Saturn':   29.447498,
        \ 'Uranus':   84.016846,
        \ 'Neptune': 164.79132,
        \ }
const s:seconds_per_earth_year = 31557600

function! Age(planet, seconds) abort
    if !s:earth_years_per_planet_year->has_key(a:planet)
        throw 'not a planet'
    endif
    return a:seconds 
                \ / s:earth_years_per_planet_year[a:planet] 
                \ / s:seconds_per_earth_year
endfunction
