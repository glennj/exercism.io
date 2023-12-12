function! Equilateral(triangle) abort
    return s:IsTriangle(a:triangle) && s:NumEqualPairs(a:triangle) == 3
endfunction

function! Isosceles(triangle) abort
    return s:IsTriangle(a:triangle) && s:NumEqualPairs(a:triangle) > 0
endfunction

function! Scalene(triangle) abort
    return s:IsTriangle(a:triangle) && s:NumEqualPairs(a:triangle) == 0
endfunction


function! s:IsTriangle(triangle)
    let [a, b, c] = a:triangle->copy()->sort('f')
    return a > 0 && a + b > c
endfunction

function! s:NumEqualPairs(triangle)
    return (a:triangle[0] == a:triangle[1])
       \ + (a:triangle[0] == a:triangle[2])
       \ + (a:triangle[1] == a:triangle[2])
endfunction
