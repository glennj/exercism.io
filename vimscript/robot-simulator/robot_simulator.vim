const s:TURNS = #{
            \ R: #{north: 'east', east: 'south', south: 'west', west: 'north'},
            \ L: #{north: 'west', east: 'north', south: 'east', west: 'south'},
            \ }

" ------------------------------------------------------------
let s:robotPrototype = {}

function s:robotPrototype.R()
    let self.direction = s:TURNS['R'][self.direction]
endfunction

function s:robotPrototype.L()
    let self.direction = s:TURNS['L'][self.direction]
endfunction

function s:robotPrototype.A()
    if     self.direction == 'north' | let self.y += 1
    elseif self.direction == 'east'  | let self.x += 1
    elseif self.direction == 'south' | let self.y -= 1
    elseif self.direction == 'west'  | let self.x -= 1
    endif
endfunction

function s:robotPrototype.Move(instructions)
    for instruction in a:instructions->toupper()->split('\zs')
        let fn = 'self.' . instruction
        if !exists('*' . fn)
            throw 'Unknown instruction: ' . instruction
        endif
        execute 'call' fn . '()'
    endfor
endfunction

lockvar! s:robotPrototype

" ------------------------------------------------------------
function! Create(direction, x, y) abort
    let robot = s:robotPrototype->copy()
    let robot.direction = a:direction
    let robot.x = a:x
    let robot.y = a:y
    return robot
endfunction
