"
" Calculates the constitution modifier using the passed ability score
"
function! Modifier(score) abort
  return float2nr(floor((a:score - 10) / 2.0))
endfunction

"
" Calculates an ability score randomly by summing the top three of four randomly generated numbers
"
function! Ability() abort
  let dice = range(4)->map({-> 1 + rand() % 6})
  return dice->reduce({sum, die -> sum + die}, 0) - min(dice)
endfunction

"
" Returns a dictionary representing a D&D character with randomly generated ability scores
"
function! Character() abort
  let character = {}
  for ability in ['strength', 'dexterity', 'constitution', 'intelligence', 'wisdom', 'charisma']
    let character[ability] = Ability()
  endfor
  let character.hitpoints = 10 + Modifier(character.constitution)
  lockvar! character
  return character
endfunction
