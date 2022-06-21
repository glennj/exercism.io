@namespace "team"

function init(name) {
    Wins[name] = Losses[name] = Draws[name] = Points[name] = 0
}

function win(name)  { Wins[name]   += 1; Points[name] += 3 }
function draw(name) { Draws[name]  += 1; Points[name] += 1 }
function lose(name) { Losses[name] += 1 }

function wins(name)   { return Wins[name] }
function losses(name) { return Losses[name] }
function draws(name)  { return Draws[name] }
function points(name) { return Points[name] }
function played(name) { return Wins[name] + Losses[name] + Draws[name] }
