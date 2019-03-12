local parse
local update_standings 
local get_team_idx
local win
local lose
local draw
local sort
local format

local tournament = function (...)
    return format(sort(parse(...)))
end

------------------------------------------------------------------------
parse = function(matches)
    local standings = {}
    local match_pattern = "^([^;]+);([^;]+);(.+)"

    for _,match in ipairs(matches) do
        local home, away, result = match:match(match_pattern)

        -- ignore any incorrect data
        if home and away and result     -- too few fields 
            and not result:find(";")    -- too many fields
            and home ~= away
            and (result == 'win' or result == 'loss' or result == 'draw')
        then
            update_standings(standings, home, away, result)
        end
    end
    return standings
end

update_standings = function (standings, home_name, away_name, result)
    local home = standings[get_team_idx(standings, home_name)]
    local away = standings[get_team_idx(standings, away_name)]

    if result == 'win' then
        win(home)
        lose(away)
    elseif result == 'loss' then
        win(away)
        lose(home)
    else
        draw(home)
        draw(away)
    end
end

get_team_idx = function(standings, name)
    local idx
    for i,team in ipairs(standings) do
        if team.name == name then
            idx = i
            break
        end
    end
    if not idx then
        -- we haven't seen this team yet
        idx = #standings + 1
        standings[idx] = {name=name, mp=0, wins=0, ties=0, lost=0, pts=0}
    end
    return idx
end

win = function (winner)
    winner.wins = winner.wins + 1
    winner.pts  = winner.pts  + 3
    winner.mp   = winner.mp   + 1
end

lose = function (loser)
    loser.lost = loser.lost + 1
    loser.mp   = loser.mp   + 1
end

draw = function (team)
    team.ties = team.ties + 1
    team.pts  = team.pts  + 1
    team.mp   = team.mp   + 1
end

------------------------------------------------------------------------
sort = function(standings)
    table.sort(standings, function(a, b)
        return a.pts > b.pts or (a.pts == b.pts and a.name < b.name)
    end)
    return standings
end

------------------------------------------------------------------------
format = function(standings)
    local fmt = "%-30s | %2d | %2d | %2d | %2d | %2d"
    local results = {
        'Team                           | MP |  W |  D |  L |  P',
    }
    for _,team in ipairs(standings) do
        results[#results+1] = fmt:format(
            team.name, team.mp, team.wins, team.ties, team.lost, team.pts
        )
    end
    return results
end


return tournament
