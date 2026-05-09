local function normalize_angle(a)
    return math.fmod(360 + math.fmod(a, 360), 360)
end

local function is_inline(line, prism)
    -- determine the perpendicular distance of the point from the line
    local dx, dy = prism.x - line.x, prism.y - line.y
    local dist = dx * math.sin(line.theta) - dy * math.cos(line.theta)
    -- if it's on the line, dist should be zero
    -- check it against some epsilon, found by trial and error
    return math.abs(dist) <= 0.0011
end

local function next_prism(line, prisms)
    local nxt = nil
    local min = math.huge
    for _, p in ipairs(prisms) do
        local dx, dy = p.x - line.x, p.y - line.y
        local projection = dx * math.cos(line.theta) + dy * math.sin(line.theta)
        if projection > 0 and projection < min then
            min = projection
            nxt = p
        end
    end
    return nxt
end


local function find_sequence(start, prisms)
    local line, seq = start, {}
    while true do
        line.angle = normalize_angle(line.angle)
        line.theta = math.rad(line.angle)
        
        local inline_prisms = {}
        for _, p in ipairs(prisms) do
            if is_inline(line, p) then
                table.insert(inline_prisms, p)
            end
        end

        local p = next_prism(line, inline_prisms)
        if not p then break end

        table.insert(seq, p.id)
        line = {x = p.x, y = p.y, angle = line.angle + p.angle}
    end
    return seq
end

return { find_sequence = find_sequence }
