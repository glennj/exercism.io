local function parse(input)
    local markup = ''
    local in_list = false

    local open_list = function() 
        if not in_list then
            in_list = true
            markup = markup .. '<ul>'
        end
    end

    local close_list = function() 
        if in_list then
            in_list = false
            markup = markup .. '</ul>'
        end
    end

    for line in input:gmatch('[^\n]+') do
        -- handle the in-line markup
        line = string.gsub(line, '__(.-)__', '<strong>%1</strong>')
        line = string.gsub(line, '_(.-)_', '<em>%1</em>')

        -- is it a list item?
        if line:sub(1,1) == '*' then
            open_list()
            markup = markup .. '<li>' .. line:sub(3) .. '</li>'

        else
            close_list()

            -- is it a heading?
            local text, header
            header, text = line:match('^(#+)%s+(.+)')
            if header and #header <= 6 then
                local tag = 'h' .. #header
                markup = markup .. '<' .. tag .. '>' .. text .. '</' .. tag .. '>'

            -- it's a plain paragraph
            else
                markup = markup .. '<p>' .. line .. '</p>'
            end
        end
    end

    close_list()

    return markup
end

return {
    parse = parse
}
