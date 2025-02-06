local xor = function(a, b)
  return (a and not b) or (b and not a)
end

local function parse_options(options)
  local i, l, n, v, x = true, false, false, false, false
  local patt = options.pattern
  
  for _, opt in ipairs(options.flags) do
    if     opt == '-i' then i = false; patt = patt:lower()
    elseif opt == '-l' then l = true
    elseif opt == '-n' then n = true
    elseif opt == '-v' then v = true
    elseif opt == '-x' then x = true      
    end
  end

  return {
    case_sensitive = i,
    filename_only  = l,
    line_numbers   = n,
    inverted       = v,
    multi_files    = #options.files > 1,
    pattern        = x and '^' .. patt .. '$' or patt,
  }
end

return function(options)
  local result = {}
  local opts = parse_options(options)

  local matches = function(line)
    local text = opts.case_sensitive and line or string.lower(line)
    return xor(opts.inverted, text:find(opts.pattern))
  end

  local output_line = function(line, filename, line_num)
    local bits = { line }
    if opts.line_numbers then table.insert(bits, 1, line_num) end
    if opts.multi_files  then table.insert(bits, 1, filename) end
    return table.concat(bits, ':')
  end
  
  for _, filename in ipairs(options.files) do
    local line_num = 0
    for line in io.lines(filename) do
      line_num = line_num + 1
      if matches(line) then
        if opts.filename_only then
          table.insert(result, filename)
          break
        end
        table.insert(result, output_line(line, filename, line_num))
      end
    end
  end
  
  return result
end
