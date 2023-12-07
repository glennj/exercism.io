function encode(s)
   replace(s, r"(.)\1+" => function(s)
      string(length(s)) * s[1]
   end)
end

function decode(s)
   replace(s, r"(\d+)(\D)"=> function(s)
      repeat(s[end], parse(Int, s[1:end-1]))
   end)
end
