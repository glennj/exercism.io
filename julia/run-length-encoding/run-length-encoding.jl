function encode(s)
   function shrink(s)
      len = length(s)
      num = len == 1 ? "" : string(len)
      num * s[1]
   end

   replace(s, r"(.)\1*" => shrink)
end

function decode(s)
   function expand(s)
      num = s[1:end-1]
      if isempty(num)
         string(s[end])
      else
         repeat(s[end], parse(Int, num))
      end
   end

   replace(s, r"(\d*)(\D)"=> expand)
end
