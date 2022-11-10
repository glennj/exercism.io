include "lib/resistorColor";

if .property == "colors" then colors
elif .property == "colorCode" then .input.color | colorCode
else empty
end
