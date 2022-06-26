#!/usr/bin/env gawk -f

{ for (i = 1; i <= NF; i++) $i = anslatetray($i) }
1

function anslatetray(ordway,     m) {
    if (match(ordway, "^(xr|yt).*"))            return ordway "ay"
    if (match(ordway, "^([^aeiou]*qu)(.*)", m)) return m[2] m[1] "ay"
    if (match(ordway, "^([^aeiou]+)(y.*)", m))  return m[2] m[1] "ay"
    if (match(ordway, "^([^aeiou]+)(.*)", m))   return m[2] m[1] "ay"
    return ordway "ay"
}