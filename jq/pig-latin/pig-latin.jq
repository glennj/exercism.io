def xr_yt:
  if startswith("xr") or startswith("yt") then . + "ay" else null end
;

def by_regex(re):
  capture(re; "x") as $m
  | if isempty($m) then null else $m.tail + $m.head + "ay" end
;

def qu:         by_regex("^ (?<head> [^aeiou]*qu ) (?<tail>  .+ )");
def y:          by_regex("^ (?<head> [^aeiou]+   ) (?<tail> y.* )");
def consonants: by_regex("^ (?<head> [^aeiou]+   ) (?<tail>  .* )");

def translate:
  xr_yt // qu // y // consonants // . + "ay"
;

.phrase | split(" ") | map(translate) | join(" ")
