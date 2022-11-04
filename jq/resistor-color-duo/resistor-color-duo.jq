def colorCode($color): 
  ["black","brown","red","orange","yellow","green","blue","violet","grey","white"]
  | index($color)
;

# .colors[0:2] | 10 * colorCode(first) + colorCode(last)

reduce .colors[0:2][] as $color (0; . * 10 + colorCode($color))
