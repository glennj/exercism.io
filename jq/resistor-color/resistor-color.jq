["black","brown","red","orange","yellow","green","blue","violet","grey","white"] as $colors
| if .property == "colors" then $colors
  elif .property == "colorCode" then .input.color as $c | $colors | index($c)
  else empty
  end