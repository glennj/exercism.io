use "resistor-color.sml";

fun value (colors: string list): int =
  case colors
    of c1::c2::_ => 10 * (colorCode c1) + (colorCode c2)
     | _         => raise Fail "input requires two colors"
