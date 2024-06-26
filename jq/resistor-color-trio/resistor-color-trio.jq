include "lib/resistorColor";

.colors as [$band1, $band2, $band3]
| [$band1, $band2] | resistorValue($band3) | withUnits("ohms")

