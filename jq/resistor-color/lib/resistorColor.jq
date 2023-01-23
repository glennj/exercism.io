############################################################
# `resistor-color` exercise
def colors: ["black","brown","red","orange","yellow","green","blue","violet","grey","white"];

def colorCode:
  . as $color
  | colors | index($color)
;

############################################################
# `resistor-color-duo` exercise
# - this is a 0-arity function
def resistorValue:
  10 * (.[0] | colorCode) + (.[1] | colorCode)
;

############################################################
# `resistor-color-trio` exercise
# - this is a 1-arity function
def resistorValue($powerColor):
  resistorValue * pow(10; $powerColor | colorCode)
;

# input is a numeric resistance value
def withUnits:
  def _withUnits($idx):
    if .value > 0 and .value % 1000 == 0
      then .value /= 1000 | _withUnits($idx + 1)
      else . + {unit: (["", "kilo", "mega", "giga"][$idx] + "ohms")}
    end
  ;
  {value: .} | _withUnits(0)
;
