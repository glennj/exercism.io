def indexOf(element):
  def _indexOf:
    if .left > .right
      then "value not in array" | halt_error
    else
      ((.left + .right) / 2 | floor) as $mid
      | if .needle == .haystack[$mid]
          then $mid
        elif .needle < .haystack[$mid]
          then .right = $mid - 1 | _indexOf
          else .left  = $mid + 1 | _indexOf
        end
    end
  ;

  {haystack: ., needle: element, left: 0, right: (length - 1)} | _indexOf
;

.value as $elem
| .array 
| indexOf($elem)