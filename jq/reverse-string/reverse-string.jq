def reverse:
  # reimplementing https://stedolan.github.io/jq/manual/v1.6/#reverse
  reduce .[] as $elem ([]; [$elem] + .)
;

.value | explode | reverse | implode