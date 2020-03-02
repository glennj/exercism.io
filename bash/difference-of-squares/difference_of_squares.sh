#!/bin/bash
#
# inspired by @DERPSQUID9

abs() { echo $(( $1 < 0 ? -1 * $1 : $1 )); }

# sum: n(n+1)/2
square_of_sum=$(( ($2 * ($2 + 1) / 2) ** 2 ))

# n(n+1)(2n+1)/6
sum_of_squares=$(( $2 * ($2 + 1) * (2 * $2 + 1) / 6 ))

difference=$(abs $(( sum_of_squares - square_of_sum )))

case $1 in
    difference | sum_of_squares | square_of_sum) echo "${!1}" ;;
    *) echo "invalid argument: $1" >&2; exit 1 ;;
esac

### my original solution follows:
## main() {
##     case $1 in
##         square_of_sum | sum_of_squares | difference) "$@" ;;
##         *) echo "unknown subcommand $1" >&2 ; exit 1 ;;
##     esac
## }
## 
## square_of_sum() {
##     local n=$1 sum=0 i
##     for ((i=1; i<=n; i++)); do ((sum += i)); done
##     echo $((sum ** 2))
## }
## 
## sum_of_squares() {
##     local n=$1 sum=0 i
##     for ((i=1; i<=n; i++)); do ((sum += (i ** 2))); done
##     echo $sum
## }
## 
## difference() {
##     abs $(( $(sum_of_squares $1) - $(square_of_sum $1) ))
## }
## 
## main "$@"
