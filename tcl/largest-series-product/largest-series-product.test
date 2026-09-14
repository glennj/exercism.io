#!/usr/bin/env tclsh
# generated: 2026-07-18T02:04:14Z
package require tcltest
namespace import ::tcltest::*
source testHelpers.tcl

# Uncomment next line to view test durations.
#configure -verbose {body error usec}

############################################################
source "largest-series-product.tcl"


test largest-series-product-1 "finds the largest product if span equals length" -body {
    largestSeriesProduct "29" 2
} -returnCodes ok -result 18

skip largest-series-product-2
test largest-series-product-2 "can find the largest product of 2 with numbers in order" -body {
    largestSeriesProduct "0123456789" 2
} -returnCodes ok -result 72

skip largest-series-product-3
test largest-series-product-3 "can find the largest product of 2" -body {
    largestSeriesProduct "576802143" 2
} -returnCodes ok -result 48

skip largest-series-product-4
test largest-series-product-4 "can find the largest product of 3 with numbers in order" -body {
    largestSeriesProduct "0123456789" 3
} -returnCodes ok -result 504

skip largest-series-product-5
test largest-series-product-5 "can find the largest product of 3" -body {
    largestSeriesProduct "1027839564" 3
} -returnCodes ok -result 270

skip largest-series-product-6
test largest-series-product-6 "can find the largest product of 5 with numbers in order" -body {
    largestSeriesProduct "0123456789" 5
} -returnCodes ok -result 15120

skip largest-series-product-7
test largest-series-product-7 "can get the largest product of a big number" -body {
    largestSeriesProduct "73167176531330624919225119674426574742355349194934" 6
} -returnCodes ok -result 23520

skip largest-series-product-8
test largest-series-product-8 "reports zero if the only digits are zero" -body {
    largestSeriesProduct "0000" 2
} -returnCodes ok -result 0

skip largest-series-product-9
test largest-series-product-9 "reports zero if all spans include zero" -body {
    largestSeriesProduct "99099" 3
} -returnCodes ok -result 0

skip largest-series-product-10
test largest-series-product-10 "rejects span longer than string length" -body {
    largestSeriesProduct "123" 4
} -returnCodes error -result "span must not exceed string length"

skip largest-series-product-11
test largest-series-product-11 "rejects empty string and nonzero span" -body {
    largestSeriesProduct "" 1
} -returnCodes error -result "span must not exceed string length"

skip largest-series-product-12
test largest-series-product-12 "rejects invalid character in digits" -body {
    largestSeriesProduct "1234a5" 2
} -returnCodes error -result "digits input must only contain digits"

skip largest-series-product-13
test largest-series-product-13 "rejects negative span" -body {
    largestSeriesProduct "12345" -1
} -returnCodes error -result "span must not be negative"


cleanupTests
