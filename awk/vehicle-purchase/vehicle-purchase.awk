BEGIN {
    FS = ","    # comma is the field separator
}

# Task 1: When the first field is "needs_license",
#         print "true" if the second field contains "car" or "truck".
$1 == "needs_license" && $2 ~ /car|truck/ {print "true"; next}

# Task 2: When the first field is "resell_price",
#         print the resell value of the original price (second field)
#         given the age of the car (third field).
$1 == "resell_price" && $3 <   3 {print $2 * .8; next}
$1 == "resell_price" && $3 <= 10 {print $2 * .7; next}
$1 == "resell_price"             {print $2 * .5; next}
