# These variables are initialized on the command line (using '-v'):
# - value

BEGIN { FS = "," }
{
    i = 1
    j = NF
    while (i <= j) {
        mid = int((i + j) / 2)
        if ($mid == value) {
            print mid
            next
        }
        if (value < $mid) j = mid - 1
        else              i = mid + 1
    }
    print -1
}