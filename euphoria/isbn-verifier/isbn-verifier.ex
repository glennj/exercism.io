include std/sequence.e

public function is_valid(sequence isbn)
    isbn = remove_all('-', isbn)

    if length(isbn) != 10 then
        return 0
    end if

    if isbn[$] = 'X' then
        isbn[$] = 10 + '0'
    end if
    isbn -= '0'

    integer sum = 0
    for i = 1 to length(isbn) - 1 do
        if isbn[1] < 0 or isbn[i] > 9 then
            return 0
        end if
        sum += isbn[i] * (11 - i)
    end for
    sum += isbn[10]

    return remainder(sum, 11) = 0
end function
