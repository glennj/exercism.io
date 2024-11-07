module TwoBucket exposing (BucketNumber(..), measure)


type BucketNumber
    = One
    | Two


type alias Bucket =
    { name : BucketNumber
    , size : Int
    , amount : Int
    }


type alias State =
    { moves : Int, bucketOne : Int, bucketTwo : Int }



-- bucket functions


isFull : Bucket -> Bool
isFull b =
    b.amount == b.size


isEmpty : Bucket -> Bool
isEmpty b =
    b.amount == 0


fill : Bucket -> Bucket
fill b =
    { b | amount = b.size }


empty : Bucket -> Bucket
empty b =
    { b | amount = 0 }


pour : Bucket -> Bucket -> ( Bucket, Bucket )
pour src dest =
    let
        quantity =
            min src.amount (dest.size - dest.amount)
    in
    ( { src | amount = src.amount - quantity }
    , { dest | amount = dest.amount + quantity }
    )



-- validation functions


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (modBy b a)


isValid : Int -> Int -> Int -> Bool
isValid aSize bSize goal =
    let
        g =
            gcd aSize bSize
    in
    goal <= max aSize bSize && (g == 1 || modBy g goal == 0)



-- the main func


measure : Int -> Int -> Int -> BucketNumber -> Maybe State
measure bucketOneSize bucketTwoSize goal startBucket =
    if not (isValid bucketOneSize bucketTwoSize goal) then
        Nothing

    else
        let
            one =
                Bucket One bucketOneSize 0

            two =
                Bucket Two bucketTwoSize 0

            ( first, second ) =
                case startBucket of
                    One ->
                        ( fill one, two )

                    Two ->
                        ( fill two, one )

            moves =
                1
        in
        if second.size == goal && first.size /= goal then
            Just (solve first (fill second) goal (moves + 1))

        else
            Just (solve first second goal moves)



-- a recursive helper


solve : Bucket -> Bucket -> Int -> Int -> State
solve first second goal moves =
    if first.amount == goal || second.amount == goal then
        case first.name of
            One ->
                State moves first.amount second.amount

            Two ->
                State moves second.amount first.amount

    else if isEmpty first then
        solve (fill first) second goal (moves + 1)

    else if isFull second then
        solve first (empty second) goal (moves + 1)

    else
        let
            ( a, b ) =
                pour first second
        in
        solve a b goal (moves + 1)
