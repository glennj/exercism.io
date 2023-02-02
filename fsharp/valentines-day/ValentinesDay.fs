module ValentinesDay

type Approval = Yes | No | Maybe
type Cuisine  = Korean | Turkish
type Genre    = Crime | Horror | Romance | Thriller

type Activity = 
    | BoardGame
    | Chill
    | Movie of Genre
    | Restaurant of Cuisine
    | Walk of int

let rateActivity (activity: Activity): Approval = 
    match activity with
    | BoardGame -> No
    | Chill -> No 
    | Movie Romance -> Yes 
    | Movie _ -> No 
    | Restaurant Korean -> Yes 
    | Restaurant Turkish -> Maybe
    | Walk w when w < 3 -> Yes
    | Walk w when w < 5 -> Maybe
    | Walk _ -> No 
