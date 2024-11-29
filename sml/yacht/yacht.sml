datatype category =
    Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht

local
  fun sortBy p lst =
    let fun insertSorted (item, []) = [item]
          | insertSorted (item, head::tail) =
              if p(item, head)
              then item::head::tail
              else head::(insertSorted (item, tail))
    in  List.foldl insertSorted [] lst
    end
  val sort = sortBy (fn (a, b) => a <= b)
  
  (* Scoring function *)
  fun single die dice = die * (length o (List.filter (fn d => d = die))) dice
  
  fun yacht dice =
    case sort dice
      of [a, _, _, _, b] => if a = b then 50 else 0
       | _               => raise Domain
  
  val sum = List.foldl op+ 0
  
  fun fullHouse dice =
    case sort dice 
      of [a, b, c, d, e] => if ((a = b andalso c = e) orelse (a = c andalso d = e)) andalso a <> e 
                            then sum dice 
                            else 0
       | _               => raise Domain
  
  fun fourOfAKind dice =
    case sort dice 
      of [a, b, c, d, e] => if a = d orelse b = e then 4 * c else 0
       | _               => raise Domain
  
  fun straight wanted dice = if wanted = sort dice then 30 else 0
  
  fun dispatch category: (int list -> int) =
    case category
      of Ones           => single 1 
       | Twos           => single 2 
       | Threes         => single 3 
       | Fours          => single 4 
       | Fives          => single 5 
       | Sixes          => single 6 
       | FullHouse      => fullHouse
       | FourOfAKind    => fourOfAKind
       | LittleStraight => straight [1,2,3,4,5]
       | BigStraight    => straight [2,3,4,5,6] 
       | Yacht          => yacht 
       | Choice         => sum 
in
  fun score (dice: int list, category): int = (dispatch category) dice
end