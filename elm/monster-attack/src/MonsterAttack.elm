module MonsterAttack exposing (..)


type alias MonsterDamage =
    String


strengths = {annalyn = 5, kazak = 1}


attackWithSword1 : MonsterDamage -> Int -> MonsterDamage
attackWithSword1 monsterDamage strength =
    monsterDamage ++ "Attacked with sword of strength " ++ String.fromInt strength ++ "."


attackWithClaw1 : MonsterDamage -> Int -> MonsterDamage
attackWithClaw1 monsterDamage strength =
    monsterDamage ++ "Attacked with claw of strength " ++ String.fromInt strength ++ "."


attack1 : MonsterDamage -> MonsterDamage
attack1 monsterDamage =
    {- first attempt, failed tests
       (attackWithSword1 monsterDamage strengths.annalyn)
       ++ (attackWithClaw1 monsterDamage strengths.kazak)
       ++ (attackWithClaw1 monsterDamage strengths.kazak)
       ++ (attackWithSword1 monsterDamage strengths.annalyn)
    -}
    {- second attempt
       (attackWithSword1 monsterDamage strengths.annalyn)
       ++ (attackWithClaw1 "" strengths.kazak)
       ++ (attackWithClaw1 "" strengths.kazak)
       ++ (attackWithSword1 "" strengths.annalyn)
    -}
    {- then, after cheating and finally grokking
          that each attack _builds_ on the previous string
       let
           first = annalynAttack monsterDamage strengths.annalyn
           second = kazakAttack first strengths.kazak
           third = kazakAttack second strengths.kazak
           last = annalynAttack third strengths.annalyn
       in
       last
    -}
    {- finally, a couple of helper functions -}
    let
        annalynAttack msg =
            attackWithSword1 msg strengths.annalyn

        kazakAttack msg =
            attackWithClaw1 msg strengths.kazak
    in
    monsterDamage
        |> annalynAttack
        |> kazakAttack
        |> kazakAttack
        |> annalynAttack


attackWithSword2 : Int -> MonsterDamage -> MonsterDamage
attackWithSword2 strength monsterDamage =
    attackWithSword1 monsterDamage strength


attackWithClaw2 : Int -> MonsterDamage -> MonsterDamage
attackWithClaw2 strength monsterDamage =
    attackWithClaw1 monsterDamage strength


attack2 : MonsterDamage -> MonsterDamage
attack2 monsterDamage =
    let
        annalynAttack =
            attackWithSword2 strengths.annalyn

        kazakAttack =
            attackWithClaw2 strengths.kazak
    in
    monsterDamage
        |> annalynAttack
        |> kazakAttack
        |> kazakAttack
        |> annalynAttack


attack3 : MonsterDamage -> MonsterDamage
attack3 =
    let
        annalynAttack =
            attackWithSword2 strengths.annalyn

        kazakAttack =
            attackWithClaw2 strengths.kazak
    in
    annalynAttack >> kazakAttack >> kazakAttack >> annalynAttack
