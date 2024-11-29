local
  val gigaSecond = Time.fromSeconds 1000000000

  fun monthOf m =
    case m
      of  1 => Date.Jan |  2 => Date.Feb |  3 => Date.Mar |  4 => Date.Apr
       |  5 => Date.May |  6 => Date.Jun |  7 => Date.Jul |  8 => Date.Aug
       |  9 => Date.Sep | 10 => Date.Oct | 11 => Date.Nov | 12 => Date.Dec
       |  _ => raise Domain

  fun parse s =
    let val bits = String.tokens (not o Char.isDigit) s
    in  case List.map (valOf o Int.fromString) bits
          of [Y, M, D, h, m, s] => (Y, monthOf M, D, h, m, s)
           | [Y, M, D]          => (Y, monthOf M, D, 0, 0, 0)
           | _ => raise Domain
    end

in
  fun add (moment: string): string =
    let val (Y, M, D, h, m, s) = parse moment
        val date   = Date.date { year = Y,   month = M,  day = D
                               , hour = h,   minute = m, second = s
                               , offset = SOME Time.zeroTime
                               }
        val future = Date.fromTimeUniv (gigaSecond + Date.toTime date)
    in  Date.fmt "%Y-%m-%dT%H:%M:%S" future
    end
end
