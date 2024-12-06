fun answer (question: string): int option =
  case String.tokens Char.isSpace question
    of "What"::"is"::words => getNumber words 0 op+
     | _                   => NONE

and getNumber (words: string list) (acc: int) (operation: int * int -> int): int option =
  case words
    of []        => NONE (* can't end without a last number *)
     | (num::ws) =>
        case Int.fromString num
          of NONE   => NONE (* expecting a number here *)
           | SOME n => getOperator ws (operation (acc, n))

and getOperator (words: string list) (acc: int): int option =
  case words
    of []        => SOME acc (* the answer *)
     | [_]       => NONE (* can't end with an operation *)
     | (op_::ws) =>
        case op_
          of "plus"       => getNumber ws acc op+
           | "minus"      => getNumber ws acc op-
           | "multiplied" => getNumber (tl ws) acc op*
           | "divided"    => getNumber (tl ws) acc (op div)
           | _            => NONE (* unknown operator *)
