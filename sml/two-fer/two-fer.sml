fun name (who: string option): string =
  let val moniker = getOpt (who, "you")
  in  "One for " ^ moniker ^ ", one for me."
  end

(* Refs:
 * https://smlfamily.github.io/Basis/option.html
 *)

(* iteration 0 not submitted: without types, recursive
 *
    fun name NONE = name (SOME "you")
      | name (SOME who) = "One for " ^ who ^ ", one for me."
 *)

(* iteration 1, recursive
 *
    fun name (who: string option): string =
      case who
        of SOME w => "One for " ^ w ^ ", one for me."
         | NONE   => name (SOME "you")
 *)
