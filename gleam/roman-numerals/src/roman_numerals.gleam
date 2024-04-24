pub fn convert(number: Int) -> String {
  do_convert(number, "")
}

fn do_convert(n: Int, roman: String) -> String {
  let add_digit = fn(base: Int, digits: #(String, String, String)) -> String {
    // Hmm, not working
    // case n {
    //   m if m >= 10 * base -> do_convert(n - 10 * base, roman <> digits.0)
    //   m if m >=  9 * base -> do_convert(n +  1 * base, roman <> digits.2)
    //   m if m >=  5 * base -> do_convert(n -  5 * base, roman <> digits.1)
    //   m if m >=  4 * base -> do_convert(n +  1 * base, roman <> digits.2)
    //   _                   -> do_convert(n -  1 * base, roman <> digits.2)
    // }
    case {n >= 10 * base}, {n >= 9 * base}, {n >= 5 * base}, {n >= 4 * base} {
      True, _, _, _              -> do_convert(n - 10 * base, roman <> digits.0)
      False, True, _, _          -> do_convert(n +  1 * base, roman <> digits.2)
      False, False, True, _      -> do_convert(n -  5 * base, roman <> digits.1)
      False, False, False, True  -> do_convert(n +  1 * base, roman <> digits.2)
      False, False, False, False -> do_convert(n -  1 * base, roman <> digits.2)
    }
  }

  case n {
    n if n > 399 -> add_digit(100, #("M", "D", "C"))
    n if n >  39 -> add_digit( 10, #("C", "L", "X"))
    n if n >   0 -> add_digit(  1, #("X", "V", "I"))
    _ -> roman
  }
}

