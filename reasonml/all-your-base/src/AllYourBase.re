let rebase = (inbase, digits, outbase) => 
  if (inbase < 2 || outbase < 2) {
    None
  } else if (List.exists(d => d < 0 || d >= inbase, digits)) {
    None
  } else {
    let outdigits = ref([])
    let dec = ref(List.fold_left((sum, d) => sum * inbase + d, 0, digits))

    while (dec^ > 0) {
      outdigits := List.cons(dec^ mod outbase, outdigits^)
      dec := dec^ / outbase
    }

    List.length(outdigits^) == 0 ? None : Some(outdigits^)
  };
