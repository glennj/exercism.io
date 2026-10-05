let toRna = strand =>
  strand
  ->String.replaceAll("C", "c")
  ->String.replaceAll("G", "C")
  ->String.replaceAll("c", "G")
  ->String.replaceAll("A", "U")
  ->String.replaceAll("T", "A")
