let tile = letter =>
  switch (letter) {
  | 'A' | 'E' | 'I' | 'O' | 'U'
  | 'L' | 'N' | 'R' | 'S' | 'T' =>  1
  | 'D' | 'G'                   =>  2
  | 'B' | 'C' | 'M' | 'P'       =>  3
  | 'F' | 'H' | 'V' | 'W' | 'Y' =>  4
  | 'K'                         =>  5
  | 'J' | 'X'                   =>  8
  | 'Q' | 'Z'                   => 10
  | _                           =>  0
  };

let score = word => {
  let s = ref(0)
  String.iter(c => s := s^ + tile(c), String.uppercase_ascii(word));
  s^
};
