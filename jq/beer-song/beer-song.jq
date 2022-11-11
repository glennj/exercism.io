def if_num(n; yes; no): if . == n then yes else no end;
  
def bottles:
  "\(if_num(0; "No more"; .)) bottle\(if_num(1; ""; "s")) of beer"
;

def action:
  if_num(0;
    "Go to the store and buy some more";
    "Take \(if . == 1 then "it" else "one" end) down and pass it around"
  )
;

def verse:
  bottles as $b
  | "on the wall" as $where
  | "",
    "\($b) \($where), \($b | ascii_downcase).",
    "\(action), \(if_num(0; 99; . - 1) | bottles | ascii_downcase) \($where)."
;

[range(.startBottles; .startBottles - .takeDown; -1) | verse] | .[1:]
