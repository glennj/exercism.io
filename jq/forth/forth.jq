def isnumber: try tonumber catch false;

############################################################
def new_command(cmd):
  if (cmd[0] | isnumber) then "illegal operation" | halt_error
  elif cmd[-1] != ";" then "missing semicolon" | halt_error
  else
    .cmds as $c
    | .cmds[cmd[0]] = reduce cmd[1:-1][] as $word ([];
        . + if ($c | has($word)) then $c[$word] else [$word] end
      )
  end
;

############################################################
def need(n):
  (.stack | length) as $len
  | if   $len == 0 and n > 0 then "empty stack" | halt_error
    elif $len == 1 and n > 1 then "only one value on the stack" | halt_error
    else .
    end
;

############################################################
def add:      need(2) | .stack = .stack[:-2] + [.stack[-2] + .stack[-1]];
def subtract: need(2) | .stack = .stack[:-2] + [.stack[-2] - .stack[-1]];
def multiply: need(2) | .stack = .stack[:-2] + [.stack[-2] * .stack[-1]];
def divide:
  need(2)
  | if .stack[-1] == 0 then "divide by zero" | halt_error
    else .stack = .stack[:-2] + [(.stack[-2] / .stack[-1]) | floor]
    end;

############################################################
def dup:  need(1) | .stack += [.stack[-1]];
def drop: need(1) | .stack  = .stack[:-1];
def swap: need(2) | .stack  = .stack[:-2] + [.stack[-1], .stack[-2]];
def over: need(2) | .stack += [.stack[-2]];

############################################################
def evaluate(words):
  if words[0] == ":" then
    new_command(words[1:])
  else
    reduce words[] as $word (.;
      if (.cmds | has($word)) then evaluate(.cmds[$word])
      elif ($word | isnumber) then .stack += [$word | tonumber]
      elif $word == "+" then add
      elif $word == "-" then subtract
      elif $word == "*" then multiply
      elif $word == "/" then divide
      elif $word == "dup" then dup
      elif $word == "drop" then drop
      elif $word == "swap" then swap
      elif $word == "over" then over
      else "undefined operation" | halt_error
      end
    )
  end
;

############################################################
reduce .instructions[] as $line ({stack: [], cmds: {}};
  evaluate($line | ascii_downcase | split("\\s+";""))
)
| .stack
