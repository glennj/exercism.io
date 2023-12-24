use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: commands end

fun commands(binary):
  fold2(
    lam(cmds, bit, action):
      ask:
        | bit == "0" then: cmds
        | action == "rev" then: cmds.reverse()
        | otherwise: cmds + [list: action]
      end
    end,
    [list:],
    string-explode(binary).reverse(),
    [list: "wink", "double blink", "close your eyes", "jump", "rev"]
  )
end
