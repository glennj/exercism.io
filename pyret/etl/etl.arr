use context essentials2020 # Don't delete this line when using Pyret on Exercism 

provide: translate end

include string-dict

fun translate(legacy):
  fun add-letters(translated, key):
    letters = legacy.get-value(key)
    letters.foldl(
      {(letter, trans): trans.set(string-to-lower(letter), key)},
      translated
    )
  end

  legacy.keys().fold(add-letters, [string-dict: ])
end

