use context starter2024

provide: my-translate end

include string-dict

fun my-translate(legacy):
  fun add-letters(translated, key):
    letters = legacy.get-value(key)
    letters.foldl(
      {(letter, trans): trans.set(string-to-lower(letter), key)},
      translated
    )
  end

  legacy.keys().fold(add-letters, [string-dict: ])
end

