U   <- paste0(LETTERS, collapse = "")
L   <- paste0(letters, collapse = "")
old <- paste0(U, L)


rotate <- function(text, key) {
  key <- key %% 26

  uu  <- paste0(substr(U, key + 1, 26), substr(U, 1, key))
  ll  <- paste0(substr(L, key + 1, 26), substr(L, 1, key))
  new <- paste0(uu, ll)

  chartr(old, new, text)
}
