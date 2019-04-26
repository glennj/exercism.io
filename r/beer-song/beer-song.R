library(stringr)

lyrics <- function(first, last) {
  paste0(sapply(first:last, verse), collapse = "\n")
}

verse <- function(n, max_beers = 99, where = "on the wall") {
  b <- bottle(n)
  first <- sprintf("%s %s, %s.\n", str_to_sentence(b), where, b)

  b <- bottle(ifelse(n == 0, max_beers, n - 1))
  second <- sprintf("%s, %s %s.\n", task(n), b, where)

  paste0(first, second)
}

bottle <- function(n) {
  sprintf("%s bottle%s of beer",
    ifelse(n == 0, "no more", n),
    ifelse(n == 1, "", "s")
  )
}

task <- function(n) {
  if (n == 0) {
    "Go to the store and buy some more"
  } else {
    sprintf("Take %s down and pass it around",
      ifelse(n == 1, "it", "one"))
  }
}
