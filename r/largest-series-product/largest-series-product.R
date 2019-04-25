largest_series_product <- function(digits, span) {
  if (grepl("\\D", digits))             stop("digits only.")
  if (span < 0 || span > nchar(digits)) stop("invalid span.")
  if (span == 0)                        return(1)

  nums <- as.numeric(unlist(strsplit(digits, "")))

  series_product <- function(i) prod(nums[i:(i + span - 1)])

  indices <- 1:(length(nums) - span + 1)

  max(sapply(indices, series_product))
}


# community     
#
# tidy error checking: split string into vector of int,
#                      _then_ check "any is.na"
# https://exercism.io/tracks/r/exercises/largest-series-product/solutions/41b094ad86244b3cb93b93bca96e5b63
#   
#     nums <- as.numeric(unlist(strsplit(digits, "")))
#       
#     if (any(is.na(nums)) || span < 0 || span > length(nums)) {
#       stop("Problem with span or non-digit characters present")
#     }
