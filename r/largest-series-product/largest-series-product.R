largest_series_product <- function(digits, span) {
  if (grepl("\\D", digits))             stop("digits only.")
  if (span < 0 || span > nchar(digits)) stop("invalid span.")
  if (span == 0)                        return(1)

  nums <- as.numeric(unlist(strsplit(digits, "")))

  series_product <- function(i) prod(nums[i:(i + span - 1)])

  indices <- 1:(length(nums) - span + 1)

  max(sapply(indices, series_product))
}
