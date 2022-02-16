allergy <- function(num) {
  allergens <- c(
    "eggs", "peanuts", "shellfish", "strawberries",
    "tomatoes", "chocolate", "pollen", "cats"
  )

  # index the allergens vector using the indices of the
  # "1" bits in `num`
  masks <- bitwShiftL(1, 1:length(allergens) - 1)
  allergens[bitwAnd(num, masks) != 0]
}

allergic_to <- function(allergy_object, allergy) {
  allergy %in% allergy_object
}

list_allergies <- function(allergy_object) {
  as.character(allergy_object)
}
