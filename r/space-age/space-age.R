sec_per_earth_yr <- 31557600

relative_yr <- list(
  "mercury" =   0.2408467,
  "venus"   =   0.61519726,
  "earth"   =   1.0,
  "mars"    =   1.8808158,
  "jupiter" =  11.862615,
  "saturn"  =  29.447498,
  "uranus"  =  84.016846,
  "neptune" = 164.79132
)

space_age <- function(seconds, planet, digits = 2) {
  age <- seconds / sec_per_earth_yr / relative_yr[[planet]]
  round(age, digits)
}
