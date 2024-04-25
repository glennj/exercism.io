import gleam/list

pub fn wines_of_color(wines: List(Wine), color c: Color) -> List(Wine) {
  wines |> list.filter(fn(w) {w.color == c})
}

pub fn wines_from_country(wines: List(Wine), country c: String) -> List(Wine) {
  wines |> list.filter(fn(w) {w.country == c})
}

// Please define the required labelled arguments for this function
pub fn filter(wines: List(Wine), color color: Color, country country: String) -> List(Wine) {
  wines
  |> wines_of_color(color)
  |> wines_from_country(country)
}

pub type Wine {
  Wine(name: String, year: Int, country: String, color: Color)
}

pub type Color {
  Red
  Rose
  White
}
