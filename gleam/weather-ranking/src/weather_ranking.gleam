import gleam/order.{type Order}
import gleam/float
import gleam/list

pub type City {
  City(name: String, temperature: Temperature)
}

pub type Temperature {
  Celsius(Float)
  Fahrenheit(Float)
}

pub fn fahrenheit_to_celsius(f: Float) -> Float {
  {f -. 32.} /. 1.8 
}

pub fn compare_temperature(left: Temperature, right: Temperature) -> Order {
  let celsius_temp = fn(temp) {
    case temp {
      Fahrenheit(f) -> fahrenheit_to_celsius(f)
      Celsius(c) -> c
    }
  }

  float.compare(celsius_temp(left), celsius_temp(right))
}

pub fn sort_cities_by_temperature(cities: List(City)) -> List(City) {
  list.sort(cities, fn(a, b) {compare_temperature(a.temperature, b.temperature)})
}
