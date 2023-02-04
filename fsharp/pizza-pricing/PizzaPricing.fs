module PizzaPricing

type Pizza =
    | Margherita
    | Caprese
    | Formaggio
    | ExtraSauce of Pizza
    | ExtraToppings of Pizza

let rec pizzaPrice (pizza: Pizza): int = 
    match pizza with
    | Margherita -> 7
    | Caprese -> 9
    | Formaggio -> 10
    | ExtraSauce basePizza -> 1 + pizzaPrice basePizza
    | ExtraToppings basePizza -> 2 + pizzaPrice basePizza

let orderPrice(pizzas: Pizza list): int =

    let rec order (pizzas: Pizza list) (subtotal: int): int =
        match pizzas with
        | [] -> subtotal
        | first::rest -> order rest (subtotal + pizzaPrice first)

    match pizzas with
    | [] -> 0
    | [pizza] -> 3 + pizzaPrice pizza
    | [first; second] -> 2 + pizzaPrice first + pizzaPrice second
    | _ -> order pizzas 0
