import gleam/queue.{type Queue}
import gleam/result
import gleam/pair

pub fn insert_top(cards: Queue(Int), card: Int) -> Queue(Int) {
  queue.push_back(cards, card)
}

pub fn remove_top_card(cards: Queue(Int)) -> Queue(Int) {
  remove_card(cards, queue.pop_back)
}

pub fn insert_bottom(cards: Queue(Int), card: Int) -> Queue(Int) {
  queue.push_front(cards, card)
}

pub fn remove_bottom_card(cards: Queue(Int)) -> Queue(Int) {
  remove_card(cards, queue.pop_front)
}

pub fn check_size_of_stack(cards: Queue(Int), target: Int) -> Bool {
  queue.length(cards) == target
}

type PopFunc(a) = fn(Queue(a)) -> Result(#(a, Queue(a)), Nil)

fn remove_card(cards: Queue(Int), pop: PopFunc(Int)) -> Queue(Int) { 
  pop(cards)
  |> result.map(pair.second)
  |> result.unwrap(or: cards)
}
