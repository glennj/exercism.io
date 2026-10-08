let sentinel = -1     // represents "no coins can make changes for amount n"
let inf = 999_999_999 // "infinity" except it won't overflow if you add 1 to it :shrug:

/* Change making algorithm from
 * http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf
 *
 * The 'change' function generates 2 arrays:
 * - minCoins: maps the minimum number of coins required to make change
 *     for each amount from 1 to total
 * - firstCoin: the _first_ coin used to make change for amount n
 *     (actually stores the index into the coins array)
 *
 * Returns the firstCoin array
 */
let change = (coins: array<int>, total: int): array<int> => {
  let minCoins = [0]
  let firstCoin = [sentinel]

  for n in 1 to total {
    let min = ref(inf)
    let coin = ref(sentinel)

    coins->Array.forEachWithIndex((denom, i) =>
      if denom <= n {
        let mc = minCoins->Array.getUnsafe(n - denom)
        if 1 + mc < min.contents {
          min := 1 + mc
          coin := i
        }
      }
    )

    minCoins->Array.push(min.contents)
    firstCoin->Array.push(coin.contents)
  }
  firstCoin
}

let makeChange = (firstCoin, coins, total): option<array<int>> => {
  let ok = ref(true)
  let change = []
  let amount = ref(total)

  while ok.contents && amount.contents > 0 {
    let idx = firstCoin->Array.getUnsafe(amount.contents)
    if idx == sentinel {
      ok := false
    } else {
      let coin = coins->Array.getUnsafe(idx)
      change->Array.push(coin)
      amount := amount.contents - coin
    }
  }

  if ok.contents {Some(change)} else {None}
}

let findFewestCoins = (coins: array<int>, total: int): option<array<int>> =>
  if total < 0 {
    None
  } else {
    change(coins, total)->makeChange(coins, total)
  }
