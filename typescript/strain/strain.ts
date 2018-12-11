type Pred<T> = (element: T) => boolean

export function keep<T> (list: T[], predicate: Pred<T >): T[] {
  return list.reduce((result, element) => {
    if (predicate(element)) {result.push(element)}
    return result
  }, new Array<T>())
}

export function discard<T> (list: T[], predicate: Pred<T> ): T[] {
  return keep(list, (element) => !predicate(element))
}
