export function flatten<T> (list: T[]): T[] {
  let result: T[] = []
  list.forEach((element: T | T[]) => {
    if (element !== undefined) {
      result = result.concat(
        Array.isArray(element)
          ? flatten(element)
          : element
      )
    }
  })
  return result
}
