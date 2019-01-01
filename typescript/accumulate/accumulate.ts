function accumulate<T, U> (
    list: T[],
    func: (element: T) => U
): U[]
{
  // return list.map(func)
  const result: U[] = []
  for (const element of list) {
    result.push( func(element) )
  }
  return result
}

export default accumulate
