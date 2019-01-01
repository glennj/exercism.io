type Old = {[key: string]: string[]}
type New = {[key: string]: number}

function transform (oldObj: Old): New {
  const newObj: New = {}
  Object.getOwnPropertyNames(oldObj).forEach((oldKey) => {
    oldObj[oldKey].forEach((oldValue) => {
      newObj[oldValue.toLowerCase()] = Number(oldKey)
    })
  })
  return newObj
}

export default transform
