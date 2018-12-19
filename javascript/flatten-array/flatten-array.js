class Flattener {
  flatten(list) {
    let result = [];
    for (let i = 0; i < list.length; i += 1) {
      if (list[i] !== null && list[i] !== undefined) {
        result = result.concat(Array.isArray(list[i]) ? this.flatten(list[i]) : list[i]);
      }
    }
    return result;
  }
}

module.exports = Flattener;
