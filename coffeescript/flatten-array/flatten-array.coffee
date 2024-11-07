class FlattenArray
  @flatten: (values) ->
    worker = (result, val) =>
      return result unless val?
      result.concat (if Array.isArray val then @flatten val else val)

    values.reduce worker, []

module.exports = FlattenArray
