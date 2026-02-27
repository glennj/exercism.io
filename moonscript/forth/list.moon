class List
  new: (items) =>
    @data = items or {}

  values: =>
    {table.unpack @data}

  is_empty: =>
    #@data == 0

  push: (item) =>
    table.insert @data, item

  pop: =>
    table.remove @data

  peek: =>
    @data[#@data]

  unshift: (item) =>
    table.insert @data, 1, item

  unshift_all: (items) =>
    for i, e in ipairs items
      table.insert @data, i, e

  shift: =>
    table.remove @data, 1
