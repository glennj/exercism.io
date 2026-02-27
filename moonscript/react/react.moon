-- -----------------------------------------------------------
class Cell
  new: =>
    @listeners = {}

  set_value: (value) =>
    @value = value

  get_value: =>
    @value

  add_listener: (cell) =>
    table.insert @listeners, cell

  recompute_listeners: =>
    listener\recompute! for listener in *@listeners

  fire_listener_callbacks: =>
    listener\fire_callbacks! for listener in *@listeners


-- -----------------------------------------------------------
class InputCell extends Cell
  new: (value) =>
    super!
    @set_value value

  set_value: (value) =>
    super value
    @recompute_listeners!
    @fire_listener_callbacks!


-- -----------------------------------------------------------
class ComputeCell extends Cell
  new: (...) =>
    super!
    @inputs = {...}
    @func = table.remove @inputs
    cell\add_listener @ for cell in *@inputs
    @compute_value!
    @prev_value = @get_value!
    @callbacks = {}

  compute_value: =>
    values = [cell\get_value! for cell in *@inputs]
    -- note the odd parentheses: can't quite explain it ATM
    @set_value (@func) table.unpack values

  recompute: =>
    @compute_value!
    @recompute_listeners!

  watch: (callback) =>
    table.insert @callbacks, callback

  unwatch: (callback) =>
    @callbacks = [cb for cb in *@callbacks when cb != callback]

  fire_callbacks: =>
    val = @get_value!
    if @prev_value != val
      @prev_value = val
      cb val for cb in *@callbacks
      @fire_listener_callbacks!


-- -----------------------------------------------------------
{ :InputCell, :ComputeCell }
