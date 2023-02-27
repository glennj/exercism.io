import std/sequtils
 
type
  Callback = proc(val: int)
  ComputeFunc = proc(vals: seq[int]): int {.noSideEffect.}  # func "type"

  Cell* = ref object of RootObj
    value: int
    listeners: seq[ComputeCell]
    callbacks: seq[Callback]

  InputCell = ref object of Cell

  ComputeCell = ref object of Cell
    inputs: seq[Cell]
    previousValue: int
    computeFunction: ComputeFunc

############################################################
# Cell procs
#
# a couple of forward declarations
proc recompute(cell: ComputeCell)
proc fireCallbacks(cell: ComputeCell)

proc recomputeListeners(cell: Cell) =
  for c in cell.listeners:
    c.recompute()

proc fireListenerCallbacks(cell: Cell) =
  for c in cell.listeners:
    c.fireCallbacks()

proc value*(cell: Cell): int = cell.value

proc `value=`*(cell: Cell, value: int) =
  cell.value = value
  cell.recomputeListeners()
  cell.fireListenerCallbacks()

############################################################
# InputCell procs
#
proc newInputCell*(value: int): Cell =
  result = InputCell(value: value)

############################################################
# ComputeCell procs
#
proc newComputeCell*(inputs: seq[Cell], function: ComputeFunc): Cell =
  let computeCell = ComputeCell(inputs: inputs, computeFunction: function)
  for cell in inputs:
    cell.listeners.add computeCell
  computeCell.recompute()
  computeCell.previousValue = computeCell.value
  computeCell

proc recompute(cell: ComputeCell) =
  cell.value = cell.computeFunction(cell.inputs.mapIt(it.value))
  cell.recomputeListeners()

proc addCallback*(cell: Cell, callback: Callback) =
  cell.callbacks.add callback

proc fireCallbacks(cell: ComputeCell) =
  if cell.value != cell.previousValue:
    cell.previousValue = cell.value
    for cb in cell.callbacks:
      cb(cell.value)
    cell.fireListenerCallbacks()

proc removeCallback*(cell: Cell, callback: Callback) =
  cell.callbacks.keepItIf(it != callback)
