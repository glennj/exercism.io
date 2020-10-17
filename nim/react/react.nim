import sequtils, strformat
import sugar

type
  Callback = int->void
  ComputeFunc = seq[int]->int

  Cell* = ref object of RootObj
    id: int
    value: int
    listeners: seq[ComputeCell]

  InputCell = ref object of Cell

  ComputeCell = ref object of Cell
    inputs: seq[Cell]
    previousValue: int
    computeFunction: ComputeFunc
    callbacks: seq[Callback]

  Reactor = ref object

var ID = 0


# I don't see why this exercise really needs a reactor type
proc newReactor*(): Reactor = Reactor()

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
proc createInput*(r: Reactor, value: int): InputCell =
  ID.inc
  result = InputCell(value: value, id: ID)


############################################################
# ComputeCell procs
#
proc createCompute*(
    r: Reactor,
    inputs: seq[Cell],
    function: ComputeFunc
): ComputeCell =
  ID.inc
  result = ComputeCell(inputs: inputs, computeFunction: function, id: ID)
  for cell in inputs:
    cell.listeners.add result
  result.recompute()
  result.previousValue = result.value

proc recompute(cell: ComputeCell) =
  cell.value = cell.computeFunction(cell.inputs.mapIt(it.value))
  cell.recomputeListeners()

proc addCallback*(cell: ComputeCell, callback: Callback): Callback =
  cell.callbacks.add callback
  callback

proc fireCallbacks(cell: ComputeCell) =
  if cell.value != cell.previousValue:
    cell.previousValue = cell.value
    for cb in cell.callbacks:
      cb(cell.value)
    cell.fireListenerCallbacks()

proc removeCallback*(cell: ComputeCell, callback: Callback) =
  cell.callbacks.keepItIf(it != callback)


############################################################
# This was for debugging
# How to get the object's actual (descendant) type?
proc `$`(cell: Cell, cellType: typedesc = Cell): string =
  var cellType = "Cell"
  if cell of InputCell: cellType = "InputCell"
  if cell of ComputeCell: cellType = "ComputeCell"
  &"{$cellType}(id: {cell.id}, value: {cell.value})"
