# this module defines functions that act on "Buckets"
#
# A Bucket is an object with keys
#   - name
#   - size
#   - amount

def new($name; $size): {$name, $size, amount: 0};

def name: .name;
def size: .size;
def amount: .amount;
def capacity: .size - .amount;

def fill: .amount = .size;
def empty: .amount = 0;

def isFull: .amount == .size;
def isEmpty: .amount == 0;

# This function is unusual: it ignores its input.
# It has to modify two buckets: pass both as arguments, and return an array.
def pour($from; $to):
  ([$from.amount, ($to | capacity)] | min) as $quantity
  | [($from | .amount -= $quantity), ($to | .amount += $quantity)]
;
