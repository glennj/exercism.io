# Common Lisp

Not _mentor_ notes, but _learning_ notes

https://exercism.org/tracks/common-lisp/concepts

## Truth

`()` and `nil` are false.
**Everything else** is true.

`t` is a handy symbol for "true".

`null` function tests if a list is empty.
```lisp
(null ())           ; => T
(null (list nil))   ; => NIL
```

## Conditionals

`cond`, `case`, `if`, `when/unless`

## Equality

- `=`
  - numbers only; relaxed about type
- `eq` 
  - object _identity_
- `eql`
  - numbers: same value _and type_
  - chars: same value
  - otherwise `eq`
- `equal`
  - lists: equal if each element is `equal`
  - strings: equal if each element is `eql` (thus case _sensitive_)
    - no other array type!
  - otherwise `eql`
- `equalp`
  - numbers: some coersion performed: `(equalp 1 1.0) # => T`
  - chars: case insensitive
  - strings: case insensitive
  - lists: equal if each element is `equalp`
  - arrays: equal if each element is `equalp`
  - structures: same type and all slots are `equalp`
  - hash-tables: keys and values are `equalp`

> It is generally considered better style to use type specific equality functions when one knows the types being compared.
> For example `string=` rather than `equal`. 

## Predicates

- "is it a ...?": `listp`, `stringp`, `arrayp`, `hash-table-p`
- empty list: `null`

## Functions

> (**defun** func-name (arg-list) "optional doc string" body)

Returns value of last expression.

### Lambda list

This is the name of the function's arg-list

#### Optional parameters

- `&optional` keyword
- allows for default values
- allows for predicate variable "was this value provided"

```lisp
(defun optional-parameters (&optional x (y 'default) (z nil z-supplied-p))
  (list x y (if z-supplied-p (list :z-was-supplied z)
                             (list :z-was-not-supplied z))))

(optional-parameters)          ; => (NIL DEFAULT (:Z-WAS-NOT-SUPPLIED NIL))
(optional-parameters 5 nil 10) ; => (5 DEFAULT (:Z-WAS-SUPPLIED 10))
```

#### Keyword parameters

`&key` keyword for named parameters

```lisp
(defun keyword-parameters (&key x (y 'default) (z nil z-supplied-p))
  (list x y (if z-supplied-p (list :z-was-supplied z)
                             (list :z-was-not-supplied z))))

(keyword-parameters)            ; => (NIL DEFAULT (:Z-WAS-NOT-SUPPLIED NIL))
(keyword-parameters :y 5)       ; => (NIL 5 (:Z-WAS-NOT-SUPPLIED NIL))
(keyword-parameters :z 10 :x 5) ; => (5 DEFAULT (:Z-WAS-SUPPLIED 10))
```

Care should be taken when combining optional and keyword parameters as the keyword name and argument could be consumed by optional parameters:
```lisp
(defun could-be-confusing (&optional x y &key z) (list x y z))
(could-be-confusing :z 'huh?) ; => (:Z HUH? NIL)
```

#### Rest parameter

- `&rest` keyword
- slurps remaining arguments into a list

```lisp
(defun rest-of-it (req &optional opt &rest rest) (list req opt rest))
(rest-of-it 1)         ; => (1 NIL NIL)
(rest-of-it 1 2)       ; => (1 2 NIL)
(rest-of-it 1 2 3)     ; => (1 2 (3))
(rest-of-it 1 2 3 4 5) ; => (1 2 (3 4 5))
```
## Multiple values

A function can return multiple values. 
Note that a list or an array is a single value.

```lisp
(defun example-1 () (values 1 2 3 4))
(example-1)
; => 1
; => 2
; => 3
; => 4
```

Return multiple list elements
```lisp
(defun example-2 () (values-list '(10 20 30 40)))
(example-2)
; => 10
; => 20
; => 30
; => 40
```

Capture multiple values into an array
```lisp
(multiple-value-list (example-1))    ; => '(1 2 3 4)
```

Capture multiple values bound to variables (temporarily)
> (**multiple-value-bind** (_varlist_) (_values_) _body_)

```lisp
(multiple-value-bind (a b c d) (example-1)
  (list d c b a))

;; => '(4 3 2 1)
```

## Datatypes

### Characters

> Characters are represented as #\\ followed by its name. The 'name' for common characters such as 'A' or 'b' or '9' or '!' are simply that: so #\\A, #\\b, #\\9 and #\\!. Some non-graphical characters such as space or new-line have names such as #\\Space and #\\Newline.
>
> The standard only requires an implementation to have 96 characters (upper and lower case Latin alphabetic characters (A-Za-z), the digits (0-9), space (#\\Space), newline (#\\Newline) and punctuation (e.g. !$"'(),\_-./:;?+<=>#%&\*@[\\]{|}\`^~) However most implementations will offer more than that, including implementations which provide all Unicode characters.

purpose | case-sensitive | case-insensitive
--------|----------------|-----------------
equality | `char=` | `char-equal`
less than | `char<` | `char-lessp
greater than | `char>` | `char-greaterp

- predicates: `graphic-char-p`, `alpha-char-p`, `alphanumericp`, `digit-char-p`, `upper-case-p`, `lower-case-p`
- case conversion: `char-upcase`, `char-downcase`

### Strings

- substring uses `subseq` since a string is a _vector_ of chars
  - zero-based indexing
- case sensitive equality: `string=` 
- case insensitive equality: `string-equal` 
- case conversion: `string-upcase`, `string-downcase`, `string-capitalize`

#### Formatting strings

> **format** _destination_ _control-string_ &rest _args_ => _result_

"destination" is a _stream_

> printing is done to the stream and there are two special values: `t` and `nil` for this argument.
> `t` means to print to standard output (actually the stream to which the variable \*standard-output\* is bound)
> and `nil` means to create a string and return it.





### Arrays

> **make-array** _dimensions_ &key _element-type_ _initial-element_ _initial-contents_ _adjustable_ _fill-pointer_ _displaced-to_ _displaced-index-offset_

```lisp
;; create
(setf a (make-array 4))  ; => #(0 0 0 0)  ;; default value _probably_ implementation-dependent
(setf a (make-array 4 :initial-contents '(11 22 33 44)))
(setf b #(a b c))

;; "lrepeat" -- use `:initial-element` keyword
(make-array 3 :initial-element 7)       ; => #(7 7 7)
(make-array '(2 3) :initial-element 7)  ; => #2A((7 7 7) (7 7 7))

;; multi-dimensional
(setf c #3A(((a b) (c d))
            ((e f) (g h))
            ((i j) (k l))))

;; access
(aref a 0)  ; => 11
(aref c 1 1 1)  ; => H
(aref c 0 1 0)  ; => C

;; set value at an index
(setf (aref b 1) 'YYY)    ; => YYY
b                         ; => #(A YYY C)
```

### Hash tables

```lisp
;; create
(setf h (make-hash-table))

;; get
(gethash :key h)

;; get or default
(gethash :key h :default-value)

;; set
(setf (gethash :key h) "new value")

;; remove a key
(remhash :key h)
```

### Dates and times

Universal time: seconds since 1900-01-01T00:00:00Z
```lisp
(get-universal-time)    ; => 3878676979
```

Decoded time: multiple values:
1. seconds
1. minutes
1. hours
1. day of month
1. month
1. year
1. day of week (0=Monday...6=Sunday)
1. daylight saving time (boolean)
1. time zone (GMT offset in hours, must be a multiple of 1/3600)
```lisp
(get-decoded-time)
; => 12
; => 19
; => 21
; => 28
; => 11
; => 2022
; => 0
; => NIL
; => 5
```

Universal time given a decoded time:
> (**encode-universal-time** _sec_ _min_ _hr_ _day_ _mon_ _yr_ _&optional time-zone_)
```lisp
(encode-universal-time 0 0 12 27 5 1967)      ; => 2126966400
(encode-universal-time 0 0 12 27 5 1967 5)    ; => 2126970000
```

The opposite direction
> (**decode-universal-time** _epoch_ _&optional time-zone_)
```lisp
(decode-universal-time 2126966400)
; => 0
; => 0
; => 12
; => 27
; => 5
; => 1967
; => 5
; => T
; => 5
```
But DST is a pain: don't assume your offset == your time zone
```lisp
 (decode-universal-time 2126966400 5)
; => 0
; => 0
; => 11     <<<<
; => 27
; => 5
; => 1967
; => 5
; => NIL    <<<<
; => 5
```

## Functional functions

### Filter

```lisp
;; remove list items based on a predicate
(remove-if #'evenp '(1 2 3 4 5)) ; => (1 3 5)
(remove-if #'oddp '(1 3 5))      ; => ()

;; remove specific elements
(remove 1 '(1 2 1 3 1 4)) ; => (2 3 4)
(remove #\l "hello")      ; => "heo"
```

Note, `remove` uses `eql`

### Map

> **mapcar** _function_ &rest _lists+_ => _result-list_

- apply the function to the car of the list, then recurse with the cdr
  ```lisp
  (mapcar #'string '(a b c d e f))  ; => ("A" "B" "C" "D" "E" "F")
  ```

  Note the syntax to name a function: `#'funcname`

> **maplist** _function_ &rest _lists+_ => _result-list_

- apply the function to the whole list, then recurse with the cdr
  ```lisp
  (maplist #'length '(a b c d e f))  ; => (6 5 4 3 2 1)
  ```

> **map** _result-type_ _function_ &rest _sequences+_ => _result_

- more general purpose
- "Applies function to successive sets of arguments in which one argument is obtained from each sequence.
  The function is called first on all the elements with index 0, then on all those with index 1, and so on.
  The result-type specifies the type of the resulting sequence."
  ```lisp
  (map 'string #'(lambda (x y)
                   (char "01234567890ABCDEF" (mod (+ x y) 16)))
        '(1 2 3 4)
        '(10 9 8 7)) =>  "AAAA"
  ```

### Reduce

> **reduce** _function_ _sequence_ &key _key_ _from-end_ _start_ _end_ _initial-value_ => _result_

<!-- ======================================== -->

## Missing concepts

- `setf`, `setq`, `define`
- `let`
- `loop`
