(module
  (import "console" "log_mem_as_u32" (func $log_mem_as_u32 (param i32) (param i32)))
  (memory (export "mem") 1 4) ;; up to 4 pages of linear memory are allocatable

  (global $capacity (mut i32) (i32.const 0))
  (global $count    (mut i32) (i32.const 0))
  (global $readp    (mut i32) (i32.const 0))
  (global $writep   (mut i32) (i32.const 0))
  (global $i32Size  i32       (i32.const 4))
  (global $pageSize i32       (i32.const 0x10000))

  (func $is-empty (result i32) (i32.eqz (global.get $count)))
  (func $is-full  (result i32) (i32.eq (global.get $count) (global.get $capacity)))

  (func $inc (param i32) (result i32) (i32.add (local.get 0) (i32.const 1)))
  (func $dec (param i32) (result i32) (i32.sub (local.get 0) (i32.const 1)))

  (func $incp (param i32) (result i32)
    ;; Forth style
    (local.get 0)
    (global.get $i32Size)
    (i32.div_u)
    (call $inc)
    (global.get $capacity)
    (i32.rem_u)
    (global.get $i32Size)
    (i32.mul))

  (func $requested-capacity-too-big (param $requestedCapacity i32) (result i32)
    (local $requestedBytes i32)
    (local $availableBytes i32)
    (local.set $requestedBytes (i32.mul (local.get $requestedCapacity)
                                        (global.get $i32Size)))
    (loop $loop (result i32)
      (local.set $availableBytes (i32.mul (memory.size)
                                          (global.get $pageSize)))
      (if (i32.le_u (local.get $requestedBytes) (local.get $availableBytes))
        (then (return (i32.const 0)))) ;; not too big
      (memory.grow (i32.const 1))
      (br_if $loop (i32.ne (i32.const -1)))
      (return (i32.const 1)))) ;; cannot allocate enough memory: too big
    
  ;;
  ;; Initialize a circular buffer of i32s with a given capacity
  ;;
  ;; @param {i32} newCapacity - capacity of the circular buffer
  ;;
  ;; @returns {i32} 0 on success or -1 on error
  ;; 
  (func (export "init") (param $newCapacity i32) (result i32)
    (if (call $requested-capacity-too-big (local.get $newCapacity))
      (then (return (i32.const -1))))

    (global.set $capacity (local.get $newCapacity))
    (call $clear)
    (i32.const 0))

  ;;
  ;; Clear the circular buffer
  ;;
  (func $clear (export "clear")
    (global.set $count  (i32.const 0))
    (global.set $readp  (i32.const 0))
    (global.set $writep (i32.const 0))
  )

  ;; 
  ;; Add an element to the circular buffer
  ;;
  ;; @param {i32} elem - element to add to the circular buffer
  ;;
  ;; @returns {i32} 0 on success or -1 if full
  ;;
  (func $write (export "write") (param $elem i32) (result i32)
    (if (call $is-full) (return (i32.const -1)))
    (i32.store (global.get $writep) (local.get $elem))
    (global.set $count (call $inc (global.get $count)))
    (global.set $writep (call $incp (global.get $writep)))
    (i32.const 0))

  ;; 
  ;; Add an element to the circular buffer, overwriting the oldest element
  ;; if the buffer is full
  ;;
  ;; @param {i32} elem - element to add to the circular buffer
  ;;
  ;; @returns {i32} 0 on success or -1 if full (capacity of zero)
  ;;
  (func (export "forceWrite") (param $elem i32) (result i32)
    (if (call $is-full) (then (call $read) (drop) (drop)))
    (call $write (local.get $elem))
  )

  ;;
  ;; Read the oldest element from the circular buffer, if not empty
  ;;
  ;; @returns {i32} element on success or -1 if empty
  ;; @returns {i32} status code set to 0 on success or -1 if empty
  ;;
  (func $read (export "read") (result i32 i32)
    (local $elem i32)
    (if (call $is-empty) (return (i32.const -1) (i32.const -1)))
    (local.set $elem (i32.load (global.get $readp)))
    (global.set $count (call $dec (global.get $count)))
    (global.set $readp (call $incp (global.get $readp)))
    (return (local.get $elem) (i32.const 0))))
