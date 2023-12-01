;; from https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
;; | algorithm Sieve of Eratosthenes is
;; |     input: an integer n > 1.
;; |     output: all prime numbers from 2 through n.
;; | 
;; |     let A be an array of Boolean values, indexed by integers 2 to n,
;; |     initially all set to true.
;; |     
;; |     for i = 2, 3, 4, ..., not exceeding √n do
;; |         if A[i] is true
;; |             for j = i², i²+i, i²+2i, i²+3i, ..., not exceeding n do
;; |                 set A[j] := false
;; | 
;; |     return all i such that A[i] is true.

(module
  (memory (export "mem") 1)
  (global $i32size i32 (i32.const 4))

  (func $array-offset (param i32) (result i32)
    (i32.add (i32.const 1024)
             (i32.mul (local.get 0) (global.get $i32size))))

  (func $result-offset (param i32) (result i32)
    (i32.mul (local.get 0) (global.get $i32size)))

  ;; create an array from 2 through n
  (func $make-array (param $limit i32) 
    (local $i i32)
    (local.set $i (i32.const 2))
    (loop $loop
      (if (i32.le_s (local.get $i) (local.get $limit))
        (then
          (i32.store (call $array-offset (local.get $i)) (local.get $i))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $loop)))))

  ;; for each i from 2 to √n, mark all multiples as non-prime
  (func $mark-multiples (param $limit i32)
    (local $i i32) (local $j i32) (local $sqrt i32)
    (local.set $sqrt
      (i32.trunc_f32_s (f32.sqrt (f32.convert_i32_s (local.get $limit)))))
    (local.set $i (i32.const 2))
    (loop $A
      (if (i32.le_s (local.get $i) (local.get $sqrt))
        (then
          (if (i32.load (call $array-offset (local.get $i)))
            (then
              (local.set $j (i32.mul (local.get $i) (local.get $i)))
              (loop $B
                (if (i32.le_s (local.get $j) (local.get $limit))
                  (then
                    (i32.store (call $array-offset (local.get $j)) (i32.const 0))
                    (local.set $j (i32.add (local.get $j) (local.get $i)))
                    (br $B))))))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $A)))))
  
  ;; collect primes from work array
  (func $collect-primes (param $limit i32) (result i32)
    (local $p i32) (local $i i32) (local $prime i32)
    (local.set $p (i32.const 0))
    (local.set $i (i32.const 2))
    (loop $loop
      (if (i32.le_s (local.get $i) (local.get $limit))
        (then
          (if (local.tee $prime (i32.load (call $array-offset (local.get $i))))
            (then
              (i32.store (call $result-offset (local.get $p)) (local.get $prime))
              (local.set $p (i32.add (local.get $p) (i32.const 1)))))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $loop))))
    (return (local.get $p)))

  ;;
  ;; Determine all the prime numbers below a given limit.
  ;; Return the offset and length of the resulting array of primes.
  ;;
  ;; @param {i32} limit - the upper bound for the prime numbers
  ;;
  ;; @return {i32} - offset off the u32[] array
  ;; @return {i32} - length off the u32[] array in elements
  ;;
  (func (export "primes") (param $limit i32) (result i32 i32)
    (call $make-array (local.get $limit))
    (call $mark-multiples (local.get $limit))
    (i32.const 0)                             ;; offset of result array
    (call $collect-primes (local.get $limit)) ;; leaves length on stack
    (return)))
