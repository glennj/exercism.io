(in-package #:cl-user)
(defpackage #:twelve-days
  (:use #:cl)
  (:export #:recite))

(in-package #:twelve-days)

(defun recite (&optional begin end)
  (loop
    for i from (or begin 1) upto (or end begin 12)
    collect (verse i) into verses
    finally (return (format nil "~{~&~a~}" verses))))

(defun verse (n)
  (loop
    with gifts = (list nil
                       "a Partridge in a Pear Tree"
                       "two Turtle Doves"
                       "three French Hens"
                       "four Calling Birds"
                       "five Gold Rings"
                       "six Geese-a-Laying"
                       "seven Swans-a-Swimming"
                       "eight Maids-a-Milking"
                       "nine Ladies Dancing"
                       "ten Lords-a-Leaping"
                       "eleven Pipers Piping"
                       "twelve Drummers Drumming")
    with xmas-day = (format nil "On the ~:r day of Christmas" n)
    for i from n downto 1
    collect
        (format nil
                (if (= n 1) "~*~a" "~[and ~:;~]~a")
                (1- i) 
                (elt gifts i))
      into presents
    finally (return
      (format nil
              "~a my true love gave to me: ~{~a~^, ~}."
              xmas-day
              presents))))
