(defpackage :secret-handshake
  (:use :cl)
  (:export :commands))

(in-package :secret-handshake)

(defun reverse-if (cond list)
  "Reverse a list if cond is T"
  (funcall (if cond #'reverse #'identity) list))

(defun commands (number)
  (loop for i = 0 then (1+ i)
        for cmd in '("wink" "double blink" "close your eyes" "jump")
        if (logbitp i number) collect cmd into cmds
        finally (return (reverse-if (logbitp i number) cmds))))
