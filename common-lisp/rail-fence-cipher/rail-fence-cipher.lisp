(defpackage :rail-fence-cipher
  (:use :cl)
  (:export :encode
           :decode))

(in-package :rail-fence-cipher)

(defun cycle (&rest elems)
  (setf (cdr (last elems)) elems)
  (lambda () (pop elems)))

(defun rail-cycle (n)
	(let ((seq (append
               (loop :for i :below (1- n) :collect i)
               (loop :for i :from (1- n) :above 0 :collect i))))
    (apply #'cycle seq)))
			
(defun encode (msg rails)
  (let* ((next-rail (rail-cycle rails))
         (char-rail (loop :for c :across msg
                          :for r = (funcall next-rail)
                          :collect (cons c r))))
    (loop :for i :below rails
          :collect (concatenate 'string
                                (mapcar #'car
                                        (remove-if #'(lambda (pair) (/= i (cdr pair)))
                                                   char-rail)))
            :into rails
          :finally (return (apply #'concatenate 'string rails)))))

(defun decode (msg rails)
  (let* ((len (length msg))
         (cycle-len (* 2 (1- rails)))
         (cycles (floor len cycle-len))
         (decoded (make-array len)))
    (loop :with k = -1
          :with idx
          :for i :below rails
          :do (loop :for j :upto cycles
                    :do (setf idx (+ i (* j cycle-len)))
                    :if (< idx len)
                      :do (setf (elt decoded idx) (elt msg (incf k)))
                      ; middle rails consume two characters per cycle
                      :and :if (< 0 i (1- rails))
                             :do (setf idx (+ (- cycle-len i) (* j cycle-len)))
                             :and :if (< idx len)
                                    :do (setf (elt decoded idx) (elt msg (incf k)))))
    (concatenate 'string decoded)))
