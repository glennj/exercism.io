(defmodule leap
	(export (leap-year 1)))

(defun leap-year (year)
	(and
		(zerop (rem year 4))
		(or
			(not (zerop (rem year 100)))
			(zerop (rem year 400)))))

(defun zerop (n)
	(== 0 n))
