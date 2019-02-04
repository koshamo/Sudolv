(defun prompt-read ()
  (format *query-io* "sudoku file (full path) : ")
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-sudoku (file)
  (with-open-file (stream file)
    (read stream)))

(defun make-sudoku-array (sudoku-list)
  (make-array '(9 9)
	      :initial-contents sudoku-list))

(defun num-in-cell-p (array num row line)
  (if (eql (aref array row line) num)
      t
      nil))

(defun num-in-line (array num line)
  (position-of-num array num 0 line 'line))

(defun num-in-row (array num row)
  (position-of-num array num row 0 'row))

(defun position-of-num (array num row line dir)
  (if (or (> row 8) (> line 8))
      nil
      (if (num-in-cell-p array num row line)
	  (cond ((eql dir 'line) row)
		((eql dir 'row) line))
	  (cond ((eql dir 'line)
		 (position-of-num array num (1+ row) line dir))
		((eql dir 'row)
		 (position-of-num array num row (1+ line) dir))))))

      

