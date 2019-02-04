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
  (position-in-line array num 0 line))

(defun position-in-line (array num row line)
  (if (eql row 9)
      nil
      (if (num-in-cell-p array num row line)
	  row
	  (position-in-line array num (1+ row) line))))

(defun num-in-row (array num row)
  (position-in-row array num row 0))

(defun position-in-row (array num row line)
  (if (eql line 9)
      nil
      (if (num-in-cell-p array num row line)
	  line
	  (position-in-row array num row (1+ line)))))
      

