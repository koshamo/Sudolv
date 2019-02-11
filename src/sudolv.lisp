(defun test-sudoku ()
  (let* ((file-name (prompt-read))
	 (file-content (read-sudoku file-name)))
    (progn
      (make-sudoku-array file-content)
      (replace-nils)
      *sudoku*)))

(defparameter *sudoku* nil "Sudoku array to work with")
(defparameter *size* 0 "size of Sudoku")

(defun prompt-read ()
  (format *query-io* "sudoku file (full path) : ")
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-sudoku (file)
  (with-open-file (stream file)
    (read stream)))

(defun make-sudoku-array (sudoku-list)
  (progn
    (setf *sudoku*
	  (make-array '(9 9)
		      :initial-contents sudoku-list))
    (setf *size* (length sudoku-list))))

(defun replace-nils ()
  (dotimes (x *size*)
    (dotimes (y *size*)
      (when (null (aref *sudoku* y x))
	(setf (aref *sudoku* y x) '(1 2 3 4 5 6 7 8 9))))))

(defun num-in-cell-p (num row line)
  (if (eql (aref *sudoku* row line) num)
      t
      nil))

(defun num-in-line (num line)
  (position-of-num num 0 line 'line))

(defun num-in-row (num row)
  (position-of-num num row 0 'row))

(defun position-of-num (num row line dir)
  (if (or (> row (- *size* 1)) (> line (- *size* 1)))
      nil
      (if (num-in-cell-p num row line)
	  (cond ((eql dir 'line) row)
		((eql dir 'row) line))
	  (cond ((eql dir 'line)
		 (position-of-num num (1+ row) line dir))
		((eql dir 'row)
		 (position-of-num num row (1+ line) dir))))))

      

