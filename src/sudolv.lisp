(defun solve-sudoku ()
  (progn
    (init-sudoku-with-prompt)
    (solver)))

(defun solve-sudoku-file (file-name)
  (progn
    (init-sudoku-with-file file-name)
    (solver)))

(defun init-sudoku-with-prompt ()
  (let* ((file-name (prompt-read))
	 (file-content (read-sudoku file-name)))
      (make-sudoku-array file-content)
      (replace-nils)
      (init-possibilities)))

(defun init-sudoku-with-file (file-name)
  (let ((file-content (read-sudoku file-name)))
      (make-sudoku-array file-content)
      (replace-nils)
      (init-possibilities)))

(defun solver ()
  (progn
    (do ((changed t))
	((null changed))
      (setf changed (single-value-solver)))
    (sudoku-solved-p)))

(defparameter *sudoku* nil "Sudoku array to work with")
(defparameter *size* 0 "size of Sudoku")
(defparameter *square-size* 0 "dimension of subsquare")

(defun prompt-read ()
  (format *query-io* "sudoku file (full path) : ")
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-sudoku (file)
  (with-open-file (stream file)
    (read stream)))

(defun make-sudoku-array (sudoku-list)
  (let* ((size (length sudoku-list))
	(square-size (truncate (sqrt size))))
    (setf *size* size)
    (setf *square-size* square-size)
    (setf *sudoku*
	  (make-array (list size size)
		      :initial-contents sudoku-list))))

(defmacro every-cell (max-index &body body)
  `(dotimes (x ,max-index)
    (dotimes (y ,max-index)
       ,@body)))

(defun replace-nils ()
  (every-cell *size*
    (when (null (aref *sudoku* x y))
      (setf (aref *sudoku* x y) (list-numbers *size*)))))

(defun list-numbers (num)
  (list-numbers' num nil))

(defun list-numbers' (num numlist)
  (if (> num 0)
      (list-numbers' (- num 1) (cons num numlist))
      numlist))
  
(defun remove-possibilities (x y)
  (let ((num (aref *sudoku* x y)))
    (remove-possibilities-in-line num y)
    (remove-possibilities-in-col num x)
    (remove-possibilities-in-square num x y)))

(defun remove-possibilities-in-line (num line)
  (dotimes (x *size*)
    (when (listp (aref *sudoku* x line))
      (setf (aref *sudoku* x line) (remove num (aref *sudoku* x line))))))

(defun remove-possibilities-in-col (num col)
  (dotimes (y *size*)
    (when (listp (aref *sudoku* col y))
      (setf (aref *sudoku* col y) (remove num (aref *sudoku* col y))))))

(defun remove-possibilities-in-square (num col line)
  (let ((square-x (truncate (/ col *square-size*)))
	(square-y (truncate (/ line *square-size*))))
    (every-cell *square-size*
      (when (listp (aref *sudoku*
			 (+ x (* square-x *square-size*))
			 (+ y (* square-y *square-size*))))
	(setf (aref *sudoku*
		    (+ x (* square-x *square-size*))
		    (+ y (* square-y *square-size*)))
	      (remove num (aref *sudoku*
				(+ x (* square-x *square-size*))
				(+ y (* square-y *square-size*)))))))))

(defun init-possibilities ()
  (every-cell *size*
    (when (numberp (aref *sudoku* x y))
      (remove-possibilities x y))))

(defun sudoku-solved-p ()
  (let ((solved t))
    (every-cell *size*
      (when (listp (aref *sudoku* x y))
	(setf solved nil)
	(return)))
    solved))
    
(defun single-value-solver ()
  (let ((changed nil))
    (every-cell *size*
      (when (and (listp (aref *sudoku* x y)) (eql (length (aref *sudoku* x y)) 1))
	(setf (aref *sudoku* x y) (car (aref *sudoku* x y)))
	(remove-possibilities x y)
	(setf changed t)))
    changed))

(defun single-position-solver ()
  (progn
    (single-position-in-line)
    (single-position-in-col)
    (single-position-in-square)))

(defun single-position-in-line ()
  )

(defun single-position-in-col ()
  nil)

(defun single-position-in-square ()
  nil)



(defun num-in-cell-p (num col line)
  (if (eql (aref *sudoku* col line) num)
      t
      nil))

(defun num-in-line (num line)
  (position-of-num num 0 line 'line))

(defun num-in-col (num col)
  (position-of-num num col 0 'col))

(defun position-of-num (num col line dir)
  (if (or (> col (- *size* 1)) (> line (- *size* 1)))
      nil
      (if (num-in-cell-p num col line)
	  (cond ((eql dir 'line) col)
		((eql dir 'col) line))
	  (cond ((eql dir 'line)
		 (position-of-num num (1+ col) line dir))
		((eql dir 'col)
		 (position-of-num num col (1+ line) dir))))))


