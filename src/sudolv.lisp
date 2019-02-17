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
  (do ((has-changed t))
      ((not (and has-changed
		 (not (sudoku-solved-p)))))
    (setf has-changed nil)
    (single-value-solver-loop)
    (unless (sudoku-solved-p)
      (setf has-changed (single-position-solver))))
  (print-sudoku))
	

(defparameter *sudoku* nil "Sudoku array to work with")
(defparameter *size* 0 "size of Sudoku")
(defparameter *square-size* 0 "dimension of subsquare")

(defun print-sudoku ()
  *sudoku*)

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
    (when (null (aref *sudoku* y x))
      (setf (aref *sudoku* y x) (list-numbers *size*)))))

(defun list-numbers (num)
  (list-numbers* num nil))

(defun list-numbers* (num numlist)
  (if (> num 0)
      (list-numbers* (- num 1) (cons num numlist))
      numlist))
  
(defun remove-possibilities (x y)
  (let ((num (aref *sudoku* y x)))
    (remove-possibilities-in-line num y)
    (remove-possibilities-in-col num x)
    (remove-possibilities-in-square num x y)))

(defun remove-possibilities-in-line (num line)
  (dotimes (x *size*)
    (when (listp (aref *sudoku* line x))
      (setf (aref *sudoku* line x) (remove num (aref *sudoku* line x))))))

(defun remove-possibilities-in-col (num col)
  (dotimes (y *size*)
    (when (listp (aref *sudoku* y col))
      (setf (aref *sudoku* y col) (remove num (aref *sudoku* y col))))))

(defun remove-possibilities-in-square (num col line)
  (let ((square-x (truncate (/ col *square-size*)))
	(square-y (truncate (/ line *square-size*))))
    (every-cell *square-size*
      (when (listp (aref *sudoku*
			 (+ y (* square-y *square-size*))
			 (+ x (* square-x *square-size*))))
	(setf (aref *sudoku*
		    (+ y (* square-y *square-size*))
		    (+ x (* square-x *square-size*)))
	      (remove num (aref *sudoku*
				(+ y (* square-y *square-size*))
				(+ x (* square-x *square-size*)))))))))

(defun init-possibilities ()
  (every-cell *size*
    (when (numberp (aref *sudoku* y x))
      (remove-possibilities x y))))

(defun sudoku-solved-p ()
  (let ((solved t))
    (every-cell *size*
      (when (listp (aref *sudoku* y x))
	(setf solved nil)
	(return)))
    solved))

(defun single-value-solver-loop ()
  (do ((changed t))
      ((null changed))
    (setf changed (single-value-solver))))

(defun single-value-solver ()
  (let ((changed nil))
    (every-cell *size*
      (when (and (listp (aref *sudoku* y x)) (eql (length (aref *sudoku* y x)) 1))
	(setf (aref *sudoku* y x) (car (aref *sudoku* y x)))
	(remove-possibilities x y)
	(setf changed t)))
    changed))

(defun single-position-solver ()
  (let ((changed nil))
    (when (single-position-in-line)
      (single-value-solver)
      (setf changed t))
    (when (single-position-in-col)
      (single-value-solver)
      (setf changed t))
;    (single-position-in-square)
;   (single-value-solver)))
    changed))

(defun single-position-in-line ()
  (let ((changed nil))
    (dotimes (y *size*)
      (when (single-position-in-line* y) (setf changed t)))
    changed))

(defun single-position-in-line* (line)
  (let ((changed nil))
    (dolist (num (list-numbers *size*))
      (let ((cnt 0))
	(dotimes (x *size*)
	  (when (listp (aref *sudoku* line x))
	    (when (member num (aref *sudoku* line x))
	      (incf cnt))))
	(when (= cnt 1)
	  (let ((x (first-possibility-in-line num line)))
	    (setf (aref *sudoku* line x) num)
	    (remove-possibilities x line)
	    (setf changed t)))))
    changed))

(defun single-position-in-col ()
  (let ((changed nil))
    (dotimes (x *size*)
      (when (single-position-in-col* x) (setf changed t)))
    changed))

(defun single-position-in-col* (col)
  (let ((changed nil))
    (dolist (num (list-numbers *size*))
      (let ((cnt 0))
	(dotimes (y *size*)
	  (when (listp (aref *sudoku* y col))
	    (when (member num (aref *sudoku* y col))
	      (incf cnt))))
	(when (= cnt 1)
	  (let ((y (first-possibility-in-col num col)))
	    (setf (aref *sudoku* y col) num)
	    (remove-possibilities col y)
	    (setf changed t)))))
    changed))

(defun single-position-in-square ()
  nil)


(defun first-possibility-in-line (num line)
  (let ((pos -1))
    (dotimes (x *size*)
      (when (listp (aref *sudoku* line x))
	(when (member num (aref *sudoku* line x))
	  (setf pos x)
	  (return))))
    (if (not (eql pos -1))
	pos
	nil)))

(defun first-possibility-in-col (num col)
  (let ((pos -1))
    (dotimes (y *size*)
      (when (listp (aref *sudoku* y col))
	(when (member num (aref *sudoku* y col))
	  (setf pos y)
	  (return))))
    (if (not (eql pos -1))
	pos
	nil)))


(defun num-in-cell-p (num col line)
  (eql (aref *sudoku* line col) num))

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


