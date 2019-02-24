(defun solve-sudoku ()
  "User interface: use this function to solve sudoku. You will be prompted for file"
  (progn
    (init-sudoku-with-prompt)
    (solver)))

(defun solve-sudoku-file (file-name)
  "User interface: use this function to solve the sudoku defined in file file-name"
  (progn
    (init-sudoku-with-file file-name)
    (solver)))

(defun init-sudoku-with-file (file-name)
  "reads the sudoku from file and initializes the sudoku data structure for use"
  (let ((file-content (read-sudoku file-name)))
      (make-sudoku-array file-content)
      (replace-nils)
      (init-possibilities)))

(defun init-sudoku-with-prompt ()
  "prompts the user for file and initializes the sudoku (see init-sudoku-with-file"
  (init-sudoku-with-file (prompt-read)))

(defun solver ()
  "the solver loop. Main work is done using the single-value solver. When no further
reductions can be done, more complex solvers are included to solve the sudoku"
  (do ((has-changed t))
      ((not (and has-changed
		 (not (sudoku-solved-p)))))
    (setf has-changed nil)
    (single-value-solver-loop)
    (unless (sudoku-solved-p)
      (setf has-changed (single-position-solver)))
    (unless (or has-changed (sudoku-solved-p))
      (setf has-changed (possibility-reduction-solver))))
  (print-sudoku))
	

(defparameter *sudoku* nil "Sudoku array to work with")
(defparameter *size* 0 "size of Sudoku")
(defparameter *square-size* 0 "dimension of subsquare")

(defun print-sudoku ()
  "print sudoku currently just returns the sudoku-array"
  *sudoku*)

(defun prompt-read ()
  "ask the user for the file containing the sudoku as a s-expression"
  (format *query-io* "sudoku file (full path) : ")
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-sudoku (file)
  "read the sudoku file"
  (with-open-file (stream file)
    (read stream)))

(defun make-sudoku-array (sudoku-list)
  "convert the s-expression (list of lists) containing the sudoku to a two-dimensional array"
  (let* ((size (length sudoku-list))
	(square-size (truncate (sqrt size))))
    (setf *size* size)
    (setf *square-size* square-size)
    (setf *sudoku*
	  (make-array (list size size)
		      :initial-contents sudoku-list))))

(defmacro every-cell (max-index &body body)
  "this macro simplifies looping over all cells of the 2-dimensional array.
It gives access to the coordinates with the symbols X and Y"
  `(dotimes (x ,max-index)
    (dotimes (y ,max-index)
       ,@body)))

(defun replace-nils ()
  "replaces all nils from an initial sudoku file with a list of possibilities for this cell.
Note: there is not yet any reduction of possibilities, it's just the full set of numbers."
  (every-cell *size*
    (when (null (aref *sudoku* y x))
      (setf (aref *sudoku* y x) (list-numbers *size*)))))

(defun list-numbers (num)
  "create a list of numbers from 1 to the size of the sudoku"
  (list-numbers* num nil))

(defun list-numbers* (num numlist)
  "helper function to recursively build the number list"
  (if (> num 0)
      (list-numbers* (- num 1) (cons num numlist))
      numlist))
  
(defun remove-possibilities (x y)
  "remove the possibilities within the sudoku array for a number in a given cell"
  (let ((num (aref *sudoku* y x)))
    (remove-possibilities-in-line num y)
    (remove-possibilities-in-col num x)
    (remove-possibilities-in-square num x y)))

(defun remove-possibilities-in-line (num line)
  "remove all possibilities for a given number in a given line"
  (dotimes (x *size*)
    (when (listp (aref *sudoku* line x))
      (setf (aref *sudoku* line x) (remove num (aref *sudoku* line x))))))

(defun remove-possibilities-in-col (num col)
  "remove all possibilities for a given number in a given column"
  (dotimes (y *size*)
    (when (listp (aref *sudoku* y col))
      (setf (aref *sudoku* y col) (remove num (aref *sudoku* y col))))))

(defun remove-possibilities-in-square (num col line)
  "remove all possibilities for a given number in its square"
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
  "remove obsolete possibilities in a freshly initialized array with new possibility lists"
  (every-cell *size*
    (when (numberp (aref *sudoku* y x))
      (remove-possibilities x y))))

(defun sudoku-solved-p ()
  "check whether the sudokuo is already solved"
  (let ((solved t))
    (every-cell *size*
      (when (listp (aref *sudoku* y x))
	(setf solved nil)
	(return)))
    solved))

(defun single-value-solver-loop ()
  "use the single-value-solver as long as it successfully can procede"
  (do ((changed t))
      ((null changed))
    (setf changed (single-value-solver))))

(defun single-value-solver ()
  "basic solver: checks all cells, if there is only one possibility left in the possibility list. If so, replaces that one possibility with that number and cares for the removal of that number out of the corresponding possibility lists"
  (let ((changed nil))
    (every-cell *size*
      (when (and (listp (aref *sudoku* y x)) (eql (length (aref *sudoku* y x)) 1))
	(setf (aref *sudoku* y x) (car (aref *sudoku* y x)))
	(remove-possibilities x y)
	(setf changed t)))
    changed))

(defun single-position-solver ()
  "intermediate solver: checks all possibilities within line / column / square if for a given number only one position to be exists and replaces the possibility list with that number. Cares for the possibility reduction based on that new number"
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
  "checks all lines for single position of possibilities"
  (let ((changed nil))
    (dotimes (y *size*)
      (when (single-position-in-line* y) (setf changed t)))
    changed))

(defun single-position-in-line* (line)
  "checks for a given line, if a possible number can be only in one position. Sets that number and cares for possibility reduction"
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
  "checks all columns for single position of possibilities"
  (let ((changed nil))
    (dotimes (x *size*)
      (when (single-position-in-col* x) (setf changed t)))
    changed))

(defun single-position-in-col* (col)
  "checks for a given linecolumn, if a possible number can be only in one position. Sets that number and cares for possibility reduction"
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
  "checks of a number can only be placed in one possition within a square.
-- currently unimplemented, as not needed --"
  nil)


(defun first-possibility-in-line (num line)
  "helper function to find the first occurance of a number as possibility in a line"
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
  "helper function to find the first occurance of a number as possibility in a column"
  (let ((pos -1))
    (dotimes (y *size*)
      (when (listp (aref *sudoku* y col))
	(when (member num (aref *sudoku* y col))
	  (setf pos y)
	  (return))))
    (if (not (eql pos -1))
	pos
	nil)))

(defun possibility-reduction-solver ()
  "complex solver: checks the remaining possibilities per square. If a number can only be placed in a single line / column, this possibility can be removed in that same line / column in all other squares"
  (every-cell *square-size*
    (let ((offset-x (* x *square-size*))
	  (offset-y (* y *square-size*)))
      (analyze-and-reduce-possibilities offset-x offset-y)))
  t)

(defun analyze-and-reduce-possibilities (offset-x offset-y)
  "this function collects all possible positions of a number within one square. It then calls to analyze the positions and to possibly remove possibilities based on the analyzation"
  (dolist (num (list-numbers *size*))
    (let ((places nil))
      (every-cell *square-size*
	(when (listp (aref *sudoku* (+ y offset-y) (+ x offset-x)))
	  (when (member num (aref *sudoku* (+ y offset-y) (+ x offset-x)))
	    (setf places (cons (list x y) places)))))
      (let ((reduction (analyze-places places nil)))
	(unless (null reduction)
	  (reduce-possibilities num reduction offset-x offset-y))))))

(defun analyze-places (places coord)
  "based on the possible positions of a number analyzes if that number can only be placed in one line or column. If so, returns a pair containing the relative coordinate of the line / column and a placeholder for the unbound coordinate"
  (if (null places)
      coord
      (if (null coord)
	  (analyze-places (cdr places) (car places))
	  (cond ((eql (caar places) (car coord))
		 (analyze-places (cdr places) (list (car coord) 'y)))
		((eql (cadar places) (cadr coord))
		 (analyze-places (cdr places) (list 'x (cadr coord))))
		(t nil)))))

(defun reduce-possibilities (num reduction offset-x offset-y)
  "the reduction parameter contains a pair of a bound and a unbound coordinate for a given number. It then cares for the reduction of possibilities of the number in the other squares. Offset is the square that needs to kept unchanged"
  (cond ((symbolp (car reduction))
	 (reduce-in-line num (+ (cadr reduction) offset-y) offset-x))
	((symbolp (cadr reduction))
	 (reduce-in-row num (+ (car reduction) offset-x) offset-y))
	(t nil)))

(defun reduce-in-line (num line offset-x)
  "remove possibilities of num in line in all squares but the offset-square"
  (let ((no-gos
	 (mapcar #'(lambda (n) (+ n offset-x)) (mapcar #'1- (list-numbers *square-size*)))))
    (dotimes (x *size*)
      (unless (member x no-gos)
	(when (listp (aref *sudoku* line x))
	  (setf (aref *sudoku* line x) (remove num (aref *sudoku* line x))))))))

(defun reduce-in-row (num col offset-y)
  "remove possibilities of num in col in all squares but the offset-square"
  (let ((no-gos
	 (mapcar #'(lambda (n) (+ n offset-y)) (mapcar #'1- (list-numbers *square-size*)))))
    (dotimes (y *size*)
      (unless (member y no-gos)
	(when (listp (aref *sudoku* y col))
	  (setf (aref *sudoku* y col) (remove num (aref *sudoku* y col))))))))


