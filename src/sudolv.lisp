(defun prompt-read ()
  (format *query-io* "sudoku file (full path) : ")
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-sudoku (file)
  (with-open-file (stream file)
    (read stream)))
