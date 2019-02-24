# Sudolv - a Sudoku solver written in Common Lisp

I wrote this tiny tool to get used to Common Lisp.

The sudoku solver is currently tested using 9x9 sudokus only. But in general it should work with any square sizes of n\*n such as 4x4, 9x9, 16x16,...

## Usage

After loading Sudolv to a Common Lisp REPL you may start the sudoku solver either using the function (solve-sudoku), which then prompts for a sudoku file or (solve-sudoku-file file-name), where file-name is a string containing the path to a sudoku file.

## Sudoku file

The sudoku file may be any text file, that contains one s-expression describing the sudoku. This means, the sudoku is a list, containing lists of line descriptions. Any given number is defined as a number and every free cell is defined as nil. It should look like this:

((nil   2 nil nil)  
 (  1 nil   2 nil)  
 (  3 nil nil nil)  
 (nil   4 nil nil))

## Limitations

I currently implemented only solvers that analytically solve the sudoku. I know there are sudokus that lead to a situation, where no analytical solution exists and you need to try, if the sudoku is solvable setting a number to a possible open place. As I haven't had such a sudoku at hand I couldn't implement this kind of solver yet.

## What to do with an unsolvable sudoku?

If you find a sudoku that isn't solvable using this solver or the solver find's a wrong solution, I'd be happy if you drop me a note and attaching the sudoku. I then can further look into the solution strategy.

## Comments about the code

If you find the code ugly, complicated or have other arguments, please let me know. As this is my first "productive usage" of Common Lisp I'd be happy to learn from experienced Lispers how to write more Lispy code 
