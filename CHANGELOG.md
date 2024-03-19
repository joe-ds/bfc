## 1.1.0

### Improvements
 - Added optimisation for runs of characters.
 - Changed *exit* to a function that accepts an optional exit code.
 - Changed *head* to a function that accepts the number of cells to allocate.
 - head now contains all the code needed before that of make-and-exec-runs.
 - Makefile now deletes the .nasm and .o files.

### Miscellaneous
 - Created CHANGELOG.md
 - Changed version numbering system.


## 1.0.1

### Fixed
 - Loops will correctly JMP if the current cell is empty.


## 1.0.0
 - Created working compiler.