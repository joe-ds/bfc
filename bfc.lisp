;;;; +_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_
;;;;
;;;; bfc v1.1.0
;;;;
;;;; Author:  Joe Peterson
;;;;    URL:  gitlab.com/l3mu
;;;; License: GNU GPL v3
;;;;
;;;; Description:
;;;;   A bf compiler created in Common Lisp.
;;;;
;;;;   Tested in GNU CLISP 2.49.93+ and SBCL 1.4.14-2.fc30 with
;;;;   NASM version 2.13.03 and GNU ld version 2.31.1-29.fc30 on Fedora 30.
;;;;
;;;; +_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_

;;; *count* and *stack*
;;; These are used to create unique labels for the loops.

(defparameter *count* 0)
(defparameter *count-stack* '())

;;; *head*
;;; Code to be present at the start of every program. The input 'memory'
;;; determines exactly how many one-byte cells are allocated.

(defun header (memory)
  (list "global _start" "section .data"
	(format NIL "memory: times ~a db 0" memory)
	"section .text" "_start:" "xor r8, r8"))

;;; *exit* is code to perform exactly what it says on the tin. The input
;;; 'code' will be the exit code.

(defun exit-call (&optional (code 0))
  (list "mov rax, 60"
	(if (> code 0)
	    (format NIL "mov rdi, ~a" code)
	  "xor rdi, rdi")
	"syscall"))

;;; inc-dec-pc takes care of runs of '>' and '<' and changes the value of r8
;;; accordingly.
;;; Note that if the '>' and '<' cancel out then no code is generated.

(defun inc-dec-pc (s)
  (let ((n (apply '+ (map 'list #'(lambda (x) (if (eq x #\>) 1 -1)) s))))
    (case n (0 ())
	  (1 (format t "~a~%" "inc r8"))
	  (-1 (format t "~a~%" "dec r8"))
	  (otherwise (format t "add r8, ~a~%" n)))))

;;; inc-dec-cell works exactly the same way as inc-dec-pc but changes the value
;;; of [memory+r8] instead.

(defun inc-dec-cell (s)
  (let ((n (apply '+ (map 'list #'(lambda (x) (if (eq x #\+) 1 -1)) s))))
    (case n (0 ())
	  (1 (format t "~a~%" "inc byte [memory+r8]"))
	  (-1 (format t "~a~%" "dec byte [memory+r8]"))
	  (otherwise (format t "add byte [memory+r8], ~a~%" n)))))

;;; show-out outputs the current cell, i.e. executes bf's '.' command. The code
;;; is optimised for multiple runs of '.'.

(defun show-out (s)
  (format t "~{~a~%~}" '("mov rax, 1" "mov rdi, 1" "lea rsi, [memory+r8]"
			 "mov rdx, 1"))
  (format t "~{~a~%~}" (make-list (length s) :initial-element "syscall")))

;;; read-in works the exact same way as show-out and executes bf's ',' command.
;;; While it is less likely that any code would want a run of ',', the code
;;; is optimised and will still be executed.

(defun read-in (s)
  (format t "~{~a~%~}" '("xor rax, rax" "xor rdi, rdi" "lea rsi, [memory+r8]"
			 "mov rdx, 1"))
  (format t "~{~a~%~}" (make-list (length s) :initial-element "syscall")))

;;; Loops

;;; new-label() generates a new and unique label and adds it to the stack.
;;; It returns code for the beginning of a loop, and adds in a conditional
;;; jump in case the cell is empty.

(defun loop-start ()
  (setf *count* (1+ *count*))
  (push *count* *count-stack*)
  (format t "~{~a~%~}" (list "xor rcx, rcx" "mov cl, [memory+r8]" "test rcx, rcx"
			     (format NIL "jz skip~a" *count*)
			     (format NIL "loop~a:" *count*))))

;;; loop-end() adds a conditional jump back up to the last label, and adds
;;; in a label for new-label() to jump to if it finds the current cell is
;;; empty.

(defun loop-end ()
  (let ((count (pop *count-stack*)))
    (format t "~{~a~%~}" (list "xor rcx, rcx" "mov cl, [memory+r8]" "test rcx, rcx"
			       (format NIL "jnz loop~a" count)
			       (format NIL "skip~a:" count)))))

;;; check-family is a helper that classifies if the current char changes the
;;; value of the program counter, current cell or is a loop or I/O instruction.
;;; It is used in make-and-exec-runs below.

(defun check-family (c last)
  (or
   (eq c last)
   (and (find c "+-") (find last "+-"))
   (and (find c "<>") (find last "<>"))))

;;; This performs a simple one-to-one mapping on the given run and calls the
;;; relevant functions. translator is a helper for make-and-exec-runs.

(defun translator (s)
  (let ((c (car s)))
    (case c (#\> (inc-dec-pc s))
	  (#\< (inc-dec-pc s))
	  (#\+ (inc-dec-cell s))
	  (#\- (inc-dec-cell s))
	  (#\[ (loop-start))
	  (#\] (loop-end))
	  (#\. (show-out s))
	  (#\, (read-in s)))))

;;; make-and-exec-runs helps optimise the code by slicing it up into runs of
;;; '+' and '-', '>' and '<', '.', ',', or individual instances of '[' and ']'.

(defun make-and-exec-runs (code)
  (let ((last NIL))
    (loop for c across code
	  if (check-family c (car last))
	  do (push c last)
	  else do
	  (progn
	    (translator last)
	    (setq last (list c)))
	  finally (translator last))))

;;; This is where the calls to generate the final NASM code are made.

(defun cc (line)
  (format t "~{~a~%~}" (header 32))
  (make-and-exec-runs line)
  (format t "~{~a~%~}" (exit-call)))

(with-open-file (*standard-output* "bf_out.nasm" :direction :output
                                   :if-exists :supersede)
		(cc (read-line)))
