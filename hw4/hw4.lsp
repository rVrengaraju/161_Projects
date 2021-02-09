;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists



; Given a specific literal from a clause, checks
; whether it isn't present in currValues -> returns t
; or does exist -> returns t
; otherwise, returns nil if the - value of it is found
(defun validLitral(currLit currValues)
  (cond
    ((null currValues) t)
    ((= (- 0 (first currValues)) currLit) nil)
    ((= (first currValues) currLit) t)
    (t (validLitral currLit (rest currValues))) 
  )
)


; Given a specific clause from the Delta and a set of values
; built through the driving function, check each literal within the
; clause by calling validLitral on it, and verify that the literal either
; exists within the currValues set, or doesn't exist
; returns t or nil
(defun validClause(currClause currValues)
  (cond
    ((null currClause) nil)
    (t
      (if (validLitral (first currClause) currValues)
        t
        (validClause (rest currClause) currValues)
      )
    )
  )
)

; given a delta, iterate through the values in Delta and call
; validClause on each one, while ANDing them together. This is to
; ensure that we maintain the ability to have the anded clauses in
; CSF

; Return t of nil

(defun validDelta(delta currValues)
  (cond
    ((null delta) t)
    (t
      (and (validClause(first delta) currValues) (validDelta (rest delta) currValues))
    )
  )
)

; Goes through and checks for each negative and positive value for all
; possible domains, and returns only the one that works

(defun backtrack(ind end delta currValues)
  (cond
    ((and (> ind end) (validDelta delta currValues)) currValues)
      
    ((or (> ind end) (null (validDelta delta currValues))) nil)
    (t 
      (or
        (backtrack (+ ind 1) end delta (append currValues (List ind)) )
        (backtrack (+ ind 1) end delta (append currValues (List (* ind -1))))
      )
    )
  )
)

; calls backtrack
(defun sat? (n delta) 
    (backtrack 1 n delta '())
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (defun split-line (line)
;   (if (equal line :eof)
;       :eof
;       (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

; (defun read-cnf (filename)
;   (with-open-file (in filename)
;     (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
;       if (equal 'p (first line)) collect (third line)      ; var count
;       if (integerp (first line)) collect (butlast line)))) ; clause

; (defun parse-cnf (filename)
;   (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; ; Following is a helper function that combines parse-cnf and sat?
; (defun solve-cnf (filename)
;   (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))



