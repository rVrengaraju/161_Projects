 ;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))


;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

(load-a-star)

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;


; takes in a row in the form of a 1-D list
; recursively iterates over each element until we either
; find a keeper or reach the end of the list without finding a keeper
; return T if the keeper is found, otherwise return nil
(defun checkRow (r)
	(cond
		((null r) nil)
		(t 
			(if (or (isKeeper (first r)) (isBox (first r)))
				t
				(checkRow (rest r))
			)
				
		)
	)
)

; pass in a state as a 2-d list and recursively iterate over
; the different nested lists. Pass each nested list into the checkRow
; function, which returns whether there's a keeper within a row. If a 
; keeper is ever found, return nil, otherwise return T
(defun goal-test (s)
	(cond
		((null s) t)
		(t 
			(if (equal (checkRow (first s)) t)
				nil
				(goal-test (rest s))
			)
		)
	)
  )





; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;


; given a list and a specific index, returns the value of the element
; at that index. If it index is out of range, returns a 1 to represent 
; a wall
(defun indexVal(lst index)
	(cond
		((< index 0) 1)
		((and (= index 0) (not (null lst))) (first lst))
		(t (indexVal (rest lst) (- index 1)))
	)
)

; given a state and (row, col) pairing, recursively iterates over the rows
; until the proper row is found. Uses indexVal to find the spcific value at the proper
; column within the row, and returns that value
; Returns an integer
(defun get-square(s row col)
	(cond
		((< row 0) 1)
		((and (= row 0) (not (null s))) (indexVal (first s) col))
		(t (get-square (rest s) (- row 1) col))
	)
)

; given a list, returns a new list with the same contents
; excpet at the specific index, which now has the new value v.
; Returns the new list
(defun setList (lst index v)
	(cond
		((null lst) nil)
		((= 0 index) (append (List v) (setList (rest lst) (- index 1) v)) )
		(t (append (List (first lst)) (setList (rest lst) (- index 1) v)) )
	)
)

; Given a state, (row, col) pairing and new value v
; Finds the right column to feed to setList, which in turn returns
; the new list, while appending all the unaltered lists to create a new
; state
; returns 2-D array
(defun set-square(s row col v)
	(cond
		((null s) nil)
		((= row 0) (append (List (setList (first s) col v)) (set-square (rest s) (- row 1) col v)))
		(t (append (List(first s)) (set-square (rest s) (- row 1) col v)))
	)
)

; given a state and transition values, calals set-square
; on these values and returns a new state
(defun modifyHelper(s trans)
	(set-square s (first trans) (second trans) (third trans))
)

; Given a current state and a list of new transitions for that state
; Recursively iterates through the list of transition values and then
; applies each transition on a state, and applies the new transition on the
; newly returned state. Does this until all transitions are completed in the list
; Returns a new 2-D state
(defun modify(s transList)
	(cond
		((null transList) s)

		(t (modify (modifyHelper s (first transList)) (rest transList)))
	)
)

; Specific for a keeper moving up, given a state and keeper position
; Checks multiple cases for if the keeper can move in this direction, and modifies
; the current state if the keeper can actually move in this direction
; Returns a new state with keeper and all relative values in their new positions
(defun moveUp(s pos)
	(let* (
		(row (second pos))
		(col (first pos))
		(keeperSwap 
			(cond
				((isKeeper (get-square s row col)) 0)
				(t 4)
			)
		)
	)

		(cond
			((isBlank (get-square s (- row 1) col)) (modify s (List (List row col keeperSwap) (List (- row 1) col 3))))
			((isStar (get-square s (- row 1) col)) (modify s (List (List row col keeperSwap) (List (- row 1) col 6))))
			((isBox (get-square s (- row 1) col)) 
				(cond
					((isBlank (get-square s (- row 2) col)) (modify s (List (List row col keeperSwap) (List (- row 1) col 3) (List (- row 2) col 2))))
					((isStar (get-square s (- row 2) col)) (modify s (List (List row col keeperSwap) (List (- row 1) col 3) (List (- row 2) col 5))))
					(t nil)
				)
			)
			((isBoxStar (get-square s (- row 1) col))
				(cond
					((isBlank (get-square s (- row 2) col)) (modify s (List (List row col keeperSwap) (List (- row 1) col 6) (List (- row 2) col 2))))
					((isStar (get-square s (- row 2) col)) (modify s (List (List row col keeperSwap) (List (- row 1) col 6) (List (- row 2) col 5))))
					(t nil)
				)
			)
			(t nil)
		)
		
	)
)


; Specific for a keeper moving down, given a state and keeper position
; Checks multiple cases for if the keeper can move in this direction, and modifies
; the current state if the keeper can actually move in this direction
; Returns a new state with keeper and all relative values in their new positions
(defun moveDown(s pos)
	(let* (
		(row (second pos))
		(col (first pos))
		(keeperSwap 
			(cond
				((isKeeper (get-square s row col)) 0)
				(t 4)
			)
		)
	)
		(cond
			((isBlank (get-square s (+ row 1) col)) (modify s (List (List row col keeperSwap) (List (+ row 1) col 3))))
			((isStar (get-square s (+ row 1) col)) (modify s (List (List row col keeperSwap) (List (+ row 1) col 6))))
			((isBox (get-square s (+ row 1) col)) 
				(cond
					((isBlank (get-square s (+ row 2) col)) (modify s (List (List row col keeperSwap) (List (+ row 1) col 3) (List (+ row 2) col 2))))
					((isStar (get-square s (+ row 2) col)) (modify s (List (List row col keeperSwap) (List (+ row 1) col 3) (List (+ row 2) col 5))))
					(t nil)
				)
			)
			((isBoxStar (get-square s (+ row 1) col))
				(cond
					((isBlank (get-square s (+ row 2) col)) (modify s (List (List row col keeperSwap) (List (+ row 1) col 6) (List (+ row 2) col 2))))
					((isStar (get-square s (+ row 2) col)) (modify s (List (List row col keeperSwap) (List (+ row 1) col 6) (List (+ row 2) col 5))))
					(t nil)
				)
			)
			(t nil)
		)
	)
)

; Specific for a keeper moving right, given a state and keeper position
; Checks multiple cases for if the keeper can move in this direction, and modifies
; the current state if the keeper can actually move in this direction
; Returns a new state with keeper and all relative values in their new positions
(defun moveRight(s pos)
	(let* (
		(row (second pos))
		(col (first pos))
		(keeperSwap 
			(cond
				((isKeeper (get-square s row col)) 0)
				(t 4)
			)
		)
	)
		(cond
			((isBlank (get-square s row (+ col 1))) (modify s (List (List row col keeperSwap) (List row (+ col 1) 3))))
			((isStar (get-square s row (+ col 1))) (modify s (List (List row col keeperSwap) (List row (+ col 1) 6))))
			((isBox (get-square s row (+ col 1))) 
				(cond
					((isBlank (get-square s row (+ col 2))) (modify s (List (List row col keeperSwap) (List row (+ col 1) 3) (List row (+ col 2) 2))))
					((isStar (get-square s row (+ col 2))) (modify s (List (List row col keeperSwap) (List row (+ col 1) 3) (List row (+ col 2) 5))))
					(t nil)
				)
			)
			((isBoxStar (get-square s row (+ col 1)))
				(cond
					((isBlank (get-square s row (+ col 2))) (modify s (List (List row col keeperSwap) (List row (+ col 1) 6) (List row (+ col 2) 2))))
					((isStar (get-square s row (+ col 2))) (modify s (List (List row col keeperSwap) (List row (+ col 1) 6) (List row (+ col 2) 5))))
					(t nil)
				)
			)
			(t nil)
		)
		
	)
)

; Specific for a keeper moving left, given a state and keeper position
; Checks multiple cases for if the keeper can move in this direction, and modifies
; the current state if the keeper can actually move in this direction
; Returns a new state with keeper and all relative values in their new positions
(defun moveLeft(s pos)
	(let* (
		(row (second pos))
		(col (first pos))
		(keeperSwap 
			(cond
				((isKeeper (get-square s row col)) 0)
				(t 4)
			)
		)
	)
		(cond
			((isBlank (get-square s row (- col 1))) (modify s (List (List row col keeperSwap) (List row (- col 1) 3))))
			((isStar (get-square s row (- col 1))) (modify s (List (List row col keeperSwap) (List row (- col 1) 6))))
			((isBox (get-square s row (- col 1))) 
				(cond
					((isBlank (get-square s row (- col 2))) (modify s (List (List row col keeperSwap) (List row (- col 1) 3) (List row (- col 2) 2))))
					((isStar (get-square s row (- col 2))) (modify s (List (List row col keeperSwap) (List row (- col 1) 3) (List row (- col 2) 5))))
					(t nil)
				)
			)
			((isBoxStar (get-square s row (- col 1)))
				(cond
					((isBlank (get-square s row (- col 2))) (modify s (List (List row col keeperSwap) (List row (- col 1) 6) (List row (- col 2) 2))))
					((isStar (get-square s row (- col 2))) (modify s (List (List row col keeperSwap) (List row (- col 1) 6) (List row (- col 2) 5))))
					(t nil)
				)
			)
			(t nil)
		)
		
	)
)

; Takes in a current state and directin
; Checks if the given direction is a valid move, and 
; returns nil if the move isn't, otherwise calls modify 
; on the state and returns a new state with the keeper having
; moved in the proper direction
(defun try-move (s dir)
	(cond
		((string= dir "UP")(moveUp s (getKeeperPosition s 0)))
		((string= dir "DOWN")(moveDown s (getKeeperPosition s 0)))
		((string= dir "LEFT")(moveLeft s (getKeeperPosition s 0)))
		((string= dir "RIGHT")(moveRight s (getKeeperPosition s 0)))
	)
)

;Given a state
; Returns all legal states from the moves "up", "down",
; "right", and "left"
; Returns a list of all possible new positions can move in 
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s "UP") (try-move s "RIGHT") (try-move s "DOWN") (try-move s "LEFT")))
	 )
    (cleanUpList result);end
   );end let
  );




; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.

; admissable heurisitic is set to zero
; we want the value of the heurisitic to be as close 
; to optimal solution, without overestimating it
(defun h0 (s)
	0
 )


; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;

; given a list, recursively iterates and counts how many
; boxes are foudn within the list
; returns the total number of boxes 
(defun boxesInRow(lst)
	(cond
		((null lst) 0)
		(t 
			(if (isBox (first lst))
				(+ 1 (boxesInRow (rest lst)))
				(+ 0 (boxesInRow (rest lst)))
			)
		)
	)
)

; Given a state
; Recursively passes each row to the boxesInRow function
; and adds up how many boxes are found in each row
; This heuristic is admissible!
(defun h1 (s)
	(cond
		((null  s) 0)
		(t (+ (boxesInRow (first s)) (h1 (rest s))))
	)
)


; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
;

; Takes in a row, (row, col) pairing, and either the isBox or isStar function
; Determines the coordinates of either all the goals or boxes in a list
; Returns a list of coordinates for either boxes or goals. 
(defun getGoalColumn (lst row col identifier)
	(cond
		((null lst) nil)
		((funcall identifier (first lst)) (append (List(List row col)) (getGoalColumn (rest lst) row (+ col 1) identifier)))
		(t (append () (getGoalColumn (rest lst) row (+ col 1) identifier)))
	)
)

; Given a state, starting row index, and either the isBox or isStar function
; Determines the coordinates of either all the boxes or goals in a state.
; Returns a list of coordinates for either boxes or goals 
(defun getPositions (s row identifier)
	(cond
		((null s) nil)
		(t (append (getGoalColumn (first s) row 0 identifier) (getPositions (rest s) (+ row 1) identifier)))
	)
)

; Given a single position of a box and a list of goal coordinates, run the
; manhattan distance calculation between the box and each coordinate to see which 
; pairing has the minimum value. And return that minimum value
(defun manhattanHelper (boxPos goalLst minVal)
	(let ((boxRow (first boxPos)) (boxCol (second boxPos)) (goalRow (first (first goalLst))) (goalCol (second (first goalLst)))  )
		(cond
			((null goalLst) minVal)
			(t
				(if (< (+ (abs(- boxRow goalRow)) (abs(- boxCol goalCol))) minVal)
					(manhattanHelper boxPos (rest goalLst) (+ (abs(- boxRow goalRow)) (abs(- boxCol goalCol))))
					(manhattanHelper boxPos (rest goalLst) minVal)
				)
			)
		)
	)

)

; Given a list of boxes and a list of goals, run the manhattan distance function
; on each box and add the values of each box. Return that added value
(defun manhattan (boxLst goalLst)
	(cond
		((null boxLst) 0)
		(t
			(+ (manhattanHelper (first boxLst) goalLst 100000000) (manhattan (rest boxLst) goalLst))
		)
	)
)

; Given a state
; Get a list of box coordinates and a list of goal coordinates and 
; put those into the manhattan function. Determine the manhattan distance
; for a state
; Return a number
(defun h404994022 (s)
	(manhattan (getPositions s 0 #'isBox) (getPositions s 0 #'isStar) )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 0)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(51)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(41)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(78)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(26)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
