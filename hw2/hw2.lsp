


; Takes a search TREE
; If the first value isn't an atom, append it to the 
; end of the list with one less set of parentheses. Append the values
; of atoms to the return list, and continue recursing/unwrapping values.
; This will make it so that deeper values are appended to the final list 
; at later times, so you are only addeing certain values at the current depth.
; Returns a list of the leaf values of TREE in the BFS algorithm search order

(defun BFS (TREE)
	(cond 
		((null TREE) nil)
		((atom TREE)  (List TREE))
		(t 
			(cond
				((atom (first TREE)) 
					(append (BFS  (first TREE)) (BFS(rest TREE)))
				)
				(t (BFS (append (rest TREE) (first TREE))))

			)
		)
	)
)


; Takes in a search TREE.
; Recusively iterates through a single branch of TREE by 
; unwrapping the left most values of the nested list. Recusively
; does this throughout the list, and recusively appends these values
; to a list.
; Returns a LIST of all the leaf values of the TREE found in the DFS
; algorithm search order

(defun DFS (TREE)
	(cond 
		((null TREE) nil)
		((atom TREE) (List TREE))
		(t (append (DFS (rest TREE)) (DFS (first TREE))))
	)
)

; Helper function for DFID takes in a search TREE and a depth value.
; Recusively goes through each layer of the tree, and appends all the valeues
; from a specific depth of a TREE to a list.
; Returns a LIST of all the leaf values of TREE from a specific depth.

(defun helper (TREE depth)
	(cond
		((or (null TREE) (< depth 0)) ())
		((and (or(= depth 0) (> depth 0))  (atom   TREE)) (List  TREE)) 
		(t  (append (helper (first TREE) (- depth 1)) (helper (rest TREE) (- depth 0)) ))
	)
)

; Takes in a search TREE and a max depth value of the TREE.
; Recusively prepend the values of the helper function from 
; the depth value to 0. In this way, each recusive call prepends
; a list of all the leaves that are at least of the depth that is
; currently being found. 
; Returns a LIST with all the leaves in a possible repeating order, to
; follow the DFID search algorithm

(defun DFID (TREE depth)
    (cond ((or (< depth 0) (null TREE)) ())
          (t 
          	(append (dfid TREE (- depth 1)) (helper TREE depth))
          )
	)
)






; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
	(cond
		((and (= (first s) 3) (= (second s) 3) (null (third s))) T)
		(t nil)
	)
)
; (print (final-state '(3 3 NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
	(let* ((miss (first s)) (cann (second s)) (otherMiss (- 3 miss)) (otherCann (- 3 cann))) 
		(cond
			((and (= m 0) (= c 0)) nil)
			((or (< (- miss m) 0)  (< (- cann c) 0)) nil)
			;other side after
			((and (< (+ otherMiss m) (+ otherCann c)) (not (= 0 (+ otherMiss m)))  ) nil)
			;current side after
			((and(< (- miss m) (- cann c))  (and  (not (= 0 (- miss m))) (not (= 0 (+ miss m))) )  ) nil)
			(t 
				(cond 
					((null (third s))
						(List (List (+ m otherMiss)  (+ c otherCann) T))
					)
					(t
						(List (List (+ m otherMiss)  (+ c otherCann) nil))
					)
				)
			)


		)
	)
)


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
	(APPEND (next-state s 1 0) (next-state s 0 1) (next-state s 1 1) (next-state s 2 0) (next-state s 0 2))
)



; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
	(cond 
		((null states) nil)
		((equal (first states) s) t)
		(t (on-path s (rest states)))
	)
)


; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
	(cond 
        ((mc-dfs (first states) path) (mc-dfs (first states) path))
        ((not (NULL (rest states))) (mult-dfs (rest states) path))
        (t NIL)
    )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
	(cond 
        ((final-state s) (append (List s) path))
        ((on-path s path) NIL)
        (t (mult-dfs (succ-fn s) (append (List s) path)))
    )
)


; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))



