

; takes in a value N and a an ordered TREE. 
; Recurively goes through each value of tree to
; to see if the value N is present within Tree
; either returns true if it is present, or false if it isn't

(defun TREE-CONTAINS (N TREE)
	(cond
		((null TREE) ())
		((atom TREE) 
			(cond
				((equal N TREE) t)
				(t ())
			)
		)
		(t (or (TREE-CONTAINS N (first TREE)) (TREE-CONTAINS N (rest TREE))))
	)
)


; Takes in an ordered TREE
; Recurively goes to the left most value
; and determiens the smallest value wihtin the ordered tree.
; Ordered trees will always hold the smallest value within left most value,
; so that's why I do this.
; Returns the smallest value when it is found

(defun TREE-MIN (TREE)
	(cond
		((atom (first TREE))  (first TREE))
		(t (TREE-MIN (first TREE)))
	)
)



; Takes in an ordered TREE
; Using the pre-order traversal algorithm, recurively appends to a 
; list the pre-ordered values of the TREE
; Returns the pre-ordered tree values in a list.

(defun TREE-ORDER (TREE)
	(cond 
		((null TREE) ())
		((atom TREE) (cons TREE ())) 
		(T (append (TREE-ORDER(second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))
	)
)


; Takes in a list L, starting index START, and length value LEN
; Recurively loops through list until the start index is found
; Recurively loops through the rest of list from start index until
; the LEN value is decremented to 0, all the while appending the traversed
; values to a list
; Returns a list of the values from START index with size LEN

(defun SUB-LIST (L START LEN)
	(cond

		((= LEN 0) ())
		((= START 0) (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))
		(t (SUB-LIST (rest L) (- START 1) LEN))
	)
)



; Takes in a list L
; Utilizing the SUB-LIST function, it cuts the given list in half
; If the list is odd, the greater portion goes in the first half.
; This can be done by giving the proper 1/2 parameters to the SUB-LIST
; function.
; Returns a list of two lists, the two lists are either the same size or the first 
; list is one element larger

(defun SPLIT-LIST(L)
	(cond
		((evenp (length L) )
			(List (SUB-LIST L 0 (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (/ (length L) 2)))
		)
		(t
			(List (SUB-LIST L 0 (+ (/ (length L) 2.0) 0.5)) (SUB-LIST L (+ (/ (length L) 2.0) 0.5) (- (/ (length L) 2.0) 0.5)))
		)
	)
)


; Takes a binary TREE.
; Recurively traverses the nodes of the tree and maintains the
; current length that it is at. Upon returing, it holds onto the larger size
; of depth by comparing depth values when recurively adding depth. 
; Returns an integer represetning the longest traversable distance from the root
; node to the furthest node in TREE.

(defun BTREE-HEIGHT(TREE)
	(cond
		((atom TREE) 0)
		(t 
			(cond
				( (> (+ (BTREE-HEIGHT (first TREE)) (/ 1 2)) (+ (BTREE-HEIGHT (rest TREE)) (/ 1 2)))
					 (+ (BTREE-HEIGHT (first TREE)) (/ 1 2))
				)
				(t (+ (BTREE-HEIGHT (rest TREE)) (/ 1 2)))
			)
		)
	)
)


; Takes in the LEAVES of a binary search tree
; Utilzi the SPLIT-LIST function to perform actions
; if the list is 1 or 2 sizes in length, a base case has been hit.
; We want to append those values to the recurive call and continue
; recursing through the rest of the list
; Returns a final nested list of lists, which is the binary tree representation
; of the given LEAVES. As a result, this list is a binary TREE.

(defun LIST2BTREE(LEAVES) 
	(cond
		((= (length LEAVES) 2) (List (first LEAVES) (second LEAVES)))
		((= (length LEAVES) 1) (first LEAVES))

		(t (List (LIST2BTREE (first (SPLIT-LIST LEAVES))) (LIST2BTREE (second (SPLIT-LIST LEAVES)))))
	)
)

; Takes in a TREE 
; Given the form of the TREE, I approached the solution by 
; thinking of how to flatten the list. Recurively looped through list
; until I came upon atoms, and appended them to the final list, and built 
; that up. Continued to recurse through list, and created final list.
; Returns a LIST of all the leaves of the TREE.

(defun BTREE2LIST (TREE)
	(cond 
		((null TREE) nil)
		((atom TREE) 
			(cond
				((numberp TREE) (List TREE))
				(t TREE)
			)
		)
		(t (APPEND (BTREE2LIST (first TREE)) (BTREE2LIST (rest TREE))))
	)

)


; Takes in two TREES.
; Recurively loop through all the values, until reaching the 
; leaves of at least one tree. If the other tree isn't at a leaf or
; the values are different, then there is a difference in TREEs. 
; If all the values of the TREEs are the same and within the same
; location, return true. Otherwise return NIL.
; Return either T or NIL

(defun IS-SAME (E1 E2)
	(cond 
		((atom E1) 
			(cond
				((and (numberp E2) (null E1)) nil)
				((and (numberp E1) (null E2)) nil)
				((and (null E2) (null E1)) t)
				((atom E2) 
					(if (= E2 E1) t)
				)
				(t nil)
			)
		)
		(t (and (IS-SAME(first E1) (first E2)) (IS-SAME(rest E1) (rest E2))))
	)
)

