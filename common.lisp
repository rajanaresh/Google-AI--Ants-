;;Finds the neighbours of a given node and increses the neighbour cost by current+1
;;=================================================================================

(defun find-neighbours (node map)
  "Returns a list of neighbouring coordinates with the format row, col and 1 added to the ants 
third element. This helps calculating the real cost in A*. It eliminates the neighbours which
are other friendly ants or water blocks."
  (remove-if #'(lambda (pos) (or (or (< (first pos) 0)
				     (> (first pos) (- (first (array-dimensions map)) 1)))
				 (or (< (second pos) 0)
				     (> (second pos) (- (second (array-dimensions map)) 1)))
				 (equal (aref map (first pos) (second pos)) 'w)
				 (equal (aref map (first pos) (second pos)) 'a)))
	     (list (list (first node) (- (second node) 1) (+ (first (last node)) 1))
		   (list (+ (first node) 1) (second node) (+ (first (last node)) 1))
		   (list (first node) (+ (second node) 1) (+ (first (last node)) 1))
		   (list (- (first node) 1) (second node) (+ (first (last node)) 1)))
	     ))

(defun issue-order (row col direction)
  "Prints a formatted order for ROW,COL and DIRECTION to standard output. Silently
drops orders when DIRECTION isn't one of north, east, south or west."
  (when (member direction '(north east south west))
    (format *standard-output* "~&o ~D ~D ~A~%" row col
            (case direction
              ('north "N")
              ('east  "E")
              ('south "S")
              ('west  "W")))))

(defun diff (busy all)
  "Takes the busy-list (which has busy ants) and everything-list (which has everything in it) 
and returns a list of  free ant's coordinates. This procedure is used to get the ants that
remain free in each turn."
  (remove-if #'(lambda (node) (member node
				      (mapcar #'(lambda (node) (first node))
					      busy) :test #'(lambda (p1 p2)
							            (and (= (first p1)
									    (first p2))
									 (= (second p1)
									    (second p2))))))
	     (second all)))


(defun direction (ant neigh)
  "Takes the coordinates of ant and the nieghbour and returns either north, south, west or east.
Used with issue-order in the game loop to print N, S, W or S."
  (cond ((> (first ant) (first neigh)) 'north)
	((< (first ant) (first neigh)) 'south)
	((> (second ant) (second neigh)) 'west)
	((< (second ant) (second neigh)) 'east)))

(defun distancep (i1 j1 i2 j2 threshold)
  "Checks if distance b/w the points (i1, j1) and (i2, j2) is less than the threshold. Returns 
true if it does."
  (< (sqrt (+ (expt (- i1 i2) 2)
	      (expt (- j1 j2) 2)))
     (+ 1 threshold)))

(defun closest (ant homes)
  "Given an ant coordinate  and a list of other coordinates (ant hills, enemy ants etc.) returns
the one which is closest to the ant."
  (first (sort homes #'(lambda (node1 node2) (< (sqrt (+ (expt (- (first ant) (first node1)) 2)
							 (expt (- (second ant) (second node1)) 2)))
						(sqrt (+ (expt (- (first ant) (first node2)) 2)
							 (expt (- (second ant) (second node2)) 2))))))))

(defun farthest (ant homes)
  "Similar to the above except returns the one which is farthest to the ant."
  (first (sort homes #'(lambda (node1 node2) (> (sqrt (+ (expt (- (first ant) (first node1)) 2)
							 (expt (- (second ant) (second node1)) 2)))
						(sqrt (+ (expt (- (first ant) (first node2)) 2)
							 (expt (- (second ant) (second node2)) 2))))))))