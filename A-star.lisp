;;Extends the beginning element in the path. It's fed the least cost path
;;=======================================================================

(defun extnd (path map)
  "Given a partial path extends the partial paths with the front most coordinate's neighbours
and returns a list of extended partial paths. It eliminates any cycles formed."
  (mapcar #'(lambda (new-node) (append (list new-node) path))
	  (remove-if #'(lambda (neighbour)
			 ;;can use member and use the :test to define a procedure
			 (find-if #'(lambda (neighbor)
				      (and (= (first neighbor) (first neighbour))
					   (= (second neighbor) (second neighbour))))
				  path))
		     (find-neighbours (first path) map))
  ))


;;Distance heuristic used is L1 distance (MACRO)
;;==============================================

(defmacro distance-heuristic (node-1 node-2)
  "Distance heuristic used for A* is manhattan distance."
  `(+ (abs (- (first ,node-1) (first ,node-2)))
     (abs (- (second ,node-1) (second ,node-2)))
     ))

;;Predicate condition for the sort. Sort with respect to cost+heuristic
;;=====================================================================

(defun closerp (path-1 path-2 target)
  "Given paths returns true if the front most elements of the paths is closer to the target by
adding the actual cost (third element) and manhattan distance. Follows triangle equality. It's
as a comparison for sorting list of paths."
  (< (+ (first (last (first path-1))) (distance-heuristic (first path-1) target))
     (+ (first (last (first path-2))) (distance-heuristic (first path-2) target))))
     

;;A-star implementation. Returns the path if exists
;;=================================================

(defun A-star (ant food map time &optional (queue (list (list ant))))
  (cond ((endp queue) 'nil)
	;;quit if target found
	((and (= (first food) (first (first (first queue))))
	      (= (second food) (second (first (first queue)))))
	 (rest (reverse (first queue))))
	;;quit if A* runs more than 250 milliseconds.
	((> (float (/ (- (get-internal-real-time) time) internal-time-units-per-second)) .250)
	 (rest (reverse (first queue))))
	(t (A-star ant
		   food
		   map
		   time
		   (sort (append (extnd (first queue) map)
				 (rest queue))
			 #'(lambda (p1 p2) (closerp p1 p2 food)))))
	   ))





