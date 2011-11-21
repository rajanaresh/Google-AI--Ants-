;;Finds the neighbours of a given node and increses the neighbour cost by current+1
;;=================================================================================

(defun find-neighbours (node map)
  (remove-if #'(lambda (pos) (equal (aref map (first pos) (second pos)) 'w))
	     (list (list (first node) (- (second node) 1) (+ (first (last node)) 1))
		   (list (+ (first node) 1) (second node) (+ (first (last node)) 1))
		   (list (first node) (+ (second node) 1) (+ (first (last node)) 1))
		   (list (- (first node) 1) (second node) (+ (first (last node)) 1)))
	     ))

;;Extends the beginning element in the path. It's fed the least cost path
;;=======================================================================

(defun extnd (path map)
  (mapcar #'(lambda (new-node) (append (list new-node) path))
	  (remove-if #'(lambda (neighbour)
			 (find-if #'(lambda (neighbor)
				      (and (= (first neighbor) (first neighbour))
					   (= (second neighbor) (second neighbour))))
				  path))
		     (find-neighbours (first path) map))
  ))


;;Distance heuristic used is L1 distance (MACRO)
;;==============================================

(defmacro distance-heuristic (node-1 node-2)
  `(+ (abs (- (first ,node-1) (first ,node-2)))
     (abs (- (second ,node-1) (second ,node-2)))
     ))

;;Predicate condition for the sort. Sort with respect to cost+heuristic
;;=====================================================================

(defun closerp (path-1 path-2 target)
  (< (+ (first (last (first path-1))) (distance-heuristic (first path-1) target))
     (+ (first (last (first path-2))) (distance-heuristic (first path-2) target))))
     

;;A-star implementation. Returns the path if exists
;;=================================================

(defun A-star (ant food map time &optional (queue (list (list ant))))
  (cond ((endp queue) 'nil)
	((and (equal (first food) (first (first (first queue))))
	      (equal (second food) (second (first (first queue)))))
	 (rest (reverse (first queue))))
	((> (float (/ (- (get-internal-real-time) time) internal-time-units-per-second)) .300)
	 (rest (reverse (first queue))))
	(t (A-star ant
		   food
		   map
		   time
		   (sort (append (extnd (first queue) map)
				  (rest queue))
			  #'(lambda (p1 p2) (closerp p1 p2 food)))))
	   ))





