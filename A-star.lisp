(defun find-neighbours (node map)
  (remove-if #'(lambda (pos) (equal (aref map (first pos) (second pos)) 'w))
	     (list (list (first node) (- (second node) 1) (+ (first (last node)) 1))
		   (list (+ (first node) 1) (second node) (+ (first (last node)) 1))
		   (list (first node) (+ (second node) 1) (+ (first (last node)) 1))
		   (list (- (first node) 1) (second node) (+ (first (last node)) 1)))
	     ))

(defun extnd (path map)
  (mapcar #'(lambda (new-node) (append (list new-node) path))
	  (remove-if #'(lambda (neighbour)
			 (find-if #'(lambda (neighbor)
				      (and (= (first neighbor) (first neighbour))
					   (= (second neighbor) (second neighbour))))
				  path))
				  (find-neighbours (first path) map))
	  ))

(defun A-star (ant food map &optional (queue (list (list ant))))
  (cond ((endp queue) 'nil)
	((and (equal (first food) (first (first (first queue))))
	      (equal (second food) (second (first (first queue)))))
	 (rest (reverse (first queue))))
	(t (A-star ant
		   food
		   map
		   (sort (append (extnd (first queue) map)
				  (rest queue))
			  #'(lambda (p1 p2) (closerp p1 p2 food)))))
	   ))


(defun closerp (path-1 path-2 target)
  (< (+ (first (last (first path-1))) (distance-heuristic (first path-1) target))
     (+ (first (last (first path-2))) (distance-heuristic (first path-2) target))))
     

;;Distance heuristic used is L1 distance
;;======================================

(defun distance-heuristic (node-1 node-2)
  (+ (abs (- (first node-1) (first node-2)))
     (abs (- (second node-1) (second node-2)))
     ))


(setf map (make-array '(10 10) :initial-element '0))
(setf (aref map 6 4) 'w)
(setf (aref map 6 5) 'w)
(setf (aref map 5 6) 'w)
(setf (aref map 4 6) 'w)
(setf (aref map 3 6) 'w)
