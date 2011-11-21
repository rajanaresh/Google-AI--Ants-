(load "A-star.lisp")

;;Reading the beginning game parameters such as turntime, loadtime, rows, cols etc.
;;=================================================================================

(defvar *parameters*)

(defun read-game-parameters ()
  (do ((buf 'nil))
      ((string-equal buf "ready") 'end)
      (setf buf (read-line))
      (cond ((search "turntime" buf)
	     (setf (get *parameters* 'turntime) (parse-integer buf :start 9)))
	    ((search "turns" buf)
	     (setf (get *parameters* 'turns) (parse-integer buf :start 6)))
	    ((search "turn" buf)
	     (setf (get *parameters* 'turn) (parse-integer buf :start 5)))
	    ((search "loadtime" buf)
	     (setf (get *parameters* 'loadtime) (parse-integer buf :start 9)))
	    ((search "rows" buf)
	     (setf (get *parameters* 'rows) (parse-integer buf :start 5)))
	    ((search "cols" buf)
	     (setf (get *parameters* 'cols) (parse-integer buf :start 5)))
	    ((search "viewradius" buf)
	     (setf (get *parameters* 'viewradius) (parse-integer buf :start 11)))
	    ((search "attackradius" buf)
	     (setf (get *parameters* 'attackradius) (parse-integer buf :start 13)))
	    ((search "spawnradius" buf)
	     (setf (get *parameters* 'spawnradius) (parse-integer buf :start 12)))
	    ((search "player_seed" buf)
	     (setf (get *parameters* 'player-seed) (parse-integer buf :start 12))))
      ))


;;Reading the game input on each turn
;;===================================

(defun read-input ()
  (let ((food-list 'nil)
       (ant-list-own 'nil)
       (ant-list-ene 'nil)
       (water-list 'nil)
       (hill-list-own 'nil)
       (hill-list-ene 'nil))
    (do ((buf 'nil))
	((string-equal buf "go") 'end)
	(setf buf (read-line))
	(cond ((search "f" buf)
	       (setf food-list (append food-list (list (string-to-coord buf)))))
	      ((search "a" buf)
	       (if (= 0 (first (reverse (string-to-coord buf))))
		   (setf ant-list-own (append ant-list-own
					      (list (string-to-coord buf))))
	           (setf ant-list-ene (append ant-list-ene
					      (list (string-to-coord buf))))))
	      ((search "w" buf) (setf water-list (append water-list
							 (list (string-to-coord buf)))))
	      ((search "h" buf)
	       (if (= 0 (first (reverse (string-to-coord buf))))
		   (setf hill-list-own (append hill-list-own
					       (list (string-to-coord buf))))
	           (setf hill-list-ene (append hill-list-ene
					       (list (string-to-coord buf)))))))
	)
    (list food-list ant-list-own ant-list-ene water-list hill-list-own hill-list-ene)))
      

;;Check if the given string is an numeric digit (MACRO)
;;=====================================================

(defmacro check-number (string)
  `(search ,string "0123456789"))


;;Converting the string input to a list of coordinates
;;====================================================

(defun string-to-coord (buf)
  (do ((count 0 (+ count 1))
       (index 0)
       (coord 'nil))
      ((< (- (length buf) 1) count) (reverse coord))
      (cond ((check-number (string (elt buf count)))
	     (setf index count)
	     (do ((count1 count (+ count1 1))
		  (flag nil))
		 ((or (eq flag t) (< (- (length buf) 1) count1))  coord)
		 (cond ((string= #\Space (elt buf count1))
			(setf coord (cons (parse-integer buf :start index :end count1) coord))
			(setf index '0)
			(setf flag t)
			(setf count count1))
			
		       ((= (- (length buf) 1) count1)
			(setf coord (cons (parse-integer buf :start index) coord))
			(setf index '0)
			(setf count count1))
		       )))
	    )))



(defun issue-order (row col direction)
  "Prints a formatted order for ROW,COL and DIRECTION to standard output. Silently
drops orders when DIRECTION isn't one of :north, :east, :south or :west."
  (when (member direction '(north east south west))
    (format *standard-output* "~&o ~D ~D ~A~%" row col
            (case direction
              ('north "N")
              ('east  "E")
              ('south "S")
              ('west  "W")))))

  
(defun diff (busy all)
  (remove-if #'(lambda (node) (member node
				      (mapcar #'(lambda (node) (first node))
					      busy) :test #'(lambda (p1 p2)
							            (and (= (first p1)
									    (first p2))
									 (= (second p1)
									    (second p2))))))
	     (second all)))


(defun direction (ant neigh)
  (cond ((> (first ant) (first neigh)) 'north)
	((< (first ant) (first neigh)) 'south)
	((> (second ant) (second neigh)) 'west)
	((< (second ant) (second neigh)) 'east)))


(defun find-neighbours (node map)
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


(defun boundingp (coord node)
  (and (> (first node) (- (first coord) 7))
       (< (first node) (+ (first coord) 7))
       (> (second node) (- (second coord) 7))
       (< (second node) (+ (second coord) 7))))


;;Main 
;;====

(defun start-main ()
  ;;variables that exist through out the game
  (let ((*parameters* 'nil)
	(time1 'nil)
	(map 'nil))
       ;;reading the game parameters in 'parameters property
       (setf time1 (get-internal-real-time))
       (read-game-parameters)
       (format *debug-io* "~& parameters ~F" (float (/ (- (get-internal-real-time) time1)
						       internal-time-units-per-second)))
       ;;when turn=0 then create map with initial elements all 0
       (when (= (get *parameters* 'turn) 0)
	 (setf map (make-array `(,(get *parameters* 'rows) ,(get *parameters* 'cols))
			       :initial-element '0)))
       ;;(print map)
       (format t "~&go~%")
       ;;game loop

       (do ((everything-list 'nil)
	    (busy-list 'nil)
	    (temp 'nil)
	    (time2 'nil)
	    (flag 'nil))
	   ((equal flag t) (return))
	   ;;read input

	   (setf time2 (get-internal-real-time))
	   (format *debug-io* "~&about to read")
	   (setf everything-list (read-input))
	   (format *debug-io* "~& read-input ~F" (float (/ (- (get-internal-real-time) time2)
							   internal-time-units-per-second)))
	   ;;process input
	   (dolist (water-block (fourth everything-list) map)
		   (setf (aref map (first water-block) (second water-block)) 'w))
	   (dolist (hill-block (fifth everything-list) map)
	           (setf (aref map (first hill-block) (second hill-block)) 'h))
	   (dolist (ant-block (second everything-list) map)
	           (setf (aref map (first ant-block) (second ant-block)) 'a))
	   (dolist (food-block (first everything-list) nil)
	   
	           (when (not (find-if #'(lambda (node) (equal food-block (second node)))
				       busy-list))
		
		         (when (setf temp
				     (find-if #'(lambda (node) (boundingp food-block node))
					      (diff busy-list everything-list)))
		
			       ;;construct the busy list with a-star
			       (setf busy-list
				     (append busy-list
					     (list (list temp food-block (a-star temp
										 food-block
										 map
										 time2)))))
			       )))
	   ;;issue orders
	   ;;(print map)
	   (format *debug-io* "~& all-blocks ~F" (float (/ (- (get-internal-real-time) time2)
							   internal-time-units-per-second)))
	   (dolist (free-ant (diff busy-list everything-list) nil)
	           (setf (aref map (first free-ant) (second free-ant)) 0)
	           (setf temp (nth (random (length (find-neighbours free-ant map)))
						(find-neighbours free-ant map)))
		   (issue-order (first free-ant)
				      (second free-ant)
				      (direction free-ant temp))
		   (setf (aref map (first temp) (second temp)) 'a))
	   ;;(print map)
	   ;;deal with the ants in busy list
	   ;;(print busy-list)
	   (format *debug-io* "~& free-ants ~F" (float (/ (- (get-internal-real-time) time2)
							  internal-time-units-per-second)))
	   (setf busy-list
		 (remove-if #'(lambda (node) (null (third node)))
			    busy-list))
	   (setf busy-list
		 (mapcar #'(lambda (node) (issue-order (first (first node))
						       (second (first node))
						       (direction (first node)
								  (first (third node))))
			                  (setf (aref map
						      (first (first node))
						      (second (first node))) '0)
					  (setf (aref map
						      (first (first (third node)))
						      (second (first (third node)))) 'a)
					  (list (first (third node)) (second node) (rest (third node))))
			 busy-list))
	   
	   (format *debug-io* "~& busy-remove ~F" (float (/ (- (get-internal-real-time) time2)
							    internal-time-units-per-second)))
	   ;;(print busy-list)
	   (format t "~&go~%"))
	   
	   ;;(print (fourth everything-list)))
       ))



	 