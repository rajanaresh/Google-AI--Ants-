(load "common.lisp")
(load "A-star.lisp")

;;Reading the beginning game parameters such as turntime, loadtime, rows, cols etc.
;;=================================================================================

(defvar *parameters*)

(defun read-game-parameters ()
  "Reads the game parameters such as turntime, loadtime etc. and stores in the special variable
used as a property list *parameters*."
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


;;Check if the given string is an numeric digit (MACRO)
;;=====================================================

(defmacro check-number (string)
  "Used to check if a given string is a single digit number or not. It's a helper procedure for
string-to-coord procedure below."
  `(search ,string "0123456789"))


;;Converting the string input to a list of coordinates
;;====================================================

(defun string-to-coord (buf)
  "Converts a valid game input string to coordinates."
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

;;Reading the game input on each turn
;;===================================

(defun read-input ()
  "The procedure generates one of the important data structures of the game i.e., everything-list.
Given the input game state at each turn it returns the new set of food list, ants, water blocks
and hills."
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
      

(defun boundingp (coord node)
  "Takes two coordinates (coord and node) as input and returns true if node lies in the bounding
box created by adding and subtracting the rows and cols of coord."
  (and (> (first node) (- (first coord) (sqrt (get *parameters* 'viewradius))))
       (< (first node) (+ (first coord) (sqrt (get *parameters* 'viewradius))))
       (> (second node) (- (second coord) (sqrt (get *parameters* 'viewradius))))
       (< (second node) (+ (second coord) (sqrt (get *parameters* 'viewradius))))))

(defun explore (ex-map all-ants everything-list map)
  "Loops through all-ants list and puts 'e for each grid that is in the view radius of the ant.
It also teases and plays defensive."
  (let ((internal-busy-list nil))
    (dolist (ant all-ants nil)
            (let ((istart (if (<= (- (first ant) (sqrt (get *parameters* 'viewradius))) 0)
			      0
			      (floor (- (first ant) (sqrt (get *parameters* 'viewradius))))))
		  (jstart (if (<= (- (second ant) (sqrt (get *parameters* 'viewradius))) 0)
			      0
			      (floor (- (second ant) (sqrt (get *parameters* 'viewradius))))))
		  (iend (if (>= (+ (first ant) (sqrt (get *parameters* 'viewradius)))
			        (get *parameters* 'rows))
			    (- (get *parameters* 'rows) 1)
			    (floor (+ (first ant) (sqrt (get *parameters* 'viewradius))))))
		  (jend (if (>= (+ (second ant) (sqrt (get *parameters* 'viewradius)))
				(get *parameters* 'cols))
			    (- (get *parameters* 'cols) 1)
			    (floor (+ (second ant) (sqrt (get *parameters* 'viewradius))))))
		  (ene-ants 'nil))
			    
	         (do ((i istart (+ i 1)))
		     ((> i iend) nil)
		     (do ((j jstart (+ j 1)))
			 ((> j jend) nil)
			 (when (distancep i j (first ant) (second ant)
					  (sqrt (get *parameters* 'viewradius)))
			       (setf (aref ex-map i j) 'e))
			 (when (member (list i j) (third everything-list)
				       :test #'(lambda (node1 node2) (and (= (first node1)
									     (first node2))
									  (= (second node1)
									     (second node2)))))
			       (setf ene-ants (append ene-ants (list (list i j)))))
			 ))
		 ;;find closest neighbour of ant with closest enemy ant
		 ;;append in internal-busy-list with (ant nil (it's neighbour)) format
		 (when (not (or (null ene-ants)
				(null (find-neighbours ant map))
				(null (closest ant ene-ants))))
		       ;;if distance b/w the neighbour with the closest ant is less than attack
		       ;;radius
		       (if (distancep (first (closest (closest ant ene-ants)
						      (find-neighbours ant map)))
				      (second (closest (closest ant ene-ants)
						       (find-neighbours ant map)))
				      (first (closest ant ene-ants))
				      (second (closest ant ene-ants))
				      (sqrt (get *parameters* 'attackradius)))
			   ;;get farther to the enemy ant
			   (setf internal-busy-list
				 (append internal-busy-list
					 (list (list ant
						     nil
						     (list (farthest (closest ant ene-ants)
								     (find-neighbours ant map)))))))
			 ;;otherwise get closer to the enemy ant
			 (setf internal-busy-list
			       (append internal-busy-list
				       (list (list ant
						   nil
						   (list (closest (closest ant ene-ants)
								  (find-neighbours ant map)))))))))
		 ))
    internal-busy-list
	  ))

;;Main 
;;====

(defun start-main ()
  ;;variables that exist through out the game
  (let ((*parameters* 'nil)
	(time1 'nil)
	(map 'nil)
	(ex-map 'nil))
       ;;reading the game parameters in 'parameters property
       (setf time1 (get-internal-real-time))
       (read-game-parameters)
       ;;(format *debug-io* "~& parameters ~F" (float (/ (- (get-internal-real-time) time1)
       ;;						       internal-time-units-per-second)))
       ;;when turn=0 then create map with initial elements all 0
       (when (= (get *parameters* 'turn) 0)
	 (setf ex-map (make-array `(,(get *parameters* 'rows) ,(get *parameters* 'cols))
			       :initial-element '0))
	 (setf map (make-array `(,(get *parameters* 'rows) ,(get *parameters* 'cols))
			       :initial-element '0)))
       ;;(print map)
       (format t "~&go~%")
       ;;game loop

       (do ((everything-list 'nil)
	    (busy-list 'nil)
	    (trail-list 'nil)
	    (temp 'nil)
	    (time2 'nil)
	    (ene-hills 'nil)
	    (flag 'nil))
	   ((equal flag t) (return))
	   ;;read input
	   ;;set time for A* in food collection
	   (setf time2 (get-internal-real-time))
	   
	   (setf everything-list (read-input))

	   ;;process input
	   (setf busy-list
		 (remove-if #'(lambda (node) (null node))
			    busy-list))
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
	   
	   ;;naive attack strategy
	   
	   (setf busy-list
		 (remove-if #'(lambda (node) (null (third node)))
			    busy-list))
	   ;;dealing with enemy ant hills if detected. Should be top priority
	   (when (not (null (first (reverse everything-list))))
	         (dolist (hill-block (first (reverse everything-list)) 'nil)
		         (when (not (member hill-block ene-hills))
			       (setf ene-hills (cons hill-block ene-hills)))))
	   (setf time2 (get-internal-real-time))
	   (dolist (hills ene-hills 'nil)
	           (when (setf temp
			       (find-if #'(lambda (node) (boundingp hills node))
					(diff busy-list everything-list)))
		         		     
		         ;;leave a trail from all the ant hills
			 
		         (let ((home (closest temp (fifth everything-list))))
			         (setf trail-list
				       (append trail-list
					       (list (a-star home temp map time2)))))
			 
		         ;;construct the busy list with a-star
			 
		         (setf busy-list
			       (append busy-list
				       (list (list temp hills (a-star temp
								      hills
								      map
								      time2)))))
			 ))
	   (setf busy-list
		 (remove-if #'(lambda (node) (null (third node)))
			    busy-list))
	   
	   ;;the trail-list used below
	   (setf trail-list
		 (remove-if #'(lambda (node) (null node)) trail-list))
	   
	   ;;code not in use
	   (mapcar #'(lambda (node-list)
		             (mapcar #'(lambda (node) (when (not (equal (aref map
									      (first node)
									      (second node))
									'a))
							    (setf (aref map
									(first node)
									(second node)) 't)))
				     node-list))
		   trail-list)

	   ;;if an ant bumbs into the trail list we put the trail and ant in the busy-list
	   (when (not (null trail-list))
	     (dolist (free-ant (diff busy-list everything-list) nil)
	             (dolist (neigh (find-neighbours free-ant map) nil)
		             (dolist (trail trail-list nil)
			             (when (member neigh trail)
				           (setf busy-list
						 (append busy-list
							 (list (list free-ant nil (member neigh trail))))))
				     ))))
	   
	   ;;explore strategy goes here
	   (when (not (null (third everything-list)))
	         (setf busy-list
		       (explore ex-map (diff busy-list everything-list) everything-list map)))

	   ;;free ants just more randomly.
	   (dolist (free-ant (diff busy-list everything-list) nil)
	           (when (> (length (find-neighbours free-ant map)) 0)
		         (setf (aref map (first free-ant) (second free-ant)) 0)
			 (setf temp (nth (random (length (find-neighbours free-ant map)))
					 (find-neighbours free-ant map)))
			 (issue-order (first free-ant)
				      (second free-ant)
				      (direction free-ant temp))
			 (setf (aref map (first temp) (second temp)) 'a)))
	   
	   
	   ;;deal with the ants in busy list
	   (setf busy-list
		 (remove-if #'(lambda (node) (null (third node)))
			    busy-list))
	   
	   ;;parsing busy-list and issueing orders in the busy-list
	   (setf busy-list
		 (mapcar #'(lambda (node)
			     (when (not (equal (aref map
						     (first (first (third node)))
						     (second (first (third node)))) 'a))
			           (issue-order (first (first node))
						(second (first node))
						(direction (first node)
							   (first (third node))))
				   (setf (aref map
					       (first (first node))
					       (second (first node))) '0)
				   (setf (aref map
					       (first (first (third node)))
					       (second (first (third node)))) 'a)
				   (list (first (third node)) (second node) (rest (third node)))))
			     busy-list))
	   
	   (format t "~&go~%"))
       ))



	 