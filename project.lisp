(defun extnd (path)
       (print (reverse path))
       (mapcar #'(lambda (new-node) (cons new-node path))
	       (remove-if #'(lambda (neighbor) (member neighbor path))
	       (get (first path) 'neighbors))))


;;Reading the beginning game parameters such as turntime, loadtime, rows, cols etc.
;;=================================================================================

(defun read-game-parameters ()
  (do ((buf 'nil)
       (parameters 'nil))
      ((string-equal buf "ready") 'end)
      (setf buf (read-line))
      (cond ((search "turntime" buf)
	     (setf (get 'parameters 'turntime) (parse-integer buf :start 9)))
	    ((search "turns" buf)
	     (setf (get 'parameters 'turns) (parse-integer buf :start 6)))
	    ((search "turn" buf)
	     (setf (get 'parameters 'turn) (parse-integer buf :start 5)))
	    ((search "loadtime" buf)
	     (setf (get 'parameters 'loadtime) (parse-integer buf :start 9)))
	    ((search "rows" buf)
	     (setf (get 'parameters 'rows) (parse-integer buf :start 5)))
	    ((search "cols" buf)
	     (setf (get 'parameters 'cols) (parse-integer buf :start 5)))
	    ((search "viewradius" buf)
	     (setf (get 'parameters 'viewradius) (parse-integer buf :start 11)))
	    ((search "attackradius" buf)
	     (setf (get 'parameters 'attackradius) (parse-integer buf :start 11)))
	    ((search "spawnradius" buf)
	     (setf (get 'parameters 'spawnradius) (parse-integer buf :start 12)))
	    ((search "player_seed" buf)
	     (setf (get 'parameters 'player-seed) (parse-integer buf :start 12))))
      ))


;;Reading the game input on each turn
;;===================================

(defun read-input ()
  (do ((buf 'nil)
       (food-coord-list 'nil)
       (ant-coord-list-own 'nil)
       (ant-coord-list-ene 'nil)
       (water-coord-list 'nil)
       (ant-hill-list 'nil))
      ((string-equal buf "go") 'end)
      (setf buf (read-line))
      (cond ((search "f" buf)
	     (setf food-coord-list (append food-coord-list (list (string-to-coord buf)))))
	    ((search "a" buf)
	     (if (= 0 (first (reverse (string-to-coord buf))))
		 (setf ant-coord-list-own (append ant-coord-list-own
						  (list (string-to-coord buf))))
	         (setf ant-coord-list-ene (append ant-coord-list-ene
						  (list (string-to-coord buf))))))
	    ((search "w" buf) (setf water-coord-list (append water-coord-list
							     (list (string-to-coord buf)))))
	    ((search "h" buf) (setf ant-hill-list (append ant-hill-list
							  (list (string-to-coord buf))))))
      (print food-coord-list)
      (print ant-coord-list-own)
      (print ant-coord-list-ene)
      (print water-coord-list)
      (print ant-hill-list)))


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

;;Check if the given string is an numeric digit (MACRO)
;;=====================================================

(defmacro check-number (string)
  `(search ,string "0123456789"))

