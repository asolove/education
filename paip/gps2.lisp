
(defvar *ops* nil)

(defstruct op "An operation"
	   (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun gps (state goals &optional (*ops* *ops*))
  "General problems solver, from state, use *ops* to accomplish goals"
  (remove-if-not #'action-p
		 (achieve-all (cons '(start) state) goals nil)))

(defun action-p (x)
  (or (equal x '(start)) (executing-p x)))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal and ensure it is still present afterwards."
  (let ((current-state state))
    (when (and (every #'(lambda (g)
			  (setf current-state
				(achieve current-state g goal-stack)))
		      goals)
	       (subsetp goals current-state :test #'equal))
      current-state)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds, or if there is an appropriate op for it that is applicable"
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
	((member-equal goal goal-stack) nil)
	(t (some #'(lambda (op) (apply-op state goal op goal-stack))
		 (find-all goal *ops* :test #'appropriate-p)))))

(defun apply-op (state goal op goal-stack)
  (dbg-indent :gps (length goal-stack) "Consider ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
			     (cons goal goal-stack))))
    (when state2
      (dbg-indent :gps (length goal-stack) "Action ~a" (op-action op))
      (append (remove-if #'(lambda (s)
			     (member-equal s (op-del-list op)))
			 state2)
	      (op-add-list op)))))

(defun executing-p (x)
  (starts-with x 'executing))

(defun appropriate-p (goal op)
  (member-equal goal (op-add-list op)))


(defun convert-op (op)
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op))
    op))

(defun op (action &key preconds add-list del-list)
  (convert-op
   (make-op :action action :preconds preconds :add-list add-list :del-list del-list)))

;;; Sample data
(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school :preconds'(son-at-home car-works)
	    :add-list '(son-at-school) :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
	    :preconds '(car-needs-battery shop-knows-problem shop-has-money)
	    :add-list '(car-works))
   (make-op :action 'tell-shop-problem
	    :preconds '(in-communication-with-shop) :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop :preconds '(know-phone-number)
	    :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
	    :preconds '(have-phone-book) :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
	    :preconds '(have-money) :add-list '(shop-has-money)
	    :del-list '(have-money))))

(mapc #'convert-op *school-ops*)

(defparameter *banana-ops*
  (list
   (op 'climb-on-chair
       :preconds '(chair-at-middle-room at-middle-room on-floor)
       :add-list '(at-bananas on-chair)
       :del-list '(at-middle-room on-floor))
   (op 'push-chair-from-door-to-middle-room
       :preconds '(chair-at-door at-door)
       :add-list '(chair-at-middle-room at-middle-room)
       :del-list '(chair-at-door at-door))
   (op 'walk-from-door-to-middle-room
       :preconds '(at-door on-floor)
       :add-list '(at-middle-room)
       :del-list '(at-door))
   (op 'grasp-bananas
       :preconds '(at-bananas empty-handed)
       :add-list '(has-bananas)
       :del-list '(empty-handed))
   (op 'drop-ball
       :preconds '(has-ball)
       :add-list '(empty-handed)
       :del-list '(has-ball))
   (op 'eat-bananas
       :preconds '(has-bananas)
       :add-list '(empty-handed not-hungry)
       :del-list '(has-bananas hungry))))

(defun make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (make-maze-op (first pair) (second pair))
	(make-maze-op (second pair) (first pair))))

(defun make-maze-op (from to)
  (op `(move from ,from to ,to)
      :preconds `((at ,from))
      :add-list `((at ,to))
      :del-list `((at ,from))))

(defparameter *maze-ops*
  (mapcan #'make-maze-ops
	  '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
	    (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
	    (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))


(defun make-block-ops (blocks)
  )

(defun move-op (a b c)
  "Make a move operation to move a from b to c"
  (op `(move ,a from ,b to ,c)
      :preconds `((,a on ,b) (space on ,a) (space on ,c))
      :del-list `((,a on ,b) (space on ,c))
      :add-list `((,a on ,c))))

(defun move-ons (a b c)
  (if (eq b 'table)))
      

;;; Helpers 
(defun find-all (item sequence &rest keyword-args
		 &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence :test (complement test) keyword-args)))

(defun starts-with (list a)
  (and (consp list) (eql (first list) a)))

(defun member-equal (item list)
  (member item list :test #'equal))

;;; Debugging
(defvar *dbg-ids* nil)

(defun dbg (id format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  (setf *dbg-ids* (set-difference *dbg-ids* ids)))

(defun dbg-indent (id indent format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))
