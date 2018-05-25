;;; Ch 6

;;; Generalized pattern matcher

(defconstant fail nil)

(defconstant no-bindings '((t . t)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-matcher pattern input bindings))
	((single-pattern-p pattern)
	 (single-matcher pattern input bindings))
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern) (rest input)
		    (pat-match (first pattern) (first input) bindings)))
	(t fail)))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single binding"
  (car binding))

(defun binding-val (binding)
  (cdr binding))

(defun make-binding (var val)
  (cons var val))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (make-binding var val)
	(if (eq bindings no-bindings)
	    nil
	    bindings)))

(defun match-variable (var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding)
	   (extend-bindings var input bindings))
	  ((equal input (binding-val binding))
	   bindings)
	  (t fail))))

;; Default single and segment matchers
(setf (get '?is 'single-match) 'match-is)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)
(setf (get '?or 'segment-match) 'match-or)

;; Generic matcher glue

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat) ?"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun segment-matcher (pattern input bindings)
  "Defer to the match function for this segment pattern"
  (funcall (segment-match-fn (first (first pattern)))
	   pattern input bindings))

(defun segment-match-fn (name)
  (when (symbolp name)
    (get name 'segment-match)))

(defun single-pattern-p (pattern)
  "Is this a single matching pattern like (?is ?n numberp)"
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun single-matcher (pattern input bindings)
  "Defer to the match function for this single pattern"
  (funcall (single-match-fn (first pattern))
	   (rest pattern) input bindings))

(defun single-match-fn (name)
  (when (symbolp name)
    (get name 'single-match)))

;; Matcher functions
(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred, where var-and-pred is the list (var pred)."
  (let ((var (first var-and-pred))
	(pred (second var-and-pred)))
    (if (funcall pred input)
	(pat-match var input bindings)
	fail)))

(defun match-and (patterns input bindings)
  "Succeed if all patterns match input: (?and ?x (?is ?x numberp))"
  (cond ((eq bindings fail) fail)
	((null patterns) bindings)
	(t (match-and (rest patterns)
		      input
		      (pat-match (first patterns) input bindings)))))




(defun match-not (patterns input bindings)
  (if (eq (match-or patterns input bindings) fail)
      fail
      bindings))



;;; Segment matcher implementations

(defun segment-match+ (pattern input bindings)
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  (let ((var (second (first pattern)))
	(rest-pattern (cdr pattern)))
    (or (pat-match (cons var pattern) input bindings)
	(pat-match rest-pattern input bindings))))

(defun segment-match (pattern input bindings &optional (start 0))
  (let ((var (second (first pattern)))
	(rest-pattern (cdr pattern)))
    (if (null rest-pattern)
	(match-variable var input bindings)
	(let ((pos (first-match-pos rest-pattern input start)))
	  (if (not pos)
	      fail
	      (let ((b2 (pat-match
			 rest-pattern (subseq input pos)
			 (match-variable var (subseq input 0 pos)
					 bindings))))
		(if (eq b2 fail)
		    (segment-match pattern input bindings (+ pos 1))
		    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position after start where pat1 could possivly match input. If pat1 is non-constant, just return start. Returns nil if no further possibilities."
  (cond ((and (atom pat1) (not (variable-p pat1)))
	 (position pat1 input :start start :test #'equal))
	((< start (length input)) start)
	(t nil)))

(defun match-or (pattern input bindings)
  "Succed if any pattern matches input: (?or ?x ?y)"
  (let ((disjuncts (cdr (first pattern))))
    (if (null disjuncts) fail
      (or (let ((new-bindings (pat-match (first disjuncts) (first input) bindings)))
            (and (not (eq new-bindings fail))
                 (pat-match (rest pattern) (rest input) new-bindings)))
          (match-or `((?or ,@(rest disjuncts)) ,@(rest pattern))
                    input bindings)))))

(defun match-if (pattern input bindings)
  "Fail unless provided expression evaluates to true in presence of bindings"
  (let ((expr (second (first pattern))))
    ;;; need to use progv rather than substitution to maintain value of lists, rather than intepreting lists as 
    (and (progv
             (mapcar #'car bindings) 
             (mapcar #'cdr bindings)
           (eval expr))
         (pat-match (rest pattern) input bindings))))

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for expansion in pat-match"
  (setf (get symbol 'expand-pat-match-abbrev)
	(expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat"
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
	((atom pat) pat)
	(t (cons (expand-pat-match-abbrev (first pat))
		 (expand-pat-match-abbrev (rest pat))))))

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))
(pat-match-abbrev '?x+ '(?+ ?x))

;;; Ex 6.2:
;;; There is definitely a bug with ?or where it always picks the first possible success,
;;; even if that later causes a failure. ?or should probably become a segment matcher so
;;; it can retry later disjuncts if the first possible match leads to a failure later.
;;; e.g. (pat-match '((?or ?x ?y) ?x ?y) '(1 2 1)) currently fails, but should succeed
;;; by back-tracking and using the ?y disjunct.

;; 6.2: A Rule-Based Translator

(defun rule-based-trans1ator
    (input rules &key (matcher #'pat-match)
		   (rule-if #'first) (rule-then #'rest) (action #'sub1is))
  "Find the first rule in rules that matches input, and apply action to that rule"
  (some
   #'(lambda (rule)
       (let ((result (funcall matcher (funcall rule-if rule) input)))
	 (if (not (eq result fail))
	     (funcall action result (funcall rule-then rule)))))
   rules))

;; 6.4 searching tools
(defun tree-search (states goal-p successors combiner)
  "Find a state that matches goal-p starting with states and their successors"
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
	((funcall goal-p (car states)) (car states))
	(t (tree-search (funcall combiner
				 (funcall successors (car states))
				 (cdr states))
			goal-p successors combiner))))

(defun depth-first-search (start goal-p successors)
  (tree-search (list start) goal-p successors #'append))

(defun breadth-first-search (start goal-p successors)
  (tree-search (list start) goal-p successors
	       #'(lambda (successors states)
		   (append states successors))))

(defun binary-tree (state)
  (list (* state 2)
	(+ (* state 2) 1)))

(defun is (x)
  #'(lambda (y)
      (eql x y)))

(defun finite-binary-tree (max)
  #'(lambda (state)
      (remove-if #'(lambda (state) (> state max))
		 (list (* 2 state)
		       (+ 1 (* 2 state))))))

(defun diff (x)
  #'(lambda (y)
      (abs (- x y))))

(defun sorter (cost-fn)
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors cost-fn)
  (tree-search (list start) goal-p successors (sorter cost-fn)))

(defun price-is-right (price)
  #'(lambda (guess)
      (if (> guess price)
          most-positive-fixnum
          (- price guess))))

(defun beam-search (start goal-p successors cost-fn beam-width)
  (tree-search (list start) goal-p successors
	       #'(lambda (old new)
		   (let ((sorted (funcall (sorter cost-fn) old new)))
		     (if (> beam-width (length sorted))
			 sorted
			 (subseq sorted 0 beam-width))))))



(defstruct (city (:type list)) name long lat)

(defparameter *cities*
  '((Atlanta      84.23 33.45) (Los-Angeles   118.15 34.03)
    (Boston       71.05 42.21) (Memphis        90.03 35.09)  
    (Chicago      87.37 41.50) (New-York       73.58 40.47) 
    (Denver      105.00 39.45) (Oklahoma-City  97.28 35.26)
    (Eugene      123.05 44.03) (Pittsburgh     79.57 40.27) 
    (Flagstaff   111.41 35.13) (Quebec         71.11 46.49)
    (Grand-Jct   108.37 39.05) (Reno          119.49 39.30)
    (Houston     105.00 34.00) (San-Francisco 122.26 37.47)
    (Indianapolis 86.10 39.46) (Tampa          82.27 27.57)
    (Jacksonville 81.40 30.22) (Victoria      123.21 48.25)
    (Kansas-City  94.35 39.06) (Wilmington     77.57 34.14)))

(defun neighbors (city)
  "Find all cities within 1000 kilometers."
  (find-all-if #'(lambda (c)
                   (and (not (eq c city))
                        (< (air-distance c city) 1000.0)))
               *cities*))

(defun city (name) 
  "Find the city with this name."
  (assoc name *cities*))


(defconstant earth-diameter 12765.0
  "Diameter of planet earth in kilometers.")

(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;; d is the straight-line chord between the two cities,
    ;; The length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere.
  The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
    (list (* (cos psi) (cos phi))
          (* (cos psi) (sin phi))
          (sin psi))))

(defun distance (point1 point2)
  "The Euclidean distance between two points.
  The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))

(defun deg->radians (deg)
  "Convert degrees and minutes to radians."
  (* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180))
