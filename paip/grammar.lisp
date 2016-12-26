
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase ->(Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")


(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> ( Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))

(defvar *grammar* *bigger-grammar*)

;;; Accessors

(defun rule-lhs (rule)
  (first rule))

(defun rule-rhs (rule)
  (rest (rest rule)))

(defun rewrites (category)
  (rule-rhs (assoc category *grammar*)))

;;; (generate 'noun) -> pick one of the rewrites, generate all of it
;;; (generate 'a) -> '(a)
;;; (generate '
(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
	 (mapcan #'generate phrase))
	((rewrites phrase)
	 (generate (random-elt (rewrites phrase))))
	('else (list phrase))))

(defun generate-tree (phrase)
  "Generate a tagged tree of a random sentence or phrase"
  (cond ((listp phrase)
	 (mapcar #'generate-tree phrase))
	((rewrites phrase)
	 (cons phrase (generate-tree (random-elt (rewrites phrase)))))
	('else (list phrase))))

(defun random-elt (set)
  (elt set (random (length set))))


;;; 2.3: cross-product
(defun cross-product (f xs ys)
  (mapcan #'(lambda (x) (mapcar #'(lambda (y) (funcall f x y)) ys)) xs))
