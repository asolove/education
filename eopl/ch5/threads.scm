#lang eopl

(require (only-in racket/base list-ref))
(require (only-in racket/list index-of))

(require "./threads-syntax.scm")

;; Threads language

(define identifier? symbol?)

(define debug? #f)

(define (debug form)
  (when debug?
    (begin
      (newline)
      (display form)
      (newline))))

; Environment data type with support for letrec
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (ref reference?)
   (env environment?))
  (extend-env*
   (vars (list-of symbol?))
   (refs (list-of reference?))
   (env environment?))
  (extend-env-rec
   (p-names (list-of symbol?))
   (b-vars-es (list-of (list-of symbol?)))
   (p-bodies (list-of expression?))
   (env environment?)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
                 (eopl:error `(Could not find ,search-var in environment)))
      (extend-env (var ref rest-env)
                  (if (eqv? var search-var)
                      ref
                      (apply-env rest-env search-var)))
      (extend-env* (vars refs rest-env)
                   (let ((i (index-of vars search-var)))
                     (if i
                         (list-ref refs i)
                         (apply-env rest-env search-var))))
      (extend-env-rec (p-names b-vars-es p-bodies saved-env)
                      (let ((p-index (index-of p-names search-var)))
                        (if p-index
                            (newref
                             (proc-val (procedure (list-ref b-vars-es p-index)
                                                  (list-ref p-bodies p-index)
                                                  env)))
                            (apply-env saved-env search-var)))))))
           

; ExpVal data type
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))

(define (expval->num val)
  (cases expval val
    (num-val (n) n)
    (else (eopl:error `(trying to get number from incompatible value ,val)))))

(define (expval->bool val)
  (cases expval val
    (bool-val (b) b)
    (else (eopl:error `(trying to get boolean from incompatible value, val)))))

(define (expval->proc val)
  (cases expval val
    (proc-val (p) p)
    (else (eopl:error `(trying to get proc from incompatible value ,val)))))

; Proc data type
(define (proc? value)
  (procedure? value))

(define (procedure vars body env)
  (lambda (vals k)
    (value-of/k body (extend-env* vars (map newref vals) env) k)))

(define (apply-procedure/k proc val k)
  (proc val k))

; Store data type
(define (empty-store) '())

(define (get-store) the-store)

(define the-store '())

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v) (integer? v))

(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    next-ref))

(define (deref ref)
  (list-ref the-store ref))

(define (setref! ref val)
  (set! the-store
        (letrec ((setref-inner
                  (lambda (store1 ref1)
                    (cond
                      ((null? store1)
                       (eopl:error `(Invalid reference ,ref1 in ,the-store)))
                      ((zero? ref1)
                       (cons val (cdr store1)))
                      (else
                       (cond (car store1)
                             (setref-inner  (cdr store1) (- ref1 1))))))))
          (setref-inner the-store ref))))

; Queue data type
(define-datatype queue queue?
  (a-queue
   (tail list?)
   (head list?)))

(define (empty-queue)
  (a-queue '() '()))

(define (queue-empty? q)
  (cases queue q
    (a-queue (tail head)
             (and (null? tail) (null? head)))))

(define (enqueue q item)
  (cases queue q
    (a-queue (tail head)
             (a-queue (cons item tail) head))))

(define (dequeue q fn)
  (cases queue q
    (a-queue (tail head)
             (if (null? head)
                 (let ((new-head (reverse tail)))
                   (fn (car new-head) (a-queue '() (cdr new-head))))
                 (fn (car head) (a-queue tail (cdr head)))))))

; Scheduler data type
(define the-ready-queue 'uninitialized)
(define the-final-answer 'uninitialized)
(define the-max-time-slice 'uninitialized)
(define the-time-remaining 'uninitialized)

(define (initialize-scheduler! time)
  (set! the-ready-queue (empty-queue))
  (set! the-final-answer '())
  (set! the-max-time-slice time)
  (set! the-time-remaining time))

(define (place-on-ready-queue! thread)
  (set! the-ready-queue (enqueue the-ready-queue thread)))

(define (run-next-thread)
  (if (queue-empty? the-ready-queue)
      the-final-answer
      (dequeue the-ready-queue
               (lambda (next-thread queue)
                 (set! the-ready-queue queue)
                 (set! the-time-remaining the-max-time-slice)
                 (next-thread)))))

(define (set-final-answer! val)
  (set! the-final-answer val))

(define (time-expired?)
  (<= the-time-remaining 0))

(define (decrement-timer!)
  (set! the-time-remaining (- the-time-remaining 1)))



; Environment data type
(define (init-env)
  (empty-env))

; Continuation data type
(define-datatype cont cont?
  (end-cont)
  (diff-cont1
   (e2 expression?)
   (env environment?)
   (k cont?))
  (diff-cont2
   (v1 expval?)
   (k cont?))
  (zero?-cont
   (k cont?))
  (if-cont
   (if-true expression?)
   (if-false expression?)
   (env environment?)
   (k cont?))
  (let-cont
   (var identifier?)
   (body expression?)
   (env environment?)
   (k cont?))
  (call-cont
   (arg-exp (list-of expression?))
   (env environment?)
   (k cont?))
  (eval-arg-cont
   (proc proc?)
   (env environment?)
   (past-args (list-of expval?))
   (rest-arg-exps (list-of expression?))
   (k cont?))
  (assign-cont
   (var identifier?)
   (env environment?)
   (k cont?))
  (begin-cont
    (rest-exps (list-of expression?))
    (env environment?)
    (k cont?))
  (spawn-cont
   (k cont?))
  (end-subthread-cont)
  (end-main-thread-cont))
    

(define (apply-cont k val)
  (if (time-expired?)
      (begin
        (place-on-ready-queue!
         (lambda ()
           (debug `(Resuming thread with continuation ,k))
           (apply-cont k val)))
        (debug `(Pausing thread with continuation ,k))
        (run-next-thread))
      (apply-cont-impl k val)))

(define (apply-cont-impl k val)
  
  (decrement-timer!)
  (debug `(Applying ,val to continuation ,k))
  (cases cont k
    (end-cont ()
              (debug "End of computation")
              val)
    (diff-cont1 (e2 env k)
                (value-of/k e2 env (diff-cont2 val k)))
    (diff-cont2 (v1 k)
                (apply-cont k (num-val (- (expval->num v1)
                                          (expval->num val)))))
    (zero?-cont (k)
                (apply-cont k (bool-val (= 0 (expval->num val)))))
    (if-cont (if-true if-false env k)
             (value-of/k
              (if (expval->bool val) if-true if-false)
              env
              k))
    (let-cont (var body env k)
              (value-of/k body (extend-env var (newref val) env) k))
    (call-cont (arg-exps env k)
               (if (null? arg-exps)
                   (apply-procedure/k (expval->proc val) '() k)
                   (let ((args (reverse arg-exps)))
                     (value-of/k
                      (car args)
                      env
                      (eval-arg-cont (expval->proc val) env '() (cdr args) k)))))
    (eval-arg-cont (proc env past-args rest-arg-exps k)
                    (let ((args (cons val past-args)))
                      (if (null? rest-arg-exps)
                          (apply-procedure/k proc args k)
                          (value-of/k (car rest-arg-exps) env
                                      (eval-arg-cont
                                       proc args (cdr rest-arg-exps) k)))))

    (begin-cont (rest-exps env k)
                (if (null? rest-exps)
                    (apply-cont k val)
                    (value-of/k (car rest-exps)
                                env
                                (begin-cont (cdr rest-exps) env k))))

    (assign-cont (var env k)
                 (setref! (apply-env env var) val)
                 (apply-cont k (num-val 42)))
    (spawn-cont (k)
                (let ((proc1 (expval->proc val)))
                  (place-on-ready-queue!
                   (lambda ()
                     (debug `(Starting to run new thread ,proc1))
                     (apply-procedure/k proc1
                                        (list (num-val 28))
                                        (end-subthread-cont))))
                  (apply-cont k (num-val 73))))
    (end-subthread-cont ()
                        (debug "Thread finished")
                        (run-next-thread))
    (end-main-thread-cont ()
                          (set-final-answer! val)
                          (run-next-thread))))
    

; Interpreter
(define (run string)
  (value-of-program (scan&parse string) 2))

(define (value-of-program prog timeslice)
  (initialize-store!)
  (initialize-scheduler! timeslice)
  (cases program prog
    (a-program (exp)
               (value-of/k exp (init-env) (end-main-thread-cont)))))

(define (value-of/k exp env k)
  (debug `(Getting value of ,exp))
  (cases expression exp
    (const-exp (n) (apply-cont k (num-val n)))
    (diff-exp (e1 e2)
              (value-of/k e1 env
                          (diff-cont1 e2 env k)))
    (zero?-exp (exp)
               (value-of/k exp env
                           (zero?-cont k)))
    (if-exp (cond if-true if-false)
            (value-of/k cond env
                        (if-cont if-true if-false env k)))
    (var-exp (var)
             (apply-cont k (deref (apply-env env var))))
    (let-exp (var expr body)
             (value-of/k expr env
                         (let-cont var body env k)))
    
    (letrec-exp (p-names b-vars-es p-bodies letrec-body)
                (let ((new-env (extend-env-rec p-names b-vars-es p-bodies env)))
                  (value-of/k letrec-body new-env k)))
    
    (proc-exp (vars body)
              (apply-cont k (proc-val (procedure vars body env))))
                
    (call-exp (fn-exp arg-exps)
              (value-of/k fn-exp env (call-cont arg-exps env k)))

    (begin-exp (exp1 rest-exps)
               (value-of/k exp1 env (begin-cont rest-exps env k)))

    (assign-exp (var exp)
                (value-of/k exp env (assign-cont var env k)))

    (spawn-exp (exp)
               (value-of/k exp env (spawn-cont k)))))
              


(define mutual-letrec-program
  "letrec
      even(x) = if zero?(x) then 1 else (odd -(x,1))
      odd(x) = if zero?(x) then 0 else (even -(x,1))
   in (odd 13)")


(define implicit-refs-program
  "       let x = 0
             in letrec even()
                        = if zero?(x)
                          then 1
                          else begin
                                set x = -(x,1);
                                (odd) end
                       odd()
                        = if zero?(x)
                          then 0
                          else begin
                                set x = -(x,1);
                                (even)
                               end
                 in begin set x = 13; (odd) end")

(define implicit-refs-program2
  "          let g = let count = 0
                     in proc (dummy)
                         begin
                          set count = -(count,-1);
                          count
                         end
             in let a = (g 11)
                in let b = (g 11)
                   in -(a, b)")

(define unsafe-counter-program
  "let x = 0
      in let incr_x = proc ()
                       proc ()
                        set x = -(x, -1)
      in begin
          spawn(proc () begin 2; (incr_x) end);
          spawn((incr_x));
          spawn((incr_x))
      end")