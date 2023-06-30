(define (square x) (* x x))
 
(define (even? n)
  (= (remainder n 2) 0))
 
; exercise 1.16
(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))

; differentiation
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (binary-expression? e)
    (null? (cdddr e)))

(define (reduce-expression e op)
    (if (binary-expression? e)
        (second-term e)
        (cons op (all-but-first-term e))))

(define (second-term e)
    (caddr e))

(define (all-but-first-term e)
    (cddr e))

(define (addend s) (cadr s))

(define (augend s) (reduce-expression s '+))

(define (minuend s) (cadr s))

(define (subtrahend s) (reduce-expression s '-))

(define (base b) (cadr b))

(define (exponent b) (reduce-expression b '**))

(define (multiplier p) (cadr p))

(define (multiplicand p) (reduce-expression p '*))

(define (numerator r) (cadr r))

(define (denominator r) (reduce-expression r '/))

(define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

(define (difference? x)
    (and (pair? x) (eq? (car x) '-)))

(define (product? x)
    (and (pair? x) (eq? (car x) '*)))

(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

(define (rational? x)
    (and (pair? x) (eq? (car x) '/)))

(define (natural-logarithm? x)
    (and (pair? x) (eq? (car x) 'ln)))

(define (logarithm? x)
    (and (pair? x) (eq? (car x) 'log)))

(define (=number? exp num)
    (and (number? exp) (= exp num)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

(define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((equal? base 1) 1)
          ((and (number? base) (number? exponent)) (fast-expt base exponent)) 
          (else (list '** base exponent))))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

(define (make-difference d1 d2)
    (cond ((=number? d1 0) d2)
          ((=number? d2 0) d1)
          ((and (number? d1) (number? d2)) (- d1 d2))
          (else (list '- d1 d2))))

(define (make-rational num den)
    (cond ((=number? num 0) 0)
          ((=number? den 0) false)
          ((and (number? num) (number? den)) (/ num den))
          (else (list '/ num den))))

(define (make-natural-logarithm exp)
    (cond ((number? (cdr exp)) (log exp))
          (else (list '/ 1 (cdr exp)))))

(define (deriv exp var)
    (cond ((number? exp) 0)
            ((variable? exp)
                (if (same-variable? exp var) 1 0))
            ((sum? exp)
                (make-sum (deriv (addend exp) var)
                            (deriv (augend exp) var)))
            ((difference? exp)
                (make-difference (deriv (minuend exp) var)
                                    (deriv (subtrahend exp) var)))
            ((product? exp)
                (make-sum
                    (make-product (multiplier exp)
                                (deriv (multiplicand exp) var))
                    (make-product (deriv (multiplier exp) var)
                                (multiplicand exp))))
            ((rational? exp)
                (make-rational 
                    (make-difference 
                        (make-product (denominator exp) (deriv (numerator exp) var))
                        (make-product (numerator exp) (deriv (denominator exp) var)))
                    (make-product (denominator exp) (denominator exp))))
            ((exponentiation? exp)
              (let ((u (base exp))
                    (n (exponent exp)))
                (make-product
                (make-product n
                        (make-exponentiation u
                                            (make-sum n -1)))
            (deriv u var))))
            ((natural-logarithm? exp)
                (make-natural-logarithm exp))
            (else
                (error "unknown expression type -- DERIV" exp))))

; test zone 

(deriv '(* (+ 3 y) (+ y (** x 4))) 'x)
(deriv '(+ (* y (+ x 3)) (** x 3)) 'x)
(deriv '(/ (** x 4) (+ x 5)) 'x)
(deriv '(- (** x 2) (* 3 x) 4) 'x)
(deriv '(ln x) 'x)
(deriv '(ln 3) 'x)