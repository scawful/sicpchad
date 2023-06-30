;; Exercise 3.1 Page 224
;; Accumulator Procedure 
(define (make-accumulator value)
    (lambda (addend)
        (+ addend value)))

; (define A (make-accumulator 10))
; (print (A 5))  ==> 15
; (print (A 10)) ==> 25

;; Exercise 3.2 Page 224
;; Function call monitoring with internal call counter
;; Two implementations: anonymous function and defined procedure
(define (make-monitored function)
    (let ((counter 0))
        (lambda (input)
            (cond ((eq? input 'how-many-calls?) (print counter))
                  ((eq? input 'reset-count) (set! counter 0))
                  (else (begin (set! counter (+ counter 1))
                     (print (function input))))))))

(define (+make-monitored+ function)
    (define counter 0)
    (define (mf message)
        (cond ((eq? message 'how-many-calls?) (print counter))
              ((eq? message 'reset-count) (set! counter 0))
              (else (begin (set! counter (+ counter 1))
                  (print (function message))))))
    mf)

; (define s (+make-monitored+ sqrt)) ==> #s of sqrt
; (s 64) ==> 8
; (s 'how-many-calls?) ==> 1
; (s 'reset-count) 
; (s 'how-many-calls?) ==> 0

;; Exercise 3.3 & 3.4 Page 225
;; Password protected bank account procedure
;; Call the cops if password is incorrect too many times
(define (make-account balance password)
    (define call-the-cops "Exceeded incorrect password attempts.")
    (define incorrect-counter 0)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch p m)
        (if (>= incorrect-counter 7)
            (lambda (amount) call-the-cops)
                (if (not (eq? p password)) 
                    (lambda (amount) 
                        (set! incorrect-counter (+ incorrect-counter 1)) 
                        "Incorrect Password")
                    (cond ((eq? m 'withdraw) withdraw)
                          ((eq? m 'deposit) deposit)
                          (else (error "Unknown request -- MAKE-ACCOUNT" m))))))
    dispatch)

; (define acc (make-account 100 'express1))
; (print ((acc 'express1 'withdraw) 50))
; (print ((acc 'express1 'withdraw) 60))
; (print ((acc 'express1 'deposit) 40))
; (print ((acc 'dogcatfood 'withdraw) 60))

;; Exercise 3.5 Page 228
;; Monte Carlo Integration 
;; MIT-Scheme Interpreter required for random function
(define (square x)
    (* x x))

(define (estimate-integral P x1 x2 y1 y2 trials)
    (* (* (- x2 x1)
          (- y2 y1))
        (monte-carlo trials P)))

(define (estimate-pi)
    (estimate-integral in-circle? -1.0 1.0 -1.0 1.0 10000))

(define (in-circle?)
    (>= 1 (+ (square (random-in-range -1.0 1.0))
             (square (random-in-range -1.0 1.0)))))

;; book provided procedures
(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((experiment)
                (iter (- trials-remaining 1) (+ trials-passed 1)))
              (else 
                (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

; (estimate-pi)

;; Exercise 3.6 Page 229
;; Rand procedure with generate and reset value
;; (rand 'generate) produces new random number
;; ((rand 'reset) <new-value> ) resets internal state variable to <new-value>

(define random-init 0)
(define (rand-update val) (+ (random 10) val))

(define (rand)
    (let ((x random-init))
    (define (dispatch message)
        (cond ((eq? message 'generate)
                (set! x (rand-update x)))
              ((eq? message 'reset)
                (lambda (value)
                    (set! x value)))))
    dispatch))

; (define rand-generator (rand))
; (rand-generator 'generate)
; (rand-generator 'generate)
; ((rand-generator 'reset) 12)
; (rand-generator 'generate)
; (rand-generator 'generate)

;; Exericse 3.7 Page 236
;; Make joint bank account with new password
;; Creates an alias to a provided bank account

(define (+make-account+ balance password)
    (define call-the-cops "Exceeded incorrect password attempts.")
    (define incorrect-counter 0)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (define (dispatch p m)
        (if (>= incorrect-counter 7)
            (lambda (amount) call-the-cops)
                (if (not (eq? p password)) 
                    (lambda (amount) 
                        (set! incorrect-counter (+ incorrect-counter 1)) 
                        "Incorrect Password")
                    (cond ((eq? m 'withdraw) withdraw)
                          ((eq? m 'deposit) deposit)
                          (else (error "Unknown request -- MAKE-ACCOUNT" m))))))
    dispatch)

(define (make-joint account password new-password)
    (define call-the-cops "Exceeded incorrect password attempts.")
    (define incorrect-counter 0)
    (define (withdraw amount) ((account password 'withdraw) amount))
    (define (deposit amount) ((acount password 'deposit) amount))

    (define (dispatch p m)
        (if (>= incorrect-counter 7)
            (if (not (eq? p new-password))
                (lambda (amount)
                    (set! incorrect-counter (+ incorrect-counter 1))
                    "Incorrect Password")
                (cond ((eq? m 'withdraw) withdraw)
                      ((eq? m 'deposit) deposit)
                      (else (error "Unknown request -- MAKE-JOINT" m))))))
    dispatch)

(define peter-acc (+make-account+ 100 'express1))
(define paul-acc (make-joint peter-acc 'express1 'doomshit))

(print ((peter-acc 'express1 'withdraw) 25))
(print ((paul-acc 'doomshit 'withdraw) 50))

;; Exercise 3.8 Page 236
