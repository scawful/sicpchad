; cons car cdr list quote

(define vhello "Hello world")

(define fhello (lambda ()
                "Hello world"))

(define hello
    (lambda (name)
        (string-append "Hello " name "!")))

(hello)

(define (increment x)
    (+ x 1))

(define (decrement x)
    (- x 1))

(define (double x)
    (* x 2))

(define pi (* 4 (atan 1.0)))

(define (degreeToRadian degree)
    (/ (* degree pi) 180))

(define (findDistance velocityX time)
    (* velocityX time))

(define (freefallTime velocityY)
    (/ (* 2.0 velocityY) 9.8))

(define (flyingDistance velocity angle)
    (findDistance
        (* velocity (cos (degreeToRadian angle)))
        (freefallTime (* velocity (sin (degreeToRadian angle))))
    ))
    
;(flyingDistance 40 30) 

(define (new-abs realNum)
    (if (> realNum 0)
        realNum
        (- realNum (double realNum))
    ))

;(new-abs -10)

(define (reciprocal realNum)
    (if (> realNum 0)
        (/ 1 realNum)
        (* (/ realNum 1) realNum)
    ))

(define (inverse realNum)
    (if (not (zero? realNum))
        (/ realNum)))

(inverse 4)
(inverse 1/4)

