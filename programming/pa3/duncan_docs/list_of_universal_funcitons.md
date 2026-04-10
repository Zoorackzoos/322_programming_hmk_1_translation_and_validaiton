# list of universal functions

## element getters
```racket
(define (my-first lst)
  (car lst))

(define (my-second lst)
  (car (cdr lst)))

(define (my-third lst)
  (car (cdr (cdr lst))))

(define (my-fourth lst)
  (car (cdr (cdr (cdr lst)))))

(define (my-fifth lst)
  (car (cdr (cdr (cdr (cdr lst))))))
```

## literal detectors
```racket
;; goofy ahh funcitons man. 
;; if this is a true or a true or a false, then true.
;; otherwise false. 
(define (boolean-literal? x)
  (or
   (equal? x 'true)
   (equal? x 'false)
  )
)

;; returns true if the input is a number or boolean 
(define (literal? x)
  (or (number? x)
      (boolean-literal? x)))
```

## operation detectors
```racket
(define (arithmetic-op? x)
  (or (equal? x '+)
      (equal? x '-)
      (equal? x '*)
      (equal? x '/)))

(define (boolean-op? x)
  (or (equal? x '&&)
      (equal? x '||)))

(define (comparison-op? x)
  (or (equal? x '==)
      (equal? x '!=)
      (equal? x '<)
      (equal? x '<=)
      (equal? x '>)
      (equal? x '>=)))

;; returns true if: - , !. returns false if antyhging else
(define (unary-op? x)
  (or (equal? x '-)
      (equal? x '!)
      (equal? x 'not)))

;; true if either of these:
;;   +, -, *, /
;;   &&, ||
;;   ==, !=, <, <=, >, >= 
(define (binary-op? x)
  (or (arithmetic-op? x)
      (boolean-op? x)
      (comparison-op? x)))
```

## shape detectors
```racket
;; this is a word nobody uses.
;;     https://docs.google.com/document/d/1HOnXb8v4rdqWKvQWRcCS--AHOB0hQhTt20cyo58_x1s/edit?usp=sharing
;; if e is a list, and has length 2, and the 1st element is either ! or -.
;;    like: - x, ! x
;;    not: + + x, - - x, ~ x, -x, !x, !!x, --x
(define (unary-shape? e)
  (and (list? e)
       (= (length e) 2)
       (unary-op? (my-first e))))

;; if the list is a list, and is the length 3
;; why is this called a shape?
(define (three-part-shape? e)
  (and (list? e)
       (= (length e) 3)))

;; has to be a list of length 3.
;; the 2nd piece of the list has to be a "binary operation":
;;   +, -, *, /
;;   &&, ||
;;   ==, !=, <, <=, >, >= 
(define (binary-shape? e)
  (and (three-part-shape? e)
       (binary-op? (my-second e))))
```

## error comparers
```racket
(define (type-error? x)
  (equal? x '(err "type error")))

(define (div-zero-error? x)
  (equal? x '(err "division by zero")))

(define (any-error? x)
  (and (list? x)
       (not (null? x))
       (equal? (car x) 'err)))
```



## maybe universals 
like i'm not sure if i'll use them
```racket
(define (normalize-op op)
  (cond
    [(equal? op '&&) 'and]
    [(equal? op '||) 'or]
    [(equal? op '!)  'not]
    [else op]))
```

