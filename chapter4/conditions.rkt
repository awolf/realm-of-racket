#lang racket

(require rackunit)

(struct student (name id# dorm) #:transparent)

(define sophomore3 (student 'David 100234 'PG))
(student-name sophomore3)

(student? 'a) ;#f
(student? sophomore3) ;#t

(define x 7)
(cond [(= x 7) 5]
      [(odd? x) 'odd-number]
      [else 'even-number])

(cond [(even? x) 'even-number]
      [(= x 7) 5]
      [else 'odd-number])

;; Recursion

(define (my-length a-list)
  (if (empty? a-list)
      0
      (add1 (my-length (rest a-list)))))

(my-length '(list with four symbols))

;; eqiality predicates

(struct point (x y) #:transparent)

(define (distance-to-origin p)
  (sqrt (+ (sqr (point-x p)) (sqr (point-y p)))))

(define pt1 (point -1 2))
(define pt2 (point -1 2))

(equal? pt1 pt2)

(eq? pt1 pt2)
(eq? pt1 pt1)

;; Testing

(check-equal? (add1 6) 7 "NUMBERS MATTER ERROR Message")

;; Chapter 4.5

(define WIDTH 100)
(define HEIGHT 200)
(unless (> HEIGHT 0)
  (error 'guess-my-number "HEIGHT may not be negative"))

(define X-CENTER (quotient WIDTH 2))
(define Y-CENTER (quotient HEIGHT 2))

;; local defenitions

(struct posn (x y))
(struct rectangle (width height))
(define (inside-of-rectangle? r p)
  (define x (posn-x p))
  (define y (posn-y p))
  (define width (rectangle-width r))
  (define height (rectangle-height r))
  (and (<= o x) (< x width) (<= 0 y) (< y height)))

  