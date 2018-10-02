#lang racket
(define x 5)
(cond [(= x 7) 5]
         [(odd? x) 'odd-number]
         [else 'even-number])


(define (my-length a-list)
  (if (empty? a-list)
      0
      (add1 (my-length (rest a-list)))))


;; 4.4 Equality Predicates, Once More

(struct point (x y) #:transparent)

(define (distance-to-origin p)
  (sqrt (+ (sqr (point-x p)) (sqr (point-y p)))))

(define pt1 (point -1 2))
(define pt2 (point -1 2))

(equal? pt1 pt2)

(define pt3 pt1)

(define (eq-first-items list1 list2)
    (eq? (first list1) (first list2)))

(eq-first-items (cons pt1 empty) (cons pt3 empty))
(eq-first-items (cons pt1 empty) (cons pt2 empty))

;; 4.5 Comparing and Testing

(require rackunit)

(check-equal? (add1 5) 6)
;; (check-equal? (add1 5) 7) TEST FAIL!!

(check-equal? 5 5 "NUMBERS MATTER!")
;;(check-equal? 5 6 "NUMBERS MATTER!") TEST FAIL!!