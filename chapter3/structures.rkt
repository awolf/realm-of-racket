#lang racket

;; Struct Basics

(struct student (name id# dorm) #:transparent)

(define freshman1 (student 'Joe 1234 'NewHall))

(student-name freshman1)

(student-id# freshman1)

(define mimi (student 'Mimi 1234 'NewHall))
(define nicole (student 'Nicole 5678 'NewHall))
(define rose (student 'Rose 8765 'NewHall))
(define eric (student 'Eric 4321 'NewHall))
(define in-class (list mimi nicole rose eric))

(student-id# (third in-class))

(struct student-body (freshman sophomores juniors seniors) #:transparent)

(define all-students
  (student-body (list freshman1 (student 'Mary 0101 'OldHall))
                (list (student 'Jeff 5678 'OldHall))
                (list (student 'bob 4321 'Apartment))
                empty))

(student-name (first (student-body-freshman all-students)))
(student-name (second (student-body-freshman all-students)))
(student-name (first (student-body-juniors all-students)))

