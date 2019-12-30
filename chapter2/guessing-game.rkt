#lang racket


#|
   The Guess My Number game, played at DrRacket's REPL
   ---------------------------------------------------
   
   You pick a number. The program guesses the nunber, 
   by asking you questions. Your responses are "too 
   small" "too large" or "you guessed it". 

   Play
   ----

   Click Run. Pick a number X between <n> and <m>. 
   Evaluate 
     (start <n> <m>)
   The program will respond with a number.
   Use
     (bigger)
   and
     (smaller)
    to let it know what you think of its guess.
|#

;                                          
;                                          
;                                          
;                          ;               
;                          ;               
;  ;;;   ;;;                               
;   ;;   ;;                                
;   ; ; ; ;     ;;;;     ;;;      ;; ;;;   
;   ; ; ; ;    ;    ;      ;       ;;   ;  
;   ; ; ; ;         ;      ;       ;    ;  
;   ;  ;  ;    ;;;;;;      ;       ;    ;  
;   ;     ;   ;     ;      ;       ;    ;  
;   ;     ;   ;    ;;      ;       ;    ;  
;  ;;;   ;;;   ;;;; ;;  ;;;;;;;   ;;;  ;;; 
;                                          
;                                          
;                                          
;

;; Example:
;; > (start 0 100)       ; [0,100]
;; 50
;; > (bigger)            ; [51,100]
;; 75
;; > (bigger)            ; [76,100]
;; 88
;; > (smaller)           ; [76,87]
;; 82

;; Number Number -> Number
;; Start a new game in [n,m] and make a guess.
;; > (start 0 100)
;; 50
(define (start n m)
  (set! lower (min n m))
  (set! upper (max n m))
  (guess))

;; the lower and upper bounds on the number
(define lower 1)
(define upper 100)

;; -> Nuber Number
;; Guess half-way between lower and upper bounds.
;; > (begin (start 0 100) (guess))
;; 50
(define (guess)
  (quotient (+ lower upper) 2))

;; -> Number
;; Lower the upper bound and guess again.
;; > (begin (start 0 100) (smaller))
;; 24
(define (smaller)
  (set! upper (max lower (sub1 (guess))))
  (guess))

;; -> Number
;; Raise the lower bound and guess again.
;; > (begin (start 0 100) (bigger))
;; 75
(define (bigger)
  (set! lower (min upper (add1 (guess))))
  (guess))
