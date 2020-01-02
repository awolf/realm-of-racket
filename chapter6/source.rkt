#lang racket

;                                 ;;
;     ;;; ;                        ;
;    ;   ;;                        ;
;    ;    ;   ;; ;;;      ;;;;     ;  ;;;     ;;;
;    ;         ;;   ;    ;    ;    ;  ;      ;   ;
;     ;;;;     ;    ;         ;    ; ;      ;     ;
;         ;    ;    ;    ;;;;;;    ;;;      ;;;;;;;
;    ;    ;    ;    ;   ;     ;    ;  ;     ;  
;    ;;   ;    ;    ;   ;    ;;    ;   ;     ;    ;
;    ; ;;;    ;;;  ;;;   ;;;; ;;  ;;  ;;;;    ;;;;

(require 2htdp/image 2htdp/universe)

(struct pit (snake goos) #:transparent)
(struct snake (dir segs) #:transparent)
(struct goo (loc expire) #:transparent)
(struct posn (x y) #:transparent)


(define TICK-RATE 1/10)

(define SIZE 30)

(define SEG-SIZE 15)

(define MAX-GOO 5)
(define EXPIRATION-TIME 150)

(define WIDTH-PX  (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-IMG  (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))

(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 15)


(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos  (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
        [else w]))

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (dead? w)
  (define snake (pit-snake w))
  (or (self-colliding? snake) (wall-colliding? snake)))

(define (render-end w)
  (overlay (text "Game over" ENDGAME-TEXT-SIZE "black")
           (render-pit w)))

(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (grow sn)
  (snake (snake-dir sn) (cons (next-head sn) (snake-segs sn))))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [(string=? dir "up") (posn-move head 0 -1)]
        [(string=? dir "down") (posn-move head 0 1)]
        [(string=? dir "left") (posn-move head -1 0)]
        [(string=? dir "right") (posn-move head 1 0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) 
                    (all-but-last (rest segs)))]))

(define (age-goo goos)
  (rot (renew goos)))

(define (renew goos)
  (cond [(empty? goos) empty]
        [(rotten? (first goos))
         (cons (fresh-goo) (renew (rest goos)))]
        [else
         (cons (first goos) (renew (rest goos)))]))

(define (rot goos)
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos))
                    (rot (rest goos)))]))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g))))

(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))

(define (dir? x)
  (or (string=? x "up") 
      (string=? x "down") 
      (string=? x "left") 
      (string=? x "right")))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (cond [(and (opposite-dir? (snake-dir the-snake) d) 
              ;; consists of the head and at least one segment:
              (cons? (rest (snake-segs the-snake))))
         (stop-with w)]
        [else 
         (pit (snake-change-dir the-snake d) 
              (pit-goos w))]))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up")    (string=? d2 "down")]
        [(string=? d1 "down")  (string=? d2 "up")]
        [(string=? d1 "left")  (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]))


;
;
;
; 
;                                      ;;
;   ;;;;;;                              ;
;    ;    ;                             ;
;    ;    ;     ;;;     ;; ;;;      ;;; ;     ;;;      ;;  ;;;
;    ;    ;    ;   ;     ;;   ;    ;   ;;    ;   ;      ;;;
;    ;;;;;    ;     ;    ;    ;   ;     ;   ;     ;     ;
;    ;  ;     ;;;;;;;    ;    ;   ;     ;   ;;;;;;;     ;
;    ;   ;    ;          ;    ;   ;     ;   ;           ;
;    ;    ;    ;    ;    ;    ;    ;   ;;    ;    ;     ;
;   ;;;   ;;    ;;;;    ;;;  ;;;    ;;; ;;    ;;;;     ;;;;;
; 
; 
; 
;                                                                                            
;; -----------------------------------------------------------------------------

(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene  (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake) 
             (cond [(string=? "up" dir) HEAD-UP-IMG]
                   [(string=? "down" dir) HEAD-DOWN-IMG]
                   [(string=? "left" dir) HEAD-LEFT-IMG]
                   [(string=? "right" dir) HEAD-RIGHT-IMG])
             snake-body-scene))


(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond [(empty? goos) empty]
          [else (cons (goo-loc (first goos))
                      (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos) GOO-IMG scene))


(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
        [else (img+scene (first posns)
                         img 
                         (img-list+scene (rest posns) img scene))]))


(define (img+scene posn img scene)
  (place-image img 
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))



(define (self-colliding? sn)
  (cons? (member (snake-head sn) (snake-body sn))))

(define (wall-colliding? sn)
  (define x (posn-x (snake-head sn)))
  (define y (posn-y (snake-head sn)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))



;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            
;     ;;;     ;;;  ;;; ;;;   ;;;  ;;;;;;;   ;;;;;     ;;;;;;;     ;;;     ;;;;;;   ;;;   ;;; 
;      ;;      ;    ;   ;     ;      ;        ;          ;         ;;      ;    ;   ;     ;  
;     ;  ;     ;    ;    ;   ;       ;        ;          ;        ;  ;     ;    ;    ;   ;   
;     ;  ;     ;    ;     ; ;        ;        ;          ;        ;  ;     ;    ;     ; ;    
;     ;  ;     ;    ;      ;         ;        ;          ;        ;  ;     ;;;;;       ;     
;    ;;;;;;    ;    ;     ; ;        ;        ;    ;     ;       ;;;;;;    ;  ;        ;     
;    ;    ;    ;    ;    ;   ;       ;        ;    ;     ;       ;    ;    ;   ;       ;     
;   ;      ;   ;    ;   ;     ;      ;        ;    ;     ;      ;      ;   ;    ;      ;     
;  ;;;    ;;;   ;;;;   ;;;   ;;;  ;;;;;;;   ;;;;;;;;  ;;;;;;;  ;;;    ;;; ;;;   ;;   ;;;;;   
;                                                                                            
;                                                                                            
;                                                                                            
;
;; -----------------------------------------------------------------------------
;; Posn Posn -> Boolean
;; Are the two posns are equal?
;; > (posn=? (posn 1 1) (posn 1 1))
;; true
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

;; Access the head position of the snake.
;; snake-head : Snake -> Seg
;; > (snake-head (snake "right" (list (posn 1 1) (posn 2 1)))
;; (posn 1 1)
(define (snake-head sn)
  (first (snake-segs sn)))

;; Snake -> [Listof Segs]
;; returns the snake's body.
;; That is everyting that isn't the snake's head.
(define (snake-body sn)
  (rest (snake-segs sn)))

;; Snake Direction -> Snake 
(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))
