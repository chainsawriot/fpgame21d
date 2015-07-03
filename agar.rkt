#lang racket
(require 2htdp/image)
(require picturing-programs)
(require 2htdp/universe)
(require lang/posn)

(define SCENEX 1024)
(define SCENEY 700)
(define GAME-SCENE (empty-scene SCENEX SCENEY))


(struct agar (xpos ypos agsize)
  #:transparent
  #:guard (λ (xpos ypos agsize type-name)
            (cond
              [(<= xpos 0) (values 1 ypos agsize)]
              [(>= xpos SCENEX) (values (- SCENEX 1) ypos agsize)]
              [(<= ypos 0) (values xpos 1 agsize)]
              [(>= ypos SCENEY) (values xpos (- SCENEY 1) agsize)]
              [else (values xpos ypos agsize)])))


(struct allagars (red-agar
                  blue-agars))

(define agarstate
  (allagars (agar 700 700 20)
            (list (agar 60 80 100)
                  (agar 60 80 10)
                  (agar 60 80 10)
                  (agar 60 80 10)
                  (agar 60 80 10)
                  (agar 60 80 10)
                  (agar 60 80 10)
                  (agar 60 80 10)
                  (agar 60 80 10)
                  (agar 60 80 10)
                  (agar 60 80 10))))

(define (ran-move)
  (define move-step (random 3))
  (if (= move-step 2)
      -20
      (* move-step 20)))

(define (brownian magar)
  (agar (+ (agar-xpos magar) (ran-move)) (+ (agar-ypos magar) (ran-move)) (agar-agsize magar)))

;(define curstate (all-agars (agar 20 20 20) (list (agar 10 10 5) (agar 50 50 5) (agar 60 60 2))))
;(agar-xpos (all-agars-red-agar curstate))


(define (update-red xpos ypos agsize mallagars)
  (allagars (agar xpos ypos agsize) (allagars-blue-agars mallagars)))


(define (key-press w key)
  (cond [(key=? key "right") (update-red  (+ (agar-xpos (allagars-red-agar w)) 10) (agar-ypos (allagars-red-agar w)) (agar-agsize (allagars-red-agar w)) w)]
        [(key=? key "left") (update-red (- (agar-xpos (allagars-red-agar w)) 10) (agar-ypos (allagars-red-agar w)) (agar-agsize (allagars-red-agar w)) w)]
        [(key=? key "down") (update-red (agar-xpos (allagars-red-agar w)) (+ (agar-ypos (allagars-red-agar w)) 10) (agar-agsize (allagars-red-agar w)) w)]
        [(key=? key "up") (update-red (agar-xpos (allagars-red-agar w)) (- (agar-ypos (allagars-red-agar w)) 10) (agar-agsize (allagars-red-agar w)) w)]
        [else w]))

;(define (key-press w key)
;  (cond [(key=? key "right") (agar (+ (agar-xpos w) 10) (agar-ypos w) (agar-agsize w))]
;        [(key=? key "left") (agar (- (agar-xpos w) 10) (agar-ypos w) (agar-agsize w))]
;        [(key=? key "down") (agar (agar-xpos w) (+ (agar-ypos w) 10) (agar-agsize w))]
;        [(key=? key "up") (agar (agar-xpos w) (- (agar-ypos w) 10) (agar-agsize w))]
;        [else w]))

(define (draw-agar w)
  ; Draw red agar first
  (define z (place-image (circle (agar-agsize (allagars-red-agar w)) "solid" "red") 
                   (agar-xpos (allagars-red-agar w)) 
                   (agar-ypos (allagars-red-agar w)) 
                   GAME-SCENE))
  (place-images
   (map (λ (x) (circle (agar-agsize x) "solid" "blue")) (allagars-blue-agars w))
   (map (λ (x) (make-posn (agar-xpos x) (agar-ypos x))) (allagars-blue-agars w)) 
   z))


;(define curstate (agar 200 200 20))


(define (update-agar w)
  (allagars (allagars-red-agar w) (map (λ (x) (brownian x)) (allagars-blue-agars w))))

(big-bang agarstate
          (to-draw draw-agar)
          (on-tick update-agar)
          (on-key key-press))

;(define x (place-image  (circle 20 "solid" "red") 60 60 GAME-SCENE))
;(place-image (text "Agar size:" 10 "black") 30 20 (place-image (circle 5 "solid" "blue") 100 100 x))

;(place-images
;   (list (circle 20 "solid" "red")
;         (circle 5 "solid" "blue")
;         (text "Agar size:" 10 "black"))
;   (list (make-posn 60 60)
;         (make-posn 100 100)
;         (make-posn 30 20))
;   GAME-SCENE)


