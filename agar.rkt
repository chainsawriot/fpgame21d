#lang racket
(require 2htdp/image)
(require picturing-programs)
(require 2htdp/universe)
(require lang/posn)

(struct agar (xpos ypos agsize)
  #:transparent
  #:guard (λ (xpos ypos agsize type-name)
            (cond
              [(<= xpos 0) (values 1 ypos agsize)]
              [(>= xpos 800) (values 799 ypos agsize)]
              [(<= ypos 0) (values xpos 1 agsize)]
              [(>= ypos 600) (values xpos 599 agsize)]
              [else (values xpos ypos agsize)])))


(struct allagars (red-agar
                  blue-agars))

(define agarstate (allagars (agar 20 20 20) (list (agar 10 10 10) (agar 50 50 10))))

(define (ran-move)
  (define move-step (random 3))
  (if (= move-step 2)
      -20
      (* move-step 20)))

(define (brownian magar)
  (agar (+ (agar-xpos magar) (ran-move)) (+ (agar-ypos magar) (ran-move)) (agar-agsize magar)))

;(define curstate (all-agars (agar 20 20 20) (list (agar 10 10 5) (agar 50 50 5) (agar 60 60 2))))
;(agar-xpos (all-agars-red-agar curstate))

(define GAME-SCENE (empty-scene 800 600))

(define (key-press w key)
  (cond [(key=? key "right") (agar (+ (agar-xpos w) 10) (agar-ypos w) (agar-agsize w))]
        [(key=? key "left") (agar (- (agar-xpos w) 10) (agar-ypos w) (agar-agsize w))]
        [(key=? key "down") (agar (agar-xpos w) (+ (agar-ypos w) 10) (agar-agsize w))]
        [(key=? key "up") (agar (agar-xpos w) (- (agar-ypos w) 10) (agar-agsize w))]
        [else w]))

(define (draw-agar w)
      (place-image (circle (agar-agsize w) "solid" "red") 
                   (agar-xpos w) 
                   (agar-ypos w) 
                   GAME-SCENE))

(define (update-red xpos ypos agsize mallagars)
  (allagars (agar xpos ypos agsize) (allagars-blue-agars mallagars)))

(define curstate (agar 200 200 20))


(define (update-agar w)
  w)

(big-bang curstate
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
