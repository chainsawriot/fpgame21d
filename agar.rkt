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
  (allagars (agar 600 600 20)
            (list (agar 60 80 200)
                  (agar 60 80 100)
                  (agar 60 80 60)
                  (agar 60 80 70)
                  (agar 60 80 50)
                  (agar 60 80 30)
                  (agar 60 80 20)
                  (agar 60 80 10)
                  (agar 60 80 10)
                  (agar 60 80 10)
                  (agar 60 80 10))))

(define (ran-move)
  (define move-step (random 3))
  (if (= move-step 2)
      -10
      (* move-step 10)))

(define (brownian magar)
  (agar (+ (agar-xpos magar) (ran-move)) (+ (agar-ypos magar) (ran-move)) (agar-agsize magar)))

;(define curstate (all-agars (agar 20 20 20) (list (agar 10 10 5) (agar 50 50 5) (agar 60 60 2))))
;(agar-xpos (all-agars-red-agar curstate))


(define (update-red xpos ypos agsize mallagars)
  (allagars (agar xpos ypos agsize) (allagars-blue-agars mallagars)))

;; TODO: Moving speed should be reduce when agsize increase

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


(define (put-agar agar color)
  (overlay (text (number->string (agar-agsize agar)) (if (> (agar-agsize agar) 200) 200 (agar-agsize agar)) "white")
           (circle (agar-agsize agar) "solid" color)))


(define (draw-agar w)
  ; Draw red agar first
  (cond [(= (agar-agsize (allagars-red-agar w)) 0) (place-image (text "GAME OVER" 100 "blue") 300 300 GAME-SCENE)]
        [(null? (allagars-blue-agars w)) (place-image (text "YOU WIN!" 100 "red") 300 300 GAME-SCENE)]
        [else (define z (place-image (put-agar (allagars-red-agar w) "red") 
                   (agar-xpos (allagars-red-agar w)) 
                   (agar-ypos (allagars-red-agar w)) 
                   GAME-SCENE))
              (place-images
               (map (λ (x) (put-agar x "blue")) (allagars-blue-agars w))
               (map (λ (x) (make-posn (agar-xpos x) (agar-ypos x))) (allagars-blue-agars w)) z)]))


;(define curstate (agar 200 200 20))


(define (collision? agar1 agar2)
  (define distance (sqrt (+ (expt (- (agar-xpos agar1) (agar-xpos agar2)) 2) (expt (- (agar-ypos agar1) (agar-ypos agar2)) 2))))
  (> 0 (- distance (agar-agsize agar1) (agar-agsize agar2))))

;; return list of list, 0: elimited blue agars, 1: remaining blue agars

(define (eliminate-blue-agar w)
  (list (filter (λ (x) (collision? x (allagars-red-agar w))) (allagars-blue-agars w))
  (filter (λ (x) ((negate collision?) x (allagars-red-agar w))) (allagars-blue-agars w))))

; TODO: eliminate blue-agars
; increase size of red-agars (absorb)
; determine ending

(define (update-agar w)
  (define current-red (allagars-red-agar w))
  (define current-blue (eliminate-blue-agar w))
  (define absorb-size (foldr + 0 (map (λ (x) (agar-agsize x)) (list-ref current-blue 0))))
  (if (ormap (λ (x) (> (agar-agsize x) (agar-agsize current-red))) (list-ref current-blue 0)) (allagars (agar 0 0 0) (list-ref current-blue 1)) 
  (allagars (agar (agar-xpos current-red) (agar-ypos current-red) (+ absorb-size (agar-agsize current-red)))
            (map (λ (x) (brownian x)) (list-ref current-blue 1)))))

(define (red-die? w)
  (= (agar-agsize (allagars-red-agar w)) 0))

(big-bang agarstate
          (to-draw draw-agar)
          (on-tick update-agar)
          (on-key key-press)
          (stop-when red-die?))

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



  