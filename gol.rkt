#lang racket
(require 2htdp/image)
(require 2htdp/universe)



;;;;
;;;; Data structurds
;;;;
(struct posn (x y) #:transparent)
(struct cell (population posn) #:transparent)

;;;;
;;;; helper functions
;;;;

;;posn-to-vec-loc :: posn -> number -> posn
(define (posn->vec-loc posn world-length)
    (+ (posn-x posn) (* (posn-y posn) world-length)))


;;xy-tranny :: list number -> list number -> list pair
(define (xy-tranny x-transform y-transform)
  (for/list ([x x-transform]
             [y y-transform])
    (cons x y)))

;;explode-posn :: posn -> list pairs -> list  
(define (explode-posn target-posn xy-pairs)
(for/list ([xy xy-pairs])
               (posn (+ (posn-x target-posn) (car xy))
                     (+ (posn-y target-posn) (cdr xy)))))


(define (get-cell posn world world-length)
         (vector-ref world (posn->vec-loc posn world-length)))



;;;;
;;;; world definitions
;;;;
(define WORLD-LENGTH 10)

;;make-world :: nunber -> world (vector cells)
(define (make-world)
  (for*/vector ([y WORLD-LENGTH][x WORLD-LENGTH])
    (cell 0 (posn x y))))


;;make-blinker :: posn -> list posn
(define (make-blinker center-posn)
  (let* ([x-transform '(-1 0 1)]
         [y-transform '(0 0 0)]
         [xy-pairs (xy-tranny x-transform y-transform)])
    (explode-posn center-posn xy-pairs)))


;;min-blinker-world :: world
(define (min-blinker-world)
  (set! WORLD-LENGTH 3)
  (vector
 (cell 0 (posn 0 0))
 (cell 0 (posn 1 0))
 (cell 0 (posn 2 0))
 (cell 1 (posn 0 1))
 (cell 1 (posn 1 1))
 (cell 1 (posn 2 1))
 (cell 0 (posn 0 2))
 (cell 0 (posn 1 2))
 (cell 0 (posn 2 2))))

(define world-template-10
  '(
    0 1 0 0 0 0 0 0 0 0
    0 0 1 0 0 0 0 1 1 1
    1 1 1 0 0 0 1 1 1 0
    0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 0 1 1 0 0 0 0 0 0
    1 1 0 1 1 0 0 0 0 0
    1 1 1 1 0 0 0 0 0 0
    0 1 1 0 0 0 0 0 0 0
    ))

(define (world-10)
  (set! WORLD-LENGTH 10)
  (let ([blank-world (make-world)]
        [max-pop (* WORLD-LENGTH WORLD-LENGTH)])
  (for/vector ([cell-pop world-template-10]
               [loc max-pop])
    (let ([a-cell (vector-ref blank-world loc)])
      (struct-copy cell a-cell [population cell-pop])))))
;;;;
;;;; world processing definitions
;;;;


;;get-naybor-posns :: posn -> list posn 
(define (get-naybor-posns target-posn)
  (let* ([x-transform '(-1 0 1 -1 1 -1 0 1)]
         [y-transform '(-1 -1 -1 0 0 1 1 1)]
         [xy-pairs (xy-tranny x-transform y-transform)])
    (explode-posn target-posn xy-pairs)))


(define (valid-naybor? position)
  (cond
    [(and (< (posn-x position) WORLD-LENGTH)
          (< (posn-y position) WORLD-LENGTH)
          (not (negative? (posn-x position)))
          (not (negative? (posn-y position))))
     #true]
    [else #false]))


;;get-naybors :: list posn -> world -> number -> list cells
(define (get-naybors naybor-posns world)
  (for/list ([naybor-posn naybor-posns]
              #:when (valid-naybor? naybor-posn))
    (get-cell naybor-posn world WORLD-LENGTH)))



;;the-reckoning :: posn -> world (vector cells) -> number -> number
(define (the-reckoning current-posn world)
  (define (count-heads naybors)
    (for/sum ([cell naybors])
      (cell-population cell)))
  (count-heads (get-naybors (get-naybor-posns current-posn) world)))
  

;;life-or-death? :: cell -> world -> WORLD-LENGTH -> bool
(define (life-or-death? cell world)
  (let* ([naybor-population (the-reckoning (cell-posn cell) world)]
         [still-living? (and (= 1 (cell-population cell))
                             (= 2 naybor-population))]
         [birth? (= 3 naybor-population)])
    (cond [still-living? #true]
          [birth? #true]
          [else #false])))


;;hand-of-god :: world (vector cells) -> number -> list posn
(define (hand-of-god world)
  (for/list ([cell world]
             #:when (life-or-death? cell world))
    (cell-posn cell)))


;;a-whole-new-world :: world -> world
(define (a-whole-new-world world)
  (define population-posns (hand-of-god world))
  (define new-world (make-world))
  (for ([a-posn population-posns])
    (let* ([vec-posn (posn->vec-loc a-posn WORLD-LENGTH)]
           [current-cell (vector-ref new-world vec-posn)]
           [new-cell (struct-copy cell
                                current-cell
                                [population 1])])
      (vector-set! new-world vec-posn new-cell)))
  new-world)


;;process-population :: world -> list posn
(define (process-population world)
  (for/list ([a-cell world]
             #:when
             (= 1 (cell-population a-cell)))
    (cell-posn a-cell)))

  
;;;;
;;;; rendering definitions
;;;;
(define PIXEL-SIZE 50)


;;populated-pixel :: number -> image
(define populated-pixel (square PIXEL-SIZE "solid" "black"))
(define kyle 
  (scale (/ PIXEL-SIZE 100)
         (crop 60 60 100 100 (bitmap/file "./kyle.jpeg"))))
;;background :: number -> image
(define (background)
  (let ([actual-length (* WORLD-LENGTH PIXEL-SIZE)])
  (empty-scene actual-length actual-length)))


;;world-scene :: world -> image
(define (world-scene world)
  (define loc-list (process-population world))
  (define (build-scene image locations)
    (cond
      [(empty? locations) image]
      [else
       (define loc (car locations))
       (build-scene (underlay/xy image
                                 (* PIXEL-SIZE (posn-x loc))
                                 (* PIXEL-SIZE (posn-y loc))
                                 populated-pixel)
                    (cdr locations))]))
  (build-scene (background) loc-list))



;;;;
;;;; make it happen!
;;;;

(define (quit w key)
  (cond [(key=? key "q") (stop-with w)]
        [else w]))


(big-bang (world-10)
          (on-tick a-whole-new-world 1/8)
          (to-draw world-scene)
 ;         (record? "./images/")
          (on-key quit))

(provide (all-defined-out))
