#lang slideshow

(define (checker p1 p2) ;; rearranges shapes into a 2x2 grid
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(four (circle 10)) ;; prints a 2x2 grid of circles

(define (square n)
  (filled-rectangle n n)) ;; creates a square object using 2 rectangles

(checker (colorize (circle 10) "red")
         (colorize (circle 10) "black"))
;; prints a small 2x2 circle grid in red and black circles

(checker (colorize (square 10) "red")
         (colorize (square 10) "black"))
;; prints a small 2x2 square grid in red and black filled squares

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))

(checkerboard (square 10))

;; map(func, lst) returns a new list with each item applied to a function
(map (λ (x) (add1 x)) (list 1 2 3)) ;; '(2 3 4)
(map (λ (x y) (- x y)) (list 1 2 3) (list 9 8 7)) ;;'(-8 -6 -4)

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple" "brown" "silver")))

(rainbow (square 25))
(apply vc-append (rainbow (square 25)))



(apply + '(1 2 3)) ;; 6
(apply + 1 2 '(3)) ;; 6
(apply + 1 3 4 '(3)) ;; 11

(define ht (make-hash)) ;; creates a hash table
(hash-set! ht "apple" '(red round)) ;; creates a item of key 'apple' and value 'red round'
(hash-set! ht "banana" '(yellow long)) ;; creates a item of key 'banana' and value 'yellow long'
(hash-ref ht "apple")
(hash-ref ht "banana")
;; (hash-ref ht "red round") doesnt work, not a key

(define ht1 #hash(("apple" . red)
                   ("banana" . yellow)))
(hash-ref ht1 "apple")



