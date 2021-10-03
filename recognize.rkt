;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       



;; 3a)
;; These are helper functions. 

;;get-x returns x value of a point
;;get-x: (listof Num) -> (listof Num) ;requires list of len 2
(define (get-x point)
  (first point))

(check-expect (get-x (list 0 100)) 0)
(check-expect (get-x (list 30 20)) 30)

;;get-y returns y value of a point
;;get-y: (listof Num) -> (listof Num) ;requires list of len 2
(define (get-y point)
  (second point))

(check-expect (get-y (list 0 100)) 100)
(check-expect (get-y (list 30 20)) 20)

;;translate-gesture consumes gesture and x-offset and y-offset and returns list gesture of list(Num, Num)
;;modified so that each x-value of each list in gesture is offset
;;by param x-offset and each y is offset by param y-offset
;;translate-gesture: (listof (list Num Num)) Num Num -> (listof (list Num Num))

(define (translate-gesture gesture x-offset y-offset)
  (cond[(empty? gesture) empty]
       [(cons? gesture) (cons (list (+ (get-x (first gesture)) x-offset)
                                    (+ (get-y (first gesture)) y-offset))
                              (translate-gesture (rest gesture) x-offset y-offset))]))

(check-expect (translate-gesture (list (list 3 5) (list 4 5) (list 2 6)) 3 5) (list (list 6 10) (list 7 10) (list 5 11)))
(check-expect (translate-gesture (list (list 1 1) (list 2 2)) 1 1) (list (list 2 2) (list 3 3)))

;;scale-gesture consumes gesture and x-offset and y-offset and returns list gesture of list(Num, Num)
;;modified so that each x-value of each list in gesture is multiplied
;;by param x-offset and each y is multiplied by param y-offset
;;scale-gesture: (listof (list Num Num)) Num Num -> (listof (list Num Num)) x-offset > 0 & y-offset > 0
(define (scale-gesture gesture x-offset y-offset)
  (cond[(empty? gesture) empty]
       [(cons? gesture) (cons (list (* (get-x (first gesture)) x-offset)
                                    (* (get-y (first gesture)) y-offset))
                              (scale-gesture (rest gesture) x-offset y-offset))]))

(check-expect (scale-gesture (list (list 1 2) (list 3 4)) 2 3) (list (list 2 6) (list 6 12)))
(check-expect (scale-gesture (list (list 2 2) (list 3 3)) 3 4) (list (list 6 8) (list 9 12)))

(define (get-xs gesture)
  (cond[(empty? gesture) empty]
       [(cons? gesture) (cons (get-x (first gesture)) (get-xs (rest gesture)))]))
 
(define (get-ys gesture)
  (cond[(empty? gesture) empty]
       [(cons? gesture) (cons (get-y (first gesture)) (get-ys (rest gesture)))]))

(define (find-min lst)
   (cond
     [(empty? (rest lst)) (first lst)]
     [else (min (first lst) (find-min (rest lst)))]))

(define (find-max lst)
   (cond
     [(empty? (rest lst)) (first lst)]
     [else (max (first lst) (find-max (rest lst)))]))

;;get-b-box consumes gesture and returns 2 pairs of coordinates
;;that correspond to box boundary of gesture
;;get-b-box: (listof (list Num Num)) -> (list Num Num) (list Num Num)

(define (get-b-box gesture)
  (list (list (find-min (get-xs gesture)) (find-min (get-ys gesture)))
        (list (find-max (get-xs gesture)) (find-max (get-ys gesture)))))

(check-expect (get-b-box (list (list 100 0) (list 200 100) (list 100 200) (list 0 100) (list
100 0))) (list (list 0 0) (list 200 200)))

(check-expect (get-b-box (list (list 0 0) (list 100 0) (list 100 100) (list 0 100) (list
0 0))) (list (list 0 0) (list 100 100)))

;; 3b)
;; Full design recipe required.

(define (xpart gesture)
 (expt (- (get-x (second gesture)) (get-x (first gesture))) 2))

(define (ypart gesture)
 (expt (- (get-y (second gesture)) (get-y (first gesture))) 2))

;;gesture-length returns the sum of distances between coordinates in list gesture
;;gesture: (listof (list Num Num)) -> Num

(define (gesture-length gesture)
  (cond[( < (length gesture) 2) 0]
       [(cons? gesture) ( + (sqrt ( + (xpart gesture)
                            (ypart gesture))) (gesture-length (rest gesture)))]))

(check-expect (gesture-length (list (list 0 0) (list 50 0) (list 50 50) (list 0 50) (list 0 0))) 200)
(check-within (gesture-length (list (list 0 0) (list 1 0) (list 1 1) (list 0 1) (list 0 0))) 4 .01)

(check-expect (gesture-length (list (list 0 0) (list 100 0) (list 100 100) (list 0 100) (list 0 0))) 400)
(check-within (gesture-length (list (list 100 0) (list 200 100) (list 100 200) (list 0 100) (list 100 0))) 565.68 .01)
(check-expect (gesture-length (list 0)) 0)

(define (numat g num)
  (cond [(= num 0) (first g)]
        [else (numat (rest g) (sub1 num))]))

(check-expect (numat (list 1 2 3 4) 2) 3)

;;get-points consumes Gesture g and list of Nat lst and
;;returns list of elements of g as indexed by the elements of lst
;;get-points: (listof (list Num Num)) (listof Nat) -> (listof (list Num Num))

(define (get-points g lst)
  (cond [(empty? lst) empty]
        [(cons? g) (cons (numat g (first lst)) (get-points g (rest lst)))]))

(check-expect (get-points (list (list 100 0) (list 200 100) (list 100 200) (list 0 100) (list 100 50))
                          (list 0 0 2 4 4)) (list (list 100 0) (list 100 0) (list 100 200) (list 100 50) (list 100 50)))
(check-expect (get-points (list (list 0 0) (list 200 0))
                          (list 1 0)) (list (list 200 0) (list 0 0)))

;; 3c) Starter code definitions

;; 3ci)
;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.

(define (getnumlast gesture)
  (cond[(empty? gesture) 0]
      [else (+ 1 (getnumlast (rest gesture)))]))  


;;five-sample consumes a Gesture and returns a new gesture with 5 points with the first one being the first of gesture
;;the 2-4 points being the points in the (floor (.25 * n)) where n is length of gesture
;;index of gesture with .5 and .75 replacing .25 for 2 -4 and the last point is the last point in gesture
;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty      
(define (five-sample gesture)
  (list (first gesture) (numat gesture (floor (* .25 (getnumlast gesture))))
        (numat gesture (floor (* .5 (getnumlast gesture))))
        (numat gesture (floor (* .75 (getnumlast gesture))))
        (numat gesture (- (getnumlast gesture) 1))))  
                 
       
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))


;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty



;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))


;; 3cii)

;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
(define (move-and-scale gesture x-scale y-scale)
  (scale-gesture (translate-gesture gesture (- 0 (get-x (first (get-b-box gesture))))
                                    (- 0 (get-y (first (get-b-box gesture))))) x-scale y-scale))

;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0




;; Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))


;; 3ciii)

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size
;; Examples:

(define (normalize-gesture gesture)
  (cond [( < (- (first (second (get-b-box gesture))) (get-x (first (get-b-box gesture)))) min-width)
         (move-and-scale gesture 1 (/ norm-size (- (get-y (second (get-b-box gesture))) (get-y (first (get-b-box gesture))))))]
        [( < (- (second (second (get-b-box gesture))) (second (first (get-b-box gesture)))) min-height)
         (move-and-scale gesture (/ norm-size (- (get-x (second (get-b-box gesture))) (get-x (first (get-b-box gesture))))) 1)]
        [else (move-and-scale gesture (/ norm-size (- (get-x (second (get-b-box gesture))) (get-x (first (get-b-box gesture)))))
                              (/ norm-size (- (get-y (second (get-b-box gesture))) (get-y (first (get-b-box gesture))))))]))
          
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01) 

;; normalize-gesture: Gesture -> Gesture
;; requires: gesture is not both vertical and horizontal
;;           gesture is non-empty




;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)


(sqrt ( + (expt (- (get-x (first (normalize-gesture (five-sample  (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))))))
                    (get-x (first (normalize-gesture (five-sample (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40))))))) 2)
                     (expt (- (get-y (first (normalize-gesture (five-sample  (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))))))
                    (get-y (first (normalize-gesture (five-sample (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40))))))) 2)))
 

;; 3civ)  

(define (totaldist gest1 gest2) 
  (cond[(empty? gest1) 0]   
  [(cons? gest1) ( + (sqrt ( + (expt (- (get-x (first gest1))
                    (get-x (first gest2))) 2)
                     (expt (- (get-y (first gest1))
                    (get-y (first gest2))) 2))) (totaldist (rest gest1) (rest gest2)))]))

(define (geometric-5match gest1 gest2)
  ( / (totaldist (normalize-gesture (five-sample gest1)) (normalize-gesture (five-sample gest2))) 5))
  
;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;; Examples: 
(check-within (geometric-5match 
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               16.16 0.01) 

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal




;; Tests:



;; 3cv)



(define (collectdif gesture templates)
  (cond [(empty? templates) empty]
         [(cons? templates) (cons (geometric-5match gesture (second (first templates))) 
                                  (collectdif gesture (rest templates)))])) 

(define (indexof num lst)
  (cond[(= num (first lst)) 0]
       [(cons? lst) (+ 1 (indexof num (rest lst)))]))

(define (letterAt num lst)
  (cond[(= num 0) (first (first lst))]
       [else (letterAt (sub1 num) (rest lst))]))  


(define (five-point-rec gesture templates)
 (letterAt (indexof (find-min (collectdif gesture templates)) (collectdif gesture templates)) templates))
    


;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)


;; five-point-rec Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal

;; Tests
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)
;;

(define (sub-sample1 x gesture k)
  (cond [(= x (- k 1))  (cons (numat gesture (- (getnumlast gesture) 1)) empty)] 
        [else (cons (numat gesture (floor (* (/ x (- k 1)) (getnumlast gesture))))
                    (sub-sample1 (+ 1 x) gesture k))]))

;;sub-sample consumes a ne-Gesture and produces a new gesture with k points
;; (ne-listof (list Num Num)) Num -> (ne-listof (list Num Num))
(define (sub-sample gesture k)    
  (cons (first gesture) (sub-sample1 1 gesture k)))

(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)) 10) (list
 (list 1 1)
 (list 1 1)
 (list 1 1)
 (list 2 2)
 (list 2 2)
 (list 3 3)
 (list 3 3)
 (list 4 4)
 (list 4 4)
 (list 4 4)))

(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)) 5) 
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (sub-sample (list (list 1 1)) 2)
              (list (list 1 1) (list 1 1)))
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)) 5)
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))

;;geometric-match produces the average distance between
;;points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;;geometric-match (ne-listof (list Num Num)) (ne-listof (list Num Num)) Num -> Num
(define (geometric-match gest1 gest2 k) 
  ( / (totaldist (normalize-gesture (sub-sample gest1 k)) (normalize-gesture (sub-sample gest2 k))) k))

(check-within (geometric-match 
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 5)
               16.16 0.01)

(check-within (geometric-match 
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)) 8) 13.46 .01)


(define (collectdif2 gesture templates k)
  (cond [(empty? templates) empty]
         [(cons? templates) (cons (geometric-match gesture (second (first templates)) k) 
                                  (collectdif2 gesture (rest templates) k))])) 


;; k-point-rec Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal
;; k-point-rec consumes gesture and sub-samples it with param k points and returns
;;the letter in templates that the gesture is closest to
(define (k-point-rec gesture templates k) 
 (letterAt (indexof (find-min (collectdif2 gesture templates k))
                    (collectdif2 gesture templates k)) templates))

(check-expect (k-point-rec testd templates 30) 'd)
(check-expect (k-point-rec testk templates 30) 'k)


(check-expect (k-point-rec tests templates 30) 's)
(check-expect (k-point-rec testy templates 30) 'y)      

