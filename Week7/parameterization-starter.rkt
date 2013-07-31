;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname parameterization-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (area r) 
  (* pi (sqr r))
  )

;(* pi (sqr 4)) ;area of circle diameter 4
;(* pi (sqr 6)) ;area of circle diameter 6

(area 4)
(area 6)


;; ====================

;; ListOfString -> Boolean
;; produce true if los includes "UBC"
(check-expect (contains-ubc? empty) false)
(check-expect (contains-ubc? (cons "McGill" empty)) false)
(check-expect (contains-ubc? (cons "UBC" empty)) true)
(check-expect (contains-ubc? (cons "McGill" (cons "UBC" empty))) true)

;(define (contains-ubc? los) false) ;stub

(define (contains-ubc? los) (contains? los "UBC"))
;  (cond [(empty? los) false]
;        [else
;         (if (string=? (first los) "UBC")
;             true
;             (contains-ubc? (rest los)))]))

;; ListOfString -> Boolean
;; produce true if los includes "McGill"
(check-expect (contains-mcgill? empty) false)
(check-expect (contains-mcgill? (cons "UBC" empty)) false)
(check-expect (contains-mcgill? (cons "McGill" empty)) true)
(check-expect (contains-mcgill? (cons "UBC" (cons "McGill" empty))) true)

;(define (contains-mcgill? los) false) ;stub

(define (contains-mcgill? los) (contains? los "McGill"))
;  (cond [(empty? los) false]
;        [else
;         (if (string=? (first los) "McGill")
;             true
;             (contains-mcgill? (rest los)))]))


; (listof String) String -> Boolean
; interp. given a list and the name of a school, search the list for the school with that name 
; produce true if los contains s
(check-expect (contains? empty "UBC") false)
(check-expect (contains? (cons "McGill" empty) "UBC") false)
(check-expect (contains? (cons "UBC" empty) "UBC") true)
(check-expect (contains? (cons "McGill" (cons "UBC" empty)) "UBC") true)
(check-expect (contains? (cons "UBC" (cons "McGill" empty)) "Toronto") false)

(define (contains? los s)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) s)
             true
             (contains? (rest los) s))]))



;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list of sqr of every number in lon
(check-expect (squares empty) empty)
(check-expect (squares (list 3 4)) (list 9 16))

;(define (squares lon) empty) ;stub

(define (squares lon) (map2 lon sqr))
;  (cond [(empty? lon) empty]
;        [else
;         (cons (sqr (first lon))
;               (squares (rest lon)))]))

;; ListOfNumber -> ListOfNumber
;; produce list of sqrt of every number in lon
(check-expect (square-roots empty) empty)
(check-expect (square-roots (list 9 16)) (list 3 4))

;(define (square-roots lon) empty) ;stub

(define (square-roots lon) (map2 lon sqrt))
;  (cond [(empty? lon) empty]
;        [else
;         (cons (sqrt (first lon))
;               (square-roots (rest lon)))]))

;; (listof X) (X -> Y) -> (listof Y)
;; interp. given a list and a function
;; produces the list values with the function applied to each element
;; (list 1 2 ...) f -> (list (f 1) (f 2) ...)
(check-expect (map2 empty sqr) empty)
(check-expect (map2 (list 2 4) sqr) (list 4 16))
(check-expect (map2 (list 16 9) sqrt) (list 4 3))
(check-expect (map2 (list 2 -3 4) abs) (list 2 3 4))
(check-expect (map2 (list "a" "aaa" "aa") string-length) (list 1 3 2))

(define (map2 lon fun2)
  (cond [(empty? lon) empty]
        [else
         (cons (fun2 (first lon))
               (map2 (rest lon) fun2))]))


;; ====================

;; ListOfNumber -> ListOfNumber
;; produce list with only positive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

;(define (positive-only lon) empty) ;stub

(define (positive-only lon) (filter2 lon positive?))
;  (cond [(empty? lon) empty]
;        [else
;         (if (positive? (first lon))
;             (cons (first lon)
;                   (positive-only (rest lon)))
;             (positive-only (rest lon)))]))


;; ListOfNumber -> ListOfNumber
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

;(define (negative-only lon) empty) ;stub

(define (negative-only lon) (filter2 lon negative?))
;  (cond [(empty? lon) empty]
;        [else
;         (if (negative? (first lon))
;             (cons (first lon)
;                   (negative-only (rest lon)))
;             (negative-only (rest lon)))]))

(check-expect (filter2 empty negative?) empty)
(check-expect (filter2 (list -2 -1 0 1 2) positive?) (list 1 2))
(check-expect (filter2 (list -2 -1 0 1 2) negative?) (list -2 -1))
; given a list and a function that evaluates to a boolean
; removes all elements that when passed into the function evaluate to false
; (listof X) (X -> Boolean) -> (listof X)
(define (filter2 lon filterFn) 
    (cond [(empty? lon) empty]
        [else
         (if (filterFn (first lon))
             (cons (first lon)
                   (filter2 (rest lon) filterFn))
             (filter2 (rest lon) filterFn))]))