;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fs_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;; fs-starter.rkt (type comments and examples)

;; Data definitions:

(define-struct elt (name data subs))
;; Element is (make-elt String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or subs.
;;         If data is 0, then subs is considered to be list of sub elements.
;;         If data is not 0, then subs is ignored.

;; ListOfElement is one of:
;;  - empty
;;  - (cons Element ListOfElement)
;; interp. A list of file system Elements

(define F1 (make-elt "F1" 1 empty))
(define F2 (make-elt "F2" 2 empty))
(define F3 (make-elt "F3" 3 empty))
(define D4 (make-elt "D4" 0 (list F1 F2)))
(define D5 (make-elt "D5" 0 (list F3)))
(define D6 (make-elt "D6" 0 (list D4 D5)))
#;


;new encapsulated function tempate
#; 
(define (fn-for-element e) (local [
                                   ;;compound data
                                   (define (fn-for-element--element e)
                                     (... (elt-name e) ; String
                                          (elt-data e) ; Int
                                          (fn-for-loe (elt-subs e)) ; ListOfElement
                                          ))
                                   (define (fn-for-loe loe)
                                     (cond [(empty? loe) (...)] ;atomic distinct empty
                                           [else
                                            (... (fn-for-element--element (first loe))
                                                 (fn-for-loe (rest loe)))]
                                           ))                                   
                                   ] (fn-for-element--element e)))
;; This write up is causing the lost of another test as well
;; This way has a disadvantage of not having a base case test
;; Do separately to unit test then refactor into something encapsulated

;; Functions:

;; Element -> Number
;; ListOfElement -> Integer
;; sigs of the locals

(check-expect (sum-data F1) 1)
;(check-expect (sum-data empty) 0) -> no longer works since empty is not a element (empty is a ListOfElement)
(check-expect (sum-data D5) 3)
(check-expect (sum-data D4) (+ 1 2))
(check-expect (sum-data D6) (+ 1 2 3))

(define (sum-data e)
  (local [(define (sum-data--element e)
            (if (zero? (elt-data e)) 
                (sum-data--loe (elt-subs e)) 
                (elt-data e)
                ))
          (define (sum-data--loe loe)
            (cond [(empty? loe) 0]
                  [else
                   (+ (sum-data--element (first loe))
                      (sum-data--loe (rest loe)))]
                  ))] 
    (sum-data--element e)))
;; Encapsulated!
;; Element->Integer
;; produce the sum of all the data in element (and its subs)

;; Functions:
;; Element -> ListOfString
;; ListOfElement -> ListOfString
;; interp. consumes an element and produces a list of the names of all elements in the tree


#;
;(check-expect (all-names--element F1) (list "F1"))
;(check-expect (all-names--loe empty) empty) ;; equivalent (list)
;(check-expect (all-names--element D5) (list "F3"))
;(check-expect (all-names--element D4) (list "F1" "F2"))
;(check-expect (all-names--element D6) (list "F1" "F2" "F3"))
;;This implementation only implements all files -> should include directories
#;
(define (all-names--element e)
  (if (zero? (elt-data e)) 
      (all-names--loe (elt-subs e)) ; ListOfElement
      (list (elt-name e)) ; String
      ))
#;
(define (all-names--loe loe)
  (cond [(empty? loe) empty] ;atomic distinct empty
        [else
         (append (all-names--element (first loe))
                 (all-names--loe (rest loe)))]
        ))
;;

;;implementation including Directories
(check-expect (all-names F1) (list "F1"))
;(check-expect (all-names--loe empty) empty) -> local fun. now
(check-expect (all-names D5) (list "D5" "F3"))
(check-expect (all-names D4) (list "D4" "F1" "F2"))
;(check-expect (all-names--loe (list D4 D5)) (append (list "D4" "F1" "F2") (list "D5" "F3")) ) -> local fun. now
(check-expect (all-names D6) (cons "D6" (append (list "D4" "F1" "F2") (list "D5" "F3") )))
(check-expect (all-names D6) (list "D6" "D4" "F1" "F2" "D5" "F3") )
;; drops the test count by 1 but all of them seem to pass 
;; seems to be messed up by my faulty implementation that is commented out

(define (all-names e) (local [
                              (define (all-names--element e)
                                (if (zero? (elt-data e)) 
                                    (cons (elt-name e) (all-names--loe (elt-subs e))) ; ListOfElement
                                    (list (elt-name e)) ; String
                                    ))
                              (define (all-names--loe loe)
                                (cond [(empty? loe) empty] ;atomic distinct empty
                                      [else
                                       (append (all-names--element (first loe))
                                               (all-names--loe (rest loe)))]
                                      ))                              
                              ] (all-names--element e)))
; encapsulated!
; Element -> ListOfString
; interp. consumes an element and produces a list of the names of all elements in the tree



;; 
;; String Element -> Integer or false
;; String ListOfElement -> Integer or false ????
;; Takes in a string and root element, searches root and children if element with name string is present
;; returns element data with name string if present and false otherwise
;(check-expect (find--loe  "F3" empty) false)
(check-expect (find "F3" F1) false)
(check-expect (find "F3" F3) 3)
(check-expect (find "D4" D4) 0)
(check-expect (find "D6" D6) 0)
;(check-expect (find--loe "F2" (cons F1 (cons F2 empty))) 2)
;(check-expect (find--loe "F3" (cons F1 (cons F2 empty))) false)
(check-expect (find "F3" D4) false)
(check-expect (find "F1" D4) 1)
(check-expect (find "F2" D4) 2)
(check-expect (find "F3" D6) 3)
(check-expect (find "F1" D6) 1)

(define (find n e) (local [
                           (define (find--elem n e)
                             (if (string=? n (elt-name e)) 
                                 (elt-data e)
                                 (find--loe n (elt-subs e))
                                 ))
                           
                           (define (find--loe n loe)
                             (cond [(empty? loe) false]
                                   [else 
                                    (local [(define try (find--elem n (first loe)))]
                                      (if (not (false? try)) ;; is it found in first loe?
                                          try
                                          (find--loe n (rest loe))) )] ;; produce integer or false depending on if n is found in this nodes children
                                   ))
                           ] ( find--elem n e )))
;Encapsulated
;; String Element -> Integer or false
;; Takes in a string and root element, searches root and children if element with name string is present
;; returns element data with name string if present and false otherwise

;; Natural -> Element 
;; produce a skinny tree n+1 deep, leaf has name "Y" data 1
(check-expect (make-skinny 0) (make-elt "Y" 1 empty))
(check-expect (make-skinny 2) (make-elt "X" 0 (list (make-elt "X" 0 (list (make-elt "Y" 1 empty))))) )

(define (make-skinny n)
  (cond [(zero? n) (make-elt "Y" 1 empty)]
        [else
         (make-elt "X" 0 (list (make-skinny (sub1 n))))]))

;; times execution of procedure
;; exponential execution time before optimization
;; drops to almost 0 after optimization by caching find result
(time (find "Y" (make-skinny 10)))
(time (find "Y" (make-skinny 11)))
(time (find "Y" (make-skinny 12)))
(time (find "Y" (make-skinny 13)))
(time (find "Y" (make-skinny 14)))
(time (find "Y" (make-skinny 15)))
(time (find "Y" (make-skinny 16)))
(time (find "Y" (make-skinny 17)))