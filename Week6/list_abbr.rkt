;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname list_abbr) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(cons "a" (cons "b" (cons "c" empty)))
(list "a" "b" "c") ; equivalent
(list (+ 1 2) (* 3 4))

(define L1 (list "b" "c"))
(define L2 (list "d" "e" "f"))
(cons "a" L1)
(list "a" L1)

(append L1 L2) ;  makes one list out of the 2 with the elements of both