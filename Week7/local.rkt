;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define p "incendio ")

;; Scope of defines in [] only applies till the end of enclosing local
(local [(define p "accio ") ;;overloads p = incendio (searchs program stack from enclosing scope to global scope)
        (define (fetch n) (string-append p n))]
  (fetch "portkey")
  )

; (local [<definitions>] <expression>)

(define a 1)
(define b 2)

(+ a (local [(define b 3)] (+ a b) )
   b)
;; (+ 1 (+ 1 3) 2)
;; 7

(define b1 1)
(+ b1 (local [(define b1 2)] (* b b)) b1)
;; (+ 1 (* 2 2) 1)
;; steps: 1) rename local variables 2) lift local defn to top level 3) replace body

;; encapsulation -> application of local
