;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname define_struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct pos (x y))

(define P1 (make-pos 3 6)) ;constructors
(define P2 (make-pos 2 8))

(pos-x P1) ; 3 -> access fields 
(pos-y P2) ; 8

(pos?  P1) ; racket implements typing
(pos? "hello") ;probably based on that string to figure it out