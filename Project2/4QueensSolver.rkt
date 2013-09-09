;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 4QueensSolver) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require racket/list) ;gets list-ref, take and drop

;; Brute force 4x4 4 Queens Solver

;; =================
;; Data definitions:

;; Board is a (listof Boolean) that is 16 elements long
;; interp.
;; A board is a 4 x 4 array of squares, where each square has a row and column number (r,c).
;; It is represented as a flat list, in which the rows hare layed out one after another in
;; a linear fashion. (See interp. of Pos below for how we convert back and forth between (r,c)
;; and position in a board.)

;; Pos is Natural[0, 15]
;; interp.
;; the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 4)
;;    - the column is (remainder p 4)

;; Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 4) c))  ;helpful for writing tests

;; =================
;; Constants:

(define Q 1) ;Q stands for Queen
(define E 0) ;E is for examined/empty
(define B false) ;B stands for blank

(define ALL-VALS (list Q E))

(define TOTAL_QUEENS 4)

;blank
(define BD1 
  (list B B B B
        B B B B
        B B B B
        B B B B))

;solution
(define BD2
  (list B Q B B
        B B B B
        Q B B B
        B B Q B))

(define BD2s
  (list E Q E E
        E E E Q
        Q E E E
        E E Q E))

;no soluntion
(define BD3
  (list Q B B B
        B B B B
        B B B Q
        B Q B B))

;no solution?
(define BD4
  (list Q B B B
        B B B B
        B B B B
        B B B B))

(define BD5
  (list B B Q B
        Q B B B
        B B B Q
        B B B B))

(define BD5s
  (list E E Q E
        Q E E E
        E E E Q
        E Q E E))

(define BD6
  (list B B Q B
        Q B B B
        B B B B
        B B B B))

;invalid boards
(define I1
  (list Q Q B B
        B B B B
        B B B B
        B B B B))

(define I2
  (list Q B B B
        Q B B B
        B B B B
        B B B B))

(define I3
  (list Q B B B
        B B B B
        B B B B
        B B B Q))

(define I4
  (list B B B Q
        B B B B
        B B B B
        Q B B B))

;; Positions of all rows, columns, and diagonals
(define ROWS
  (list (list 0 1 2 3)
        (list 4 5 6 7)
        (list 8 9 10 11)
        (list 12 13 14 15)))

(define COLS
  (list (list 0 4 8 12)
        (list 1 5 9 13)
        (list 2 6 10 14)
        (list 3 7 11 15)))

; upward slope diagonals
(define DIAGU
  (list (list 1 4)
        (list 2 5 8)
        (list 3 6 9 12)
        (list 7 10 13)
        (list 11 14)))

; downward slope diagonals
(define DIAGD
  (list (list 2 7)
        (list 1 6 11)
        (list 0 5 10 15)
        (list 4 9 14)
        (list 8 13)))

(define UNITS (append ROWS COLS DIAGU DIAGD))

;; =================
;; Functions:

;; Board -> Board or false
;; produce a solution for bd; or false if unsolvable
;; Assume: bd is valid
(check-expect (queens BD2s) BD2s)
(check-expect (queens BD5s) BD5s)
(check-expect (queens BD2) BD2s)
(check-expect (queens BD5) BD5s)
(check-expect (queens BD3) false)
(check-expect (queens BD1) BD2s) 

(define (queens bd)
  (local [(define (solve--bd bd)
            (if (solved? bd)
                (map standardize-val bd)
                (solve--lobd (next-boards bd))))
          
          (define (solve--lobd lobd)
            (cond [(empty? lobd) false]
                  [else 
                   (local [(define try (solve--bd (first lobd)))] ; try first sub-board
                     (if (not (false? try)) 
                         try
                         (solve--lobd (rest lobd))))]))
          ]
    (solve--bd bd)
    ))

;; Val -> Natural[0,1]
;; standardizes the value to make a fully numeric board
;; Numeric values will be left as is, any Booleans will be changed into a 0
(check-expect (standardize-val false) 0)
(check-expect (standardize-val 0) 0)
(check-expect (standardize-val 1) 1)
(define (standardize-val val)
  (if (boolean? val)
      0
      (if (number? val)
          val
          (error "The board had a undefined value."))))

;; Board -> Boolean
;; produce true if board is solved
;; Assume: board is valid so only need to count number of queens
(check-expect (solved? BD1) false)
(check-expect (solved? BD2) false)
(check-expect (solved? BD2s) true)

(define (solved? bd) 
  (= TOTAL_QUEENS (number-of-queens bd)))

;; Board -> Natural
;; produce the number of queens currently on the board
(define (number-of-queens bd)
 (foldr + 0 (map queens-at-square? bd)))

;; Boolean -> Natural[0,1]
;; produces 1 if a queen is present (true) or 0 otherwise
(check-expect (queens-at-square? Q) 1)
(check-expect (queens-at-square? E) 0)
(check-expect (queens-at-square? B) 0)
(define (queens-at-square? bd-pos) 
  (if (false? bd-pos)
      0
      bd-pos))

;; Board -> (listof Board)
;; produce list of valid next boards from board
;; finds first un attacked square and fills it with a queen
(check-expect (next-boards (cons Q (rest BD1))) 
              (list (cons Q (cons E (rest (rest BD1))))
                    ))
(define (next-boards bd)
  (keep-only-valid (fill-with-Q-E (find-blank bd) bd)))

;; Board -> Pos
;; produces the position of the first blank square
;; Assume: board is not full/solved (at least 1 blank square)
(check-expect (find-blank BD1) 0)
(check-expect (find-blank (cons Q (rest BD1))) 1)
(check-expect (find-blank (cons Q (cons E (rest (rest BD1))))) 2)
(define (find-blank bd) 
  (cond [(empty? bd) (error "The board didn't have a blank space.")]
        [else 
         (if (false? (first bd))
             0
             (+ 1 (find-blank (rest bd))))]
        ))

;; Pos Board -> (listof Board)
;; Produces 2 boardhs with blank filled with Q(1) or E(0)
(check-expect (fill-with-Q-E 1 (cons Q (rest BD1))) 
              (list (cons Q (cons Q (rest (rest BD1)))) 
                    (cons Q (cons E (rest (rest BD1))))
                    ))
(define (fill-with-Q-E pos bd) 
  (list (fill-square bd pos Q) (fill-square bd pos E)))

;; (listof Board) -> (listof Board)
;; produceh list containing only valid boards
(check-expect (keep-only-valid (list I1 I2 I3 I4)) empty)
(check-expect (keep-only-valid (list BD1 BD2 BD3 BD4)) (list BD1 BD2 BD3 BD4))
(define (keep-only-valid lobd) 
  (filter valid-board? lobd))

;; Board -> Boolean
;; produce true if no unit on the board has the Queen twice; false otherwise
(check-expect (valid-board? BD1) true)
(check-expect (valid-board? BD2) true)
(check-expect (valid-board? BD5s) true)
(check-expect (valid-board? BD3) true)
(check-expect (valid-board? BD4) true)
(check-expect (valid-board? BD5) true)
(check-expect (valid-board? BD5s) true)
(check-expect (valid-board? BD6) true)
(check-expect (valid-board? I1) false)
(check-expect (valid-board? I2) false)
(check-expect (valid-board? I3) false)
(check-expect (valid-board? I4) false)
(define (valid-board? bd) 
  (local [(define (valid-units? lou) 
            (andmap valid-unit? lou)) ; (listof Unit) -> Boolean 
          
          (define (valid-unit? u)  
            (one-queen-only? (keep-only-values (read-unit u)))) ; Unit -> Boolean
          
          (define (read-unit u) 
            (map read-pos u)) ; Unit -> (listof Val|false)
          
          (define (read-pos pos) 
            (read-square bd pos));  Pos -> Val|false
          
          (define (keep-only-values lovf) 
            (filter number? lovf)) ; (listof Val|false) -> (listof Val)
          
          ; (listof Val) -> Boolean
          (define (one-queen-only? lov) 
            (>= 1 (foldr + 0 lov)))
          ] (if (has-unexamined-spots? bd) 
                (valid-units? UNITS) ;if there are unexamined spaces, just make sure that the units are valid 
                (and (valid-units? UNITS) (solved? bd))))) ;filter out fully examined boards with not enough queens

; Board -> Boolean
; returns true if there are unexamined spots and false otherwise
(check-expect (has-unexamined-spots? BD2s) false)
(check-expect (has-unexamined-spots? BD2) true)
(define (has-unexamined-spots? bd) 
  (ormap boolean? bd))

;; =================
;; Utility functions from sudoku-starter.rkt:

;; Board Pos -> Val or false
;; Produce value at given position on board.
(check-expect (read-square I1 (r-c->pos 0 1)) 1)
(check-expect (read-square I2 (r-c->pos 1 0)) 1)

(define (read-square bd p)
  (list-ref bd p))            


;; Board Pos Val -> Board
;; produce new board with val at given position
(check-expect (fill-square BD1 (r-c->pos 0 0) 1)
              (cons 1 (rest BD1)))

(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))