;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Abstraction and Lambda|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
;; CS 305 2014.12.13
;; Homework 6

;; 1. Abstract
;; a.
(define (smaller_or_larger-items f lon threshold)
  (cond
    [(empty? lon) empty]
    [else (if (f (first lon) threshold)
              (cons (first lon) (smaller_or_larger-items f (rest lon) threshold))
              (smaller_or_larger-items f (rest lon) threshold))]))
;; b.
(define (quick-sort lon)
  (cond
    [(empty? lon) empty]
    [else (append (quick-sort (smaller_or_larger-items < lon (first lon)))
                  (list (first lon))
                  (quick-sort (smaller_or_larger-items > lon (first lon))))]))
(check-expect (quick-sort (list 11 9 2 18 12 14 4 1)) (list 1 2 4 9 11 12 14 18))

;; 2. Determine output
((lambda(x y)
   (+ (* x y) x))
 1 2)
;; Output: 3
;; (1 * 2) + 1 = 3

((lambda(x y)
   (+ x
      (local ((define x (* y y)))
        (+ (* 3 x) (/ 1 x)))))
 1 2)
;; Output: 13.25
;; 1 + (3 * 2 * 2) + (1/4) = 13.25

((lambda(x y)
   (+ x
      ((lambda(x)
         (+ (* 3 x) (/ 1 x)))
       (* y y))))
 1 2)
;; Output: 13.25
;; 1 + (3 * 2 * 2) + (1/4) = 13.25

;; 3.
(define-struct plane (ID miles-flown miles-since-checked problems? mechanic))

;; a. sevice-priority
;; high -> >100000 flown miles since checked
;; medium -> 100000 > miles flown since checked > 50000 and no problems
;; low -> else
(define (service-priority plane)
  (cond
    [(> (plane-miles-since-checked plane) 100000) "high"]
    [(> (plane-miles-since-checked plane) 50000) "medium"]
    [else "low"]))

;; b. findplanestoservice
(define (findplanestoservice lop priority)
  (filter
   (lambda(x) (string=? (service-priority x) priority))
   lop))
(define p1 (make-plane 1 200000 1 false "Joe"))
(define p2 (make-plane 2 200000 50001 false "Bob"))
(define p3 (make-plane 3 200000 100001 false "Bob"))
(define p4 (make-plane 4 200000 1 false "Joe"))
(define p5 (make-plane 5 200000 1 false "Joe"))
(define planelist (list p1 p2 p3 p4 p5))
(findplanestoservice planelist "low")
;; the output of this is very interesting. I like it.