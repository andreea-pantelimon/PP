#lang racket/gui
 (define (f1 A B C)
   (let ((A B ))
     (let ((B C))
       (let ((C A))
         (foldr + 0 C)))))
(define (f2 A B C)
  (let* ((A B) (B C) (C A))
    (foldr + 0 C)))
(f1 '(1) '(2) '(3))
(f2 '(1) '(2) '(3))