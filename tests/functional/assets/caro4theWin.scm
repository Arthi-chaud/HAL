(define (append l1 l2)
  (cond ((eq? l1 '()) l2)
        (#t (cons (car l1) (append (cdr l1) l2)))))

(append '(a b c) '(d e f))