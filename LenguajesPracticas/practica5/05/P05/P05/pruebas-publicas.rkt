#lang racket
(require rackunit "grammars.rkt" "parser.rkt" "desugar.rkt" "interp.rkt")
(define (run e) (interp (desugar (parse e)) (mtSub)))
;; Pruebas de referencia: no sustituyen la revisión del código.
(check-equal? (run 5) (numV 5))
(check-equal? (run '(if #t 8 9)) (numV 8))
(check-equal? (run '(with ((x 4) (y 6)) (+ x y))) (numV 10))
(check-equal? (run '(with* ((x 4) (y (+ x 3))) y)) (numV 7))
(check-equal? (run '((fun (x y) (+ x y)) 2 3)) (numV 5))
(check-equal? (run '(cond ((zero? 1) 10) (else 20))) (numV 20))
;; La implementación recibida requiere completar el manejo de booleanos
;; y comparaciones en interp para superar estas pruebas.
(check-equal? (run '(< 2 3)) (boolV #t))
(check-equal? (run '(not #f)) (boolV #t))
(check-equal? (run '(if (zero? 0) 1 2)) (numV 1))
