#lang plai
(require (file "./grammars.rkt"))

;; desugar : SCFWBAE -> CFWBAE
(define (desugar expr)
  (type-case SCFWBAE expr
    [idS   (i) (id i)]
    [numS  (n) (num n)]
    [boolS (b) (bool b)]

    [iFS (c t e)
         (iF (desugar c) (desugar t) (desugar e))]

    [opS (f args)
         (op f (map desugar args))]

    [condS (cases)
           (desugar-cond cases)]

    ;; with {{x v1} {y v2}} body
    ;; -> (app (fun (x y) (desugar body)) (list (desugar v1) (desugar v2)))
    [withS (bindings body)
           (let ([ids  (map binding-id bindings)]
                 [vals (map (λ (b) (desugar (binding-value b))) bindings)])
             (app (fun ids (desugar body)) vals))]

    ;; with* {{x v1} {y v2}} body
    ;; -> (app (fun (list x) (desugar (with* {{y v2}} body))) (list (desugar v1)))
    [withS* (bindings body)
            (if (null? bindings)
                (desugar body)
                (let* ([b    (first bindings)]
                       [id   (binding-id b)]
                       [val  (desugar (binding-value b))]
                       [rest (rest bindings)])
                  (app (fun (list id)
                            (desugar (withS* rest body)))
                       (list val))))]

    [funS (params body)
          (fun params (desugar body))]

    [appS (f args)
          (app (desugar f) (map desugar args))]
  ))

(define (desugar-cond cases)
  (let loop ([cs cases])
    (cond
      [(null? cs)
       (error 'desugar-cond "cond sin cláusulas (falta 'else' al final)")]
      [else
       (type-case Condition (first cs)
         [else-cond (else-expr)
           (if (null? (rest cs))
               (desugar else-expr) ; última cláusula: se devuelve su cuerpo
               (error 'desugar-cond "'else' debe ser la última cláusula en cond"))]
         [condition (test-expr then-expr)
           (iF (desugar test-expr)
               (desugar then-expr)
               (loop (rest cs)))])])))
