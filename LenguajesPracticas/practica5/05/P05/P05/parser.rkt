#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> SCFWBAE

;; parse: s-expression -> SCFWBAE
(define (parse sexp)
 (match sexp 
 ;; Caso base 
 [(? symbol?) (idS sexp)]     ; Identificador
 [(? number?) (numS sexp)]              ; Número
 [(? boolean?) (boolS sexp)]            ; Booleano

;; Expresión if
[(list 'if cond then else)
     (iFS (parse cond) (parse then) (parse else))]
;; Expresión cond
[(list 'cond cases ...)
     (condS (parse-cond cases))]
;; Expresión with 
[(list 'with (list (list id expr) ...) body)
 (let ([bindings (map (λ (i e) (binding i (parse e))) id expr)]) ;; Tomamos lista
       (if (check-duplicates (map binding-id bindings))
           (error 'parse "identificador duplicado en with: ~a" 
                  (check-duplicates (map binding-id bindings))) ;; Línea
           (withS bindings (parse body))))]
;; Expresión with* (asignación secuencial)

[(list 'with* (list (list id expr) ...) body)
     (withS* (map (λ (i e) (binding i (parse e))) id expr) (parse body))]
;; Expresión fun (definición de función)
[(list 'fun (list params ...) body)
     (if (check-duplicates params)
         (error 'parse "parámetro duplicado en función: ~a" 
                (check-duplicates params))
         (funS params (parse body)))]
 ;; Aplicación de función o operación 
[(list op args ...)
     (if (and (symbol? op) (operator? op))
         (opS (get-op op) (map parse args)) ; Operación primitiva
         (appS (parse op) (map parse args))] ; Aplicación de función
    
    ;; Caso no reconocido
    [else (error 'parse "expresión no válida: ~a" sexp)]))


;; operator?: symbol -> boolean
(define (operator? op)
  (member op '(+ - * / modulo expt add1 sub1 
               < <= = > >= not and or zero?)))

;; get-op: symbol -> procedure
(define (get-op op)
  (match op
    ['+ +] ['- -] ['* *] ['/ /]
    ['modulo modulo] ['expt expt]
    ['add1 add1] ['sub1 sub1]
    ['< <] ['<= <=] ['= =] ['> >] ['>= >=]
    ['not not] 
    ['and (λ (x y) (and x y))] 
    ['or (λ (x y) (or x y))]
    ['zero? zero?]
    [else (error 'parse "operador desconocido: ~a" op)]))





;; parse-cond: listof(s-exp) -> listof(Condition)
(define (parse-cond cases)
  (match cases
  ;; 1. Caso base: lista vacía de condiciones
    [(list) (error 'parse "cond sin casos")]
    ;; 2. Caso especial 'else' como última condición
    [(list (list 'else expr)) (list (else-cond (parse expr)))]
    ;; 3. Caso de condición normal {test-expr then-expr}
    [(list (list test expr) rest ...)
    ;; 3.1 Construye la condición actual 3.2 Procesa recursivamente el resto
     (cons (condition (parse test) (parse expr)) (parse-cond rest))]
     ;; 4. Cualquier otro formato es error
    [else (error 'parse "sintaxis de cond incorrecta")]))
    