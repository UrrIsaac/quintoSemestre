#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))


;; lookup: symbol DefrdSub -> CFWBAE-Value
;; (define (lookup name ds)
(define (lookup name env)
  (type-case DefrdSub env
    ;; Caso base 
    [mtSub () (error 'lookup "Variable libre: ~a" name)] 
    ;; Asociación encontrada
    [aSub (id val rest)  
         (if (symbol=? id name)
             val  ; 2.1 Encontrado
             (lookup name rest))])) ; 2.2 Sigue buscando
)

;; interp: CFWBAE DefrdSub-> CFWBAE-Value
;; Toma dos argumentos: Una expresión del tipo CFWBAE, ya sin azúcar sintáctica.
;; Un ambiente (DefrdSub) con las variables ya ligadas.
(define (interp expr env)
  (type-case CFWBAE expr 
    [id (i) (lookup i env)]  
    [num (n) (numV n)]  
    [bool (b) (boolV b)]  

    ;; Caso: Condicional 
    [iF (c t e)  
      ;; [EAGER] Se evalúa la condición c INMEDIATAMENTE una sola vez.
      (let ([c-val (interp c env)]) 
        (if (boolV? c-val)  
            (if (boolV-b c-val)  
                (interp t env)  ; Solo se evalúa la rama elegida (normal en ambos enfoques).
                (interp e env))
            (error 'interp "La condición debe ser booleana")))]

    ;; Caso operación
    [op (f args)  
      ;; [EAGER] Aquí se evalúan TODOS los argumentos antes de aplicar f:
      ;; (map (λ (a) (interp a env)) args) fuerza el cálculo de cada arg.
      (let ([arg-vals (map (λ (a) (interp a env)) args)])
        ;; Revisamos que todos sean numéricos (numV?) usando andmap.
        (if (andmap numV? arg-vals)  ; 5.1 Validación tipo
            (numV (apply f (map numV-n arg-vals)))  
            (error 'interp "Operación aplicada a no número")))]

    ;; Caso Función 
    [fun (params body)  
      ;; Se crea una clausura con el ambiente actual (cierre léxico).
      (closure params body env)]  

    ;; Caso Aplicación de función 
    [app (fun-expr arg-exprs)  
      ;; [EAGER] Se evalúa la expresión de la FUNCIÓN y TAMBIÉN TODOS los ARGUMENTOS
      ;; ANTES de hacer la sustitución/extendido del ambiente:
      ;; - fun-val: (interp fun-expr env)
      ;; - arg-vals: (map (λ (a) (interp a env)) arg-exprs)
      (let ([fun-val  (interp fun-expr env)]
            [arg-vals (map (λ (a) (interp a env)) arg-exprs)])
        (if (closure? fun-val)  
            (if (= (length (closure-params fun-val))
                   (length arg-vals))
                (let* ([params  (closure-params fun-val)]
                       [env0    (closure-env fun-val)]
                       ;; [EAGER] Aquí ya se insertan VALORES evaluados (no thunks)
                       ;; al extender el ambiente para la invocación.
                       [new-env (foldr (λ (p v acc) (aSub p v acc))
                                       env0
                                       params
                                       arg-vals)])
                  (interp (closure-body fun-val) new-env))
                (error 'interp "Número incorrecto de argumentos"))
            (error 'interp "Intento de aplicar no-función")))]))

;; Ejemplo: 
;; (app (fun (x) 1) (op / (num 1) (num 0)))
;; Con evaluación glotona, el argumento (/ 1 0) se evalúa ANTES de entrar al cuerpo → ERROR.
;; Con evaluación perezosa, x nunca se usa y el resultado sería 1 sin error.
