#lang plai

#|
PRÁCTICA 03 - parser
ALUMNOS :
   Andrade Castañeda Angel
   Urrutia Alfaro Isaac Arturo
|#


;Estructuras necesarias :

;; Definicion del tipo Binding
; identificador → expresión, es lo q tenemos en un (with (x 5) (+ 1 x)), el binding es el x -> 5
( define-type Binding
   [ binding ( id symbol?)
             ( value WAE+?) ])

;; Definicion del tipo FWAE
( define-type WAE+
   [ id ( i symbol?) ]
   [ num ( n number?) ]
   [ bool ( b boolean?) ]
   [ op ( f procedure?)
        ( args ( listof WAE+?) ) ]
   [ with ( bindings ( listof binding?) )
          ( body WAE+?) ]
   [ with* ( bindings ( listof binding?) )
          ( body WAE+?) ])


; Como conocemos la gramática y las aridades de las operaciones tenemos que tenerlas en algún sitio :
;; operaciones : symbol -> (procesimiento aridad)
(define operaciones
  (list (list '+ + 'multi)
        (list '- - 'multi)
        (list '* * 'multi)
        (list '/ / 'multi)
        (list 'add1 add1 'uno)
        (list 'sub1 sub1 'uno)
        (list 'not not 'uno)
        (list 'modulo modulo 'dos)
        (list 'expt expt 'dos)
        (list '= = 'dos)
        (list '> > 'dos)
        (list '< < 'dos)
        (list '<= <= 'dos)
        (list '>= >= 'dos)
   )
  )



; Nuestra definición más importante
;; parse : s-expression - > FWAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(boolean? sexp) (bool sexp)]
    [(symbol? sexp)
     (if(es-with? sexp)
        (error 'parse "Syntax Error : expresion mal formada en parse")
        (id sexp))]
    [(list? sexp)
     (cond
       [(eq? (first sexp) 'with) (parse-with sexp with #t)] ; true para ver que es with
       [(eq? (first sexp) 'with*) (parse-with sexp with* #f)] ; false para ver que es with*
       [(operaciones? (first sexp)) (parse-op sexp)]
       [else (error 'parse "Syntax Error : expresion mal formada en parse") ]
       )]
    [else (error 'parse "Syntax Error : expresion mal formada en parse") ]
    )
  )

; Para buscar si un símbolo está en nuestra lista
;; symbol -> boolean
(define (operaciones? simb)
  (and (symbol? simb) (assq simb operaciones) ))

; Para ver si es un with o un with*
;; symbol -> boolean
(define (es-with? simb)
  (memq simb '(with with*)))

;;
(define (parse-op sexp)
  (define op-sym (first sexp))
  (define args (rest sexp))
  (define n-args (length args))
  (define definicion (assq op-sym operaciones))
  (define proceso (second definicion))
  (define aridad (third definicion))
  (if (case aridad
        [(uno) (= n-args 1) ]
        [(dos) (= n-args 2) ]
        [(multi) (>= n-args 2) ]
        [else #f ]
        )
      (op proceso (map parse args))
      (error 'parse "Syntax Error : expresion mal formada en parse")
      )
  )

;;
(define (parse-with sexp constructor tipo)
  (if (and (= (length sexp) 3) (list? (second sexp)) (>= (length (second sexp)) 1) )
      (let* ([bindings-sexp (second sexp)]
             [body-sexp (third sexp)]
             [bindings (map parse-binding bindings-sexp)]
             [ids (map binding-id bindings)]) ; accessor del campo id
        (if (and tipo (tiene-duplicados? ids))
            (error 'parse "Syntax Error : expresion mal formada en parse")
            (constructor bindings (parse body-sexp))
            )
        )
      (error 'parse "Syntax Error : expresion mal formada en parse")
      )
  )

(define (parse-binding b)
  (if(and (list? b) (= (length b) 2) (symbol? (first b)) (not (es-with? (first b))) )
     (binding (first b) (parse (second b)))
     (error 'parse "Syntax Error : expresion mal formada en parse")
     )
  )

(define (tiene-duplicados? lista)
  (not (= (length lista) (length (remove-duplicates lista))))
  )



; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
; un parse-binding de b, debe ver si es una lista, su longitud es dos, el primero es un símbolo y no es reservados (with ((with x)) (+ x 5)) no
; si cumple entonces hacemos binding del primero con el parseo del segundo
; si no cumple es un error



; el has-duplicados? es de una lista, es el not de si es igual la longitud de la lista con el remove-duplicates de la lista


; a ver, tengo mi lista de operaciones, tengo mi gramática y mi binding
; empecé a parsear, hice casos sencillos
;  si es with entonces parseo el with
;  si es with* entonces parseo el with*
;      en ambos casos, tienen que ser long 3 (with ((id exp) (id exp) + ) (body)), el seguno debe de ser lista y la longitus del segundo debe ser mayor o igual q 1
;      si sí, let* definidmos el bindings-sexp (segundo), el body (tercero), bndings (map del parse-binging) los ids (map de binding-id de bindings), entonces
;      checho, si es #t de with normal NO puedo tener duplicados (un igual de si tiene duplicados con si es #t), si no tiene entonces constructor bindings del parseo del body-sexp
; si truena todo un error



;                      (define b (binding 'x (num 5)))
;                      (binding-id b)     ; => 'x



;  si es operación entonces parseo la operación, debo checar aridad (uno, dos multi, si no #f) si sí un (op proc (map parse arg-sexps)), si no error.

; un parse-binding de b, debe ver si es una lista, su longitud es dos, el primero es un símbolo y no es reservados (with ((with x)) (+ x 5)) no
; si cumple entonces hacemos binding del primero con el parseo del segundo
; si no cumple es un error



; el has-duplicados? es de una lista, es el not de si es igual la longitud de la lista con el remove-duplicates de la lista
