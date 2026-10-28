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
;; operaciones :  (listof (list symbol procedure aridad))
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
;; operaciones? : any -> boolean, pero tendría que ser un simbolo lo que se reciba
(define (operaciones? simb)
  (and (symbol? simb) (assq simb operaciones) ))

; Para ver si es un with o un with*
;; symbol -> boolean
(define (es-with? simb)
  (memq simb '(with with*)))

; Parsea las operaciones que podemos hacer 
;;  parse-op : s-expression -> FWAE
(define (parse-op sexp)
  (define op-sym (first sexp)) ; simbolo de la operación
  (define args (rest sexp)) ; argumentos de la operación
  (define n-args (length args)) ; número de argumentos de la operación
  (define definicion (assq op-sym operaciones))  ; para ver si está definidia la operación q quiere hacer
  (define proceso (second definicion)) ; la operación q se debe hacer
  (define aridad (third definicion)) ; aridad de la operación
  (if (case aridad ;checa por casos que las aridades concuerden con el número de argumentos
        [(uno) (= n-args 1) ]
        [(dos) (= n-args 2) ]
        [(multi) (>= n-args 2) ]
        [else #f ]
        )
      (op proceso (map parse args)) ; si concuerda todo es que se puede hacer el proceso, pero se deben parsear los argumentos.
      (error 'parse "Syntax Error : expresion mal formada en parse")
      )
  )

; para parsear el with
;; parse-with : s-expression procedure boolean -> FWAE
(define (parse-with sexp constructor tipo)
  (if (and (= (length sexp) 3) (list? (second sexp)) (>= (length (second sexp)) 1) ) ; Si la expresión tiene 3 elementos, su segundo es una lista y ese segundo no es lista vacía
      (let* ([bindings-sexp (second sexp)] ; definimos con let* los bindings
             [body-sexp (third sexp)] ; el body del with
             [bindings (map parse-binding bindings-sexp)] ; parseamos los bindings de la expresión
             [ids (map binding-id bindings)]) ; accessor del campo id
        (if (and tipo (tiene-duplicados? ids)) ; si es with habiamos mandado #t en tipo, por lo que si tiene duplicados no podemos; si es with habiamos mandado #t en tipo, por lo que si tiene duplicados no podemos
            (error 'parse "Syntax Error : expresion mal formada en parse")
            (constructor bindings (parse body-sexp)) ; si es with* habiamos mandado #f en tipo, podemos actuar con duplicados.
            )
        )
      (error 'parse "Syntax Error : expresion mal formada en parse")
      )
  )

; el binding debe ver si es una lista de longitud 2, si el primero es símbolo y no reservado podemos hacer el binding
;;parse-binding : s-expression -> binding
(define (parse-binding b)
  (if(and (list? b) (= (length b) 2) (symbol? (first b)) (not (es-with? (first b))) )
     (binding (first b) (parse (second b)))
     (error 'parse "Syntax Error : expresion mal formada en parse")
     )
  )

; función auxiliar para ver si una lista tiene duplicados
;; tiene-duplicados? : list -> boolean
(define (tiene-duplicados? lista)
  (not (= (length lista) (length (remove-duplicates lista))))
  )