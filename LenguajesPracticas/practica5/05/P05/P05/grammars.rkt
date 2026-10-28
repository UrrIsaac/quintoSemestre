#lang plai

; Definición del tipo Binding
;; Binding: Asociación de un identificador con una expresión
(define-type Binding
  [binding (id symbol?) (value SCFWBAE?)])
;;Ejemplo: {with {{x 5} {y 10}} {+ x y}}
;; (binding 'x  (numS 5))
;; (binding 'y (numS 10))


;;Definición del tipo condition para la definición de cond.
;; es una estructura condicional múltiple que funciona como una generalización de if.
;; Un cond consiste en:
;; Una serie de cláusulas {test-expr then-expr}
;; Una cláusula else opcional al final
(define-type Condition
  [condition (test-expr SCFWBAE?) (then-expr SCFWBAE?)]
  [else-cond (else-expr SCFWBAE?)])
;; Ejemplo:
;;{cond 
;;     
;;      {else "cero"}}
;; { cond { < expr > < expr >+} { else < expr >}}
;; {{(> x 0) "positivo"}  {(< x 0) "negativo"} +}

;; Definición del tipo SCFWBAE
;; SCFWBAE: Sintaxis concreta (con azúcar sintáctica)
(define-type SCFWBAE
  [idS    (i symbol?)]
  [numS   (n number?)]
  [boolS  (b boolean?)]
  [iFS    (cond SCFWBAE?) (then SCFWBAE?) (else SCFWBAE?)]
  [opS    (f procedure?) (args (listof SCFWBAE?))]
  [condS  (cases (listof Condition?))]
  [withS  (bindings (listof binding?)) (body SCFWBAE?)]
  [withS* (bindings (listof binding?)) (body SCFWBAE?)]
  [funS   (params (listof symbol?)) (body SCFWBAE?)]
  [appS   (fun SCFWBAE?) (args (listof SCFWBAE?))])

;; Definición del tipo CFWBAE
;; CFWBAE: Sintaxis abstracta (sin azúcar sintáctica)
(define-type CFWBAE
  [id   (i symbol?)]
  [num  (n number?)]
  [bool (b boolean?)]
  [iF   (condicion CFWBAE?) (then CFWBAE?) (else CFWBAE?)]
  [op   (f procedure?) (args (listof CFWBAE?))]
  [fun  (params (listof symbol?)) (body CFWBAE?)]
  [app  (fun CFWBAE?) (args (listof CFWBAE?))])


  (define-type DefrdSub
  [mtSub]
  [aSub  (name symbol?) (value CFWBAE-Value?) (ds DefrdSub?)])

  ;;Ejemplo visual: {with {{x 1}} {with {{x 2}} x}}, el ambiente evoluciona así:
;; mtSub 
;; (aSub 'x (num 1) mtSub)
;; (aSub 'x (num 2)  (aSub 'x (num 1) mtSub) )
;; Error interp

;;Data-type que representa la sintaxis abstracta de CFWBAE-Value
;; CFWBAE-Value: Valores producidos por el intérprete
;;param: Parámetros formales
;;body: Cuerpo de la función
;;env: Ambiente en el momento de la definición (clave para static scoping)

;;Permite que las funciones "recuerden" las variables disponibles cuando fueron creadas
;;Diferencia este intérprete (static scope) de uno con dynamic scope

(define-type CFWBAE-Value
  [closure  (param (listof symbol?)) (body CFWBAE?) (env DefrdSub?)]
  [numV     (n number?)]
  [boolV    (b boolean?)])

;; Ejemplo:
;;with {{x 10}
;;       {f {fun {y} {+ x y}}}}
;;  {with {{x 20}}
;;    {f 5}}}  

