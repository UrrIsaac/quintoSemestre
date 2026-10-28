#lang plai

#|
PRÁCTICA 03 - subst / interp
ALUMNOS :
   Andrade Castañeda Angel
   Urrutia Alfaro Isaac Arturo
|#

(require "parser.rkt")

; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
; SUBST
; 
; Ideas generales
;   - id: si es el mismo símbolo que sub-id, se cambia por val; si no, igual.
;   - num/bool: se regresan tal cual, no tienen variables.
;   - op: se sustituye en cada uno de sus argumentos.
;   - with: los VALUES de los bindings siempre están "afuera" del with, así que
;           siempre se sustituyen. Los IDs que declara el with son los que se
;           pueden sombrear: si sub-id aparece entre esos ids, el body ya no se toca.
;   - with*: aquí sí importa el orden. Vamos de izquierda a derecha sustituyendo
;            en cada value; en cuanto un binding declara el mismo id que sub-id,
;            ese id sombrea a partir de ahí (los bindings siguientes y el body
;            ya no se sustituyen).

;; subst : WAE+ symbol WAE+ -> WAE+
(define (subst expr sub-id val)
  (type-case WAE+ expr
    [id (i) (if (symbol=? i sub-id) val expr)]
    [num (n) expr]
    [bool (b) expr]
    [op (f args) (op f (map (lambda (a) (subst a sub-id val)) args))]
    [with (bindings body)
          (with (subst-bindings-with bindings sub-id val)
                (if (memq sub-id (map binding-id bindings))
                    body ; sombreado, el body no se toca
                    (subst body sub-id val)))]
    [with* (bindings body)
           (subst-with-asterisco bindings body sub-id val)]))

;FUNCION AUXILIAR DE WITH
; en with, los valores siempre quedan fuera del alcance de los ids que se están
; declarando, así que siempre se les aplica subst; los ids en sí no se tocan

;; subst-bindings-with : (listof binding) symbol WAE+ -> (listof binding)
(define (subst-bindings-with bindings sub-id val)
  (map (lambda (b) (binding (binding-id b) (subst (binding-value b) sub-id val)))
       bindings))

;FUNCIONES AXILIARES EN WITH*
; en with* recorremos los bindings de izquierda a derecha sustituyendo en cada
; value. En cuanto encontramos un binding cuyo id == sub-id, ese binding sombrea
; a sub-id para todo lo que sigue (los bindings restantes y el body se dejan
; intactos respecto a sub-id).

;; subst-with-asterisco : (listof binding) WAE+ symbol WAE+ -> WAE+
(define (subst-with-asterisco bindings body sub-id val)
  (define-values (nuevos-bindings sombreado?) (subst-bindings-with-asterisco bindings sub-id val))
  (with* nuevos-bindings (if sombreado? body (subst body sub-id val))))

;; subst-bindings-with-asterisco : (listof binding) symbol WAE+ -> (values (listof binding) boolean)
(define (subst-bindings-with-asterisco bindings sub-id val)
  (cond
    [(empty? bindings) (values empty #f)]
    [else
     (define b (first bindings))
     (define nuevo-b (binding (binding-id b) (subst (binding-value b) sub-id val)))
     (if (symbol=? (binding-id b) sub-id)
         (values (cons nuevo-b (rest bindings)) #t)   ; sombreado desde aquí: el resto no se toca
         (let-values ([(resto sombreado?) (subst-bindings-with-asterisco (rest bindings) sub-id val)])
           (values (cons nuevo-b resto) sombreado?)))]))


; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
; INTERP
;
; Estrategia glotona: primero se evalúa la expresión del binding (con interp,
; obteniendo un número o booleano de racket), luego ese valor se convierte de
; vuelta a WAE+ (num/bool) y se sustituye con subst.


;; interp : WAE+ -> (or/c number? boolean?)
(define (interp expr)
  (type-case WAE+ expr
    [id (i) (error 'interp "Variable libre: ~a" i)]
    [num (n) n]
    [bool (b) b]
    [op (f args) (apply f (map interp args))]
    [with (bindings body) (interp (interp-with bindings body))]
    [with* (bindings body) (interp (interp-with-asterisco bindings body))]))

;FUNCIONES AUXILIARES DE INTERP

; convierte un valor racket ya evaluado (number o boolean) de vuelta a un nodo WAE+
;; valor->WAE+ : (or/c number? boolean?) -> WAE+
(define (valor->WAE+ val)
  (if (boolean? val) (bool val) (num val)))

; with: bindings simultáneos. Cada value se evalúa siempre sobre el binding
; original (contexto exterior, nunca sobre lo ya sustituido) y su resultado se
; va sustituyendo directo en el body acumulado.
;; interp-with : (listof binding) WAE+ -> WAE+
(define (interp-with bindings body)
  (if (empty? bindings)
      body
      (let* ([b (first bindings)]
             [val (interp (binding-value b))])
        (interp-with (rest bindings) (subst body (binding-id b) (valor->WAE+ val))))))

; with*: bindings secuenciales. Evaluamos el primer binding y reusamos subst
; sobre un with* armado con (resto de bindings + body): así el sombreado, si
; algún binding posterior repite el id, lo resuelve subst mismo (ver arriba).
;; interp-with-asterisco : (listof binding) WAE+ -> WAE+
(define (interp-with-asterisco bindings body)
  (if (empty? bindings)
      body
      (let* ([b (first bindings)]
             [val (interp (binding-value b))]
             [resto (with* (rest bindings) body)]
             [sustituido (subst resto (binding-id b) (valor->WAE+ val))])
        (interp-with-asterisco-body sustituido))))

; si lo que quedó tras sustituir sigue siendo un with* (aún quedan bindings),
; seguimos "pelando" bindings; si no, ya es puro body listo para interp
;; interp-with-asterisco-body : WAE+ -> WAE+
(define (interp-with-asterisco-body sustituido)
  (type-case WAE+ sustituido
    [with* (bindings2 body2) (interp-with-asterisco bindings2 body2)]
    [else sustituido]))
