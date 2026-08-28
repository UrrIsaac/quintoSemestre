#lang plai
#|
PRÁCTICA 01
ALUMNOS :
   Andrade Castañeda Angel
   Urrutia Alfaro Isaac Arturo
|#

; EJERCICIO 4
;; anagrama-profundo ? : string string - > boolean
; ejemplo :
; ( anagrama-profundo? " abcde " " xyzde ")   #f

; NOTAS :
; investigando un poco habia una forma más bonita de pasar el string a lista, con un string->list, pero como no lo vimos, no lo ocupamos
; Para la primera idea fue acomodar todos los caracteres y ver si eran iguales las cadenas, pero era complicado sin ocupar algo que no habiamos visto.
( define ( anagrama-profundo? s1 s2 )
   (if (= (string-length s1) (string-length s2) ) ; si no tienen la misma longitud, sabemos luego luego que no será anagrama
       (string-a-lista s1 s2 '() 0) ; si la longitud es igual, convertiremos la s1 en lista para manipular más sencillo todo, usaremos '() y 0
       #f
       )
    )

(define (string-a-lista s1 s2 lista i) ; usamos el '() para iniciar la lista que será s1 y 0 para ir calculando el caracter de s1 para convertir en elemento de la lista
  (cond
    [(= (string-length s1) (length lista)) (verifica-anagrama? lista s2 0)] ; si la longitud es igual es que ya convertimos todo s1 a lista, entonces pasamos a verificar
    [else (string-a-lista s1 s2 (append lista (list (string-ref s1 i))) (+ i 1))]
    #| si la longitud aún no es igual, nos faltan elementos,así que hacemos recursión,
llamando s1 y s2 iguales (cargandolas), añadiremos a lista el siguiente elemento de s1 que toca y
añadiremos uno al contador para pasar al siguiente caracter|#
    )
  )

(define (verifica-anagrama? lista s2 i) ; usamos i, como contador para ver qué elemento de s2 debemos ver en s1
  (cond
    [(= i (string-length s2)) (empty? lista)]
    ; si las longitudes son iguales terminamos los elementos, si la lista está vacía es que son anagramas, si la lista no es vacía es que no tienen los mismos caracteres
    [else 
     (verifica-anagrama? (remove (string-ref s2 i) lista ) s2 (+ i 1)) ]
    ; llamada recursiva, remove del elemento de s2 a la lista (s1), s2 y el contador para el siguiente caracter. Si el remove no encuentra coincidencia, no quita nada.
    )
  )


; EJERCICIO 5.
;; escalera ? : ( Listof Number ) - > Boolean
; ejemplo : 
; ( escalera? '(-2 -1 0 1) ) #t

( define ( escalera? lst )
   (cond
     [(empty? lst) #t] ; caso base 1 con lista vacía, está ordenada
     [(empty? (rest lst)) #t] ; caso base 2 con un elmento, está ordenada
     [else
      (and (= (first (rest lst)) (+ (first lst) 1) ) (escalera? (rest lst)) )
      ])) ; como todos deben de ser sucesores del anterior, es el and, comparo que el segundo elemento sea igual al primero+1, y comparo el siguiente par


; EJERCICIO 6.
;; ultimo-caracter : String - > Char
; ejemplo : 
; ( ultimo-caracter " Racket") #\t

( define ( ultimo-caracter cadena )
   (string-ref cadena (- (string-length cadena) 1))) ; solo tomo la longitud de la cadena y le resto uno para acceder al último elemento.


; EJERCICIO 7.
;; intercalar : ( listof Any) ( listof Any) - > ( listof Any)
; ejemplo : 
; ( intercalar ’(1 2 3) ’( a b c ) )
( define ( intercalar a b )
   (cond
     [(empty? a) b]
     [(empty? b) a]
     [else ( intercalar-aux a b #t '() )] ; usar función auxiliar para saber qué elemento de la lista añadir (turno que es boolean, #t para a, #f para b)
     )
   )

( define (intercalar-aux a b turno result)
   (cond
     [(empty? a) (append result b) ] ; si ya acabamos con una lista añadimos lo que falte de la otra (casos base)
     [(empty? b) (append result a) ]
     [else (if (eq? #t turno) ; si el turno es #t va elemento de a, si el turno es #f va elemento de b
               ; vuelvo a verificar sin el elemento que añadí al resultado, cambio el turno y añado al resultado el elemento
               (intercalar-aux (rest a) b (not turno) (append result (list (first a)) ) )
               (intercalar-aux a (rest b) (not turno) (append result (list (first b)) ) ) )]
     )
   )

#| esa fue la primera idea que tuvimos, pero luego nos dimos cuenta de una forma más limpia :
( define ( intercalar a b )
   (cond
     [(empty? a) b]
     [(empty? b) a]
     [else ( cons (first a) (intercalar b (rest a)) )]
     )
   )

Sólo invirtiendo la lista para ir añadiendo uno de cada una
|#
