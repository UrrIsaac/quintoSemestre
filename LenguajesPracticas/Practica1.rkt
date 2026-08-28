#lang plai
#|
PRÁCTICA 01
ALUMNOS :
   Andrade Castañeda Angel
   Urrutia Alfaro Isaac Arturo
|#

; EJERCICIO 1
;; password-aceptable? : String -> Boolean
; ; ejemplo :
; (password-aceptable? "Racket2027")   #t

; La funcion trabaja con un cond, primero verificamos si tiene menos de 8 caracteres, si no tiene número (usa una funcion auxiliar) y si no tiene mayuscula (con otra funcion auxiliar), en cualquier caso ocupamos la funcion string-a-lista y despues si es cierta alguna regresa #f, en otro caso #t

(define (password-aceptable? contraseña)
	(cond
	  [(< (string-length contraseña) 8) #f]
	  [(not (tiene-numero? (string-a-lista contraseña))) #f]
	  [(not (tiene-mayuscula? (string-a-lista contraseña))) #f]
	  [else #t]))


;; tiene-numero? : List -> Boolean
; Ocupamos recursión recorriendo la lista 
(define (tiene-numero? cadena)
  (if (empty? cadena)
      #f ; Caso base, si es vacia regresamos false
      (if (char-numeric? (first cadena)) ; preguntamos si el primer elemento de la lista es un numero 
                        #t ; Caso base 2, regresamos true si es cierto
                        (tiene-numero? (rest cadena))))) ; Caso rescursivo, en caso  contrario aplicamos tiene-numero? a el resto de la lista 

;; tiene-mayuscula? : List -> Boolean
; Ocupamos recursion recorriendo la lista
(define (tiene-mayuscula? cadena)
  (if (empty? cadena)
      #f ; Caso base, si es vacia regresamos false
      (if (char-upper-case? (first cadena)) ; preguntamos si el primer elemento es una mayuscula
                           #t ; Caso base 2, si es cierto regresamos true
                           (tiene-mayuscula? (rest cadena))))) ; Caso recursivo, en caso contrario aplicamos la funcion tiene-mayuscula al resto de la lista


;; string-a-lista : String -> list
; Esta funcion toma un String y lo convierte en una lista ocupando otra funcion y con el indice 0
(define (string-a-lista cadena)
  (string-a-lista-recursion cadena 0))

;; string-a-lista-recursion : String Number -> list
; Esta funcion hace la recursion que ocupa la funcion string-a-lista
(define (string-a-lista-recursion cadena posicion)
  (if (= posicion (string-length cadena)) ; vamos si la posicion es igual a la longitud de la cadena
      '() ; caso base, de ser cierto regresamos la lista vacia 
      (cons (string-ref cadena posicion) ; caso recursivo, en caso contrario unimos la lista que contiene el elemento de la cadena de la posicion donde estemos con la lista que le aplica la funcion string-a-lista-recursion a la posicion actual mas 1.
            (string-a-lista-recursion cadena (+ posicion 1)))))


; EJERCICIO 2
;; triangulo-valido? : Number Number Number - > Boolean
; ejemplo :
; (triangulo-valido? 3 4 5)	#t

;La funcion ocupa un cond que reisa todas las condiciones que debe tener 

(define (triangulo-valido? lado1 lado2 lado3)
	(cond
	  [(or (or (<= lado1 0) (<= lado2 0)) (<= lado3 0)) #f] ; Primero vemos si el lado 1 o el lado 2, o el lado 3 son menosres o iguales a 0, si alguno se cumple regresamos false
	  [(<= (+ lado1 lado2) lado3) #f] ; Si la suma del lado 1 con el lado 2 es menor o igual que el lado 3 regresamos false
	  [(<= (+ lado1 lado3) lado2) #f] ; Si la suma del lado 1 con el lado 3 es menor o igual que el lado 2 regresamos false
	  [(<= (+ lado2 lado3) lado1) #f] ; Si la suma del lado 2 con el lado 3 es menor o igual que el lado 1 regresamos false
	  [else #t])) ; Si no cumple alguno de los casos anteriores significa que es valido y regresamos true


; EJERCICIO 3
;; letras-repetidas? : String -> Boolean
; ejemplo :
; (letras-repetidas? "hola") #f

; Ocupamos una funcion auxiliar llamada letras-repetidas-aux?
(define (letras-repetidas? s)
  (letras-repetidas-aux? (string-a-lista s))) ; Ocupamos la funcion string-a-lista del ejercicio 1

;; letras-repetidas-aux? : List<Char> -> Boolean
; Esta funcion ocupa recursion para ver si existe alguna letra repetida 
(define (letras-repetidas-aux? lista)
  (if (empty? lista) ; preguntamos si la lista esta vacia 
      #f ; Caso base, en caso de ser cierto regresamos false 
      (if (contiene? (first lista) (rest lista)) ; preguntamos con una funcion auxiliar si la lista contiene el primer elemnto de la lista en el resto de la lista
          #t ; caso base 2, de ser cierto regresamos true
          (letras-repetidas-aux? (rest lista))))) ; Caso recursivo, en caso contrario aplicamos la funcion letras-repetidas-aux? a la lista sin el primer elemento


;; contiene? : Char List<Char> -> Boolean
; Esta funcion verifica si un caracter esta en una lista 
(define (contiene? caracter lista)
  (if (empty? lista) ; Preguntamos si la lista es vacia 
      #f ; Caso base; de ser cierrto regresamos #f
      (if (char=? caracter (first lista)) ; preguntamos si el caracter es el mismo que el primero de la lista
          #t ; Caso base 2, de ser cierto regresamos true
          (contiene? caracter (rest lista))))) ; Caso recursivo, aplicamos contiene? a el caracter que buscamos con la lista sin el primer elemento



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
