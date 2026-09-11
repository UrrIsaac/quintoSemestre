#lang plai
#|
PRÁCTICA 02
ALUMNOS :
   Andrade Castañeda Angel 322164741
   Urrutia Alfaro Isaac Arturo 322287879
|#

; PARTE 2.1 FUNCIONES DE ORDEN SUPERIOR Y FUNCIONES ANÓNIMAS

; EJERCICIO 1
;; filtra-rango :
;; number number ( listof number ) - > ( listof number )

(define (filtra-rango a b list)
  (filter
   (lambda (x)
     (and (>= x a) (<= x b))) list)) ; revisamos de cada elemento de la lista que sea mayor a "a" y menor que "b" 

; EJERCICIO 2
;; cuenta-si :
;; (A - > boolean ) ( listof A) - > integer

(define (cuenta-si p? list)
  (foldl (lambda (x acumulador) ; el fold es desde la izquierda 
           (if (p? x)
               (+ 1 acumulador)
               acumulador)) 0 list)) ; preguntamos para cada elemento si cumple con p?, en caso de que si sumamos 1 a un acumulador inciado en 0

; EJERCICIO 3
; suma-transformados :
;; ( number - > number )
;; ( listof number ) - > number

(define (suma-transformados f list)
  (if (empty? list) ; hacemos un caso recursivo
      0 ; caso base, si la lista es vacia sumamos 0
      (foldl + 0
         (map (lambda (x)
                (f x)) list)))) ; el foldl recibe la funcion suma y se inicia en 0, con un map le aplicaremos a cada elemento de la lista a funcion f, los resultados se sumaran por el foldl 


; PARTE 2.2 TIPO DE DATOS ABSTRACTOS

; Estructuras definidas en la práctica
( struct jugador ( nombre dorsal posicion ) #:transparent )
( struct equipo ( nombre jugadores ) #:transparent )
( struct evento ( tipo equipo jugador minuto ) #:transparent )

; Estructuras definidas en el laboratorio

( struct partido
   ( local
     visitante
     goles-local
     goles-visitante
     minuto
     eventos
     finalizado )
   #:transparent )


;; crear-partido : equipo equipo - > partido
( define ( crear-partido local visitante )
   (if (or (empty? (equipo-jugadores local))
           (empty? (equipo-jugadores visitante))
           )
   (error 'crear-partido "no se puede crear un partido con un equipo sin jugadores")

   (partido
    local visitante 0 0 0 '() #f
    )
   
   )
)

; EJERCICIO 6
;; avanzar-minuto : partido natural - > partido

( define ( avanzar-minuto p minutos )
   (cond
     [(< minutos 1) (error 'avanzar-minuto "Los minutos debes ser positivos") ]
     [(partido-finalizado p) (error 'avanzar-minuto "Partido finalizado") ]
     [ (if (hay-tiempo (partido-minuto p)  minutos)
           ;sí hay tiempo, osea se sí se puede sumar
           (partido
            (partido-local p)
            (partido-visitante p)
            (partido-goles-local p)
            (partido-goles-visitante p)
            (+ minutos (partido-minuto p) )
            (partido-eventos p)
            #f)
           
           ;no hay tiempo, osea se no se puede sumar
           (partido
            (partido-local p)
            (partido-visitante p)
            (partido-goles-local p)
            (partido-goles-visitante p)
            90
            (cons (evento 'fin-partido 'ninguno 'ninguno 90 ) (partido-eventos p) ) ;hasta enfrente el último evento
            #t)
           )

     ]
    )
)

; funcion auxiliar de avanzar-minuto
; hay-tiempo natural natural -> booleano
(define (hay-tiempo actuales n)
  (if (>= n (- 90 actuales))
      #f
      #t
      )
  )

    
; EJERCICIO 7
;; registrar-gol : partido symbol symbol - > partido
( define ( registrar-gol p lado nombre-jugador )
   (cond
     [(partido-finalizado p)
      (error 'registrar-gol "El partido está finalizado") ]
     [(not (or (symbol=? lado 'local) (symbol=? lado 'visitante) ))
      (error 'registrar-gol "El simbolo del equipo debe ser 'local o 'visitante") ]
     [(not (pertenece-equipo p lado nombre-jugador) )
      (error 'registrar-gol "El jugador no pertenece a ese equipo") ]
     [else (if (symbol=? lado 'local)
               ;gol del local
               (partido
                (partido-local p)
                (partido-visitante p)
                (+ 1 (partido-goles-local p))
                (partido-goles-visitante p)
                (partido-minuto p)
                (cons (evento 'gol lado nombre-jugador (partido-minuto p) ) (partido-eventos p) ) ;hasta enfrente el último evento
                (partido-finalizado p)
                )
               ;gol del visitante
               (partido
                (partido-local p)
                (partido-visitante p)
                (partido-goles-local p)
                (+ 1 (partido-goles-visitante p))
                (partido-minuto p)
                (cons (evento 'gol lado nombre-jugador (partido-minuto p) ) (partido-eventos p) ) ;hasta enfrente el último evento
                (partido-finalizado p)
                )
               )]
     )

   )


;; Función auxiliar recursiva para buscar al jugador por su nombre
(define (busca-jugador? nombre lista-jugadores)
  (cond
    [(empty? lista-jugadores) #f]
    [(symbol=? nombre (jugador-nombre (first lista-jugadores))) #t]
    [else (busca-jugador? nombre (rest lista-jugadores))]))

;; Función pertenece-equipo actualizada sin usar let ni ormap
(define (pertenece-equipo ptd lado-jugador nom-jugador)
  (if (symbol=? lado-jugador 'local)
      (busca-jugador? nom-jugador (equipo-jugadores (partido-local ptd)))
      (busca-jugador? nom-jugador (equipo-jugadores (partido-visitante ptd)))))



; EJERCICIO 8
;; buscar-jugadores : partido ( jugador - > boolean ) - > ( listof jugador )

( define ( buscar-jugadores p pred )
   (append (filter pred (equipo-jugadores (partido-local p)))
         (filter pred (equipo-jugadores (partido-visitante p))) )

   )








; extra?
(define (probar-partido)
  (define j1 (jugador 'carlos 10 'delantero))
  (define j2 (jugador 'ana 8 'medio))
  (define j3 (jugador 'luis 1 'portero))
  
  (define j4 (jugador 'pedro 9 'delantero))
  (define j5 (jugador 'sofia 6 'defensa))
  (define j6 (jugador 'mateo 1 'portero))
  
  (define eq-local (equipo 'rojos (list j1 j2 j3)))
  (define eq-visitante (equipo 'azules (list j4 j5 j6)))
  
  (define p0 (crear-partido eq-local eq-visitante))
  (define p1 (avanzar-minuto p0 30))
  (define p2 (registrar-gol p1 'local 'carlos))
  (define p3 (registrar-gol p2 'visitante 'pedro))
  (define p-final (avanzar-minuto p3 60))
  
  p-final)


;(probar-partido)



; EJEMPLOS PARA PROBAR, QUITAR ANTES DE ENTREGAAAAAAR 
; ---------------------------------------------------------------------------
( define equipo-rojo
( equipo 'rojos
( list
( jugador 'memo 9 'delantero )
( jugador 'ana 10 'medio )
( jugador 'luis 1 'portero ) ) ) )

( define equipo-azul
( equipo 'azules
( list
( jugador 'memo2 9 'delantero )
( jugador 'ana2 10 'medio )
( jugador 'luis2 1 'portero ) ) ) )

(define partido1
( crear-partido equipo-rojo equipo-azul ) )

( define partido2
( avanzar-minuto partido1 25) )


( partido-minuto partido2 )
( partido-finalizado partido2 )

( define partido-final
( avanzar-minuto partido2 70) )

( partido-minuto partido-final )
( partido-finalizado partido-final )




;; Continuación con las pruebas para registrar-gol

(define partido3 (registrar-gol partido2 'local 'memo))

(partido-goles-local partido3)
;; Resultado esperado: 1

(partido-goles-visitante partido3)
;; Resultado esperado: 0

(first (partido-eventos partido3))
;; Resultado esperado: (evento 'gol 'local 'memo 25)

;; Prueba de error: Jugador inexistente
;(registrar-gol partido3 'visitante 'jugador-inexistente)
;; Resultado esperado: Error en registrar-gol: El jugador no pertenece a ese equipo

;; Prueba de error: Lado incorrecto
;(registrar-gol partido3 'invalido 'memo)
;; Resultado esperado: Error en registrar-gol: El simbolo del equipo debe ser 'local o 'visitante


(buscar-jugadores
 partido1
 (lambda (j)
   (symbol=? (jugador-posicion j)
             'portero)))


