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
     [(< minutos 1) (error 'avanzar-minuto "Los minutos debes ser positivos") ] ;no puede haber añadidura de minutos negativos y sumar 0 no tiene sentido
     [(partido-finalizado p) (error 'avanzar-minuto "Partido finalizado") ] ;no se puede añadir minutos a un partido finalizado
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
  (if (>= n (- 90 actuales))  ;checa si lo que le queremos sumar supera los minutos restantes del partido
      #f
      #t
      )
  )

    
; EJERCICIO 7
;; registrar-gol : partido symbol symbol - > partido
( define ( registrar-gol p lado nombre-jugador )
   (cond
     [(partido-finalizado p)
      (error 'registrar-gol "El partido está finalizado") ]  ;no podemos en un partido finalizado
     [(not (or (symbol=? lado 'local) (symbol=? lado 'visitante) ))
      (error 'registrar-gol "El simbolo del equipo debe ser 'local o 'visitante") ] ;checamos si el símbolo del equipo es correcto
     [(not (pertenece-equipo p lado nombre-jugador) )
      (error 'registrar-gol "El jugador no pertenece a ese equipo") ] ;checamos si el jugador sí es de ese equipo.
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


; Función auxiliar recursiva para buscar al jugador por su nombre en la lista
;; busca-jugador? : symbol list -> boolean
(define (busca-jugador? nombre lista-jugadores)
  (cond
    [(empty? lista-jugadores) #f]
    [(symbol=? nombre (jugador-nombre (first lista-jugadores))) #t]
    [else (busca-jugador? nombre (rest lista-jugadores))]))

; Función auxiliar para ver si pertenece a una equipo
;;pertenece-equipo : partido symbol symbol
(define (pertenece-equipo ptd lado-jugador nom-jugador)
  (if (symbol=? lado-jugador 'local)
      (busca-jugador? nom-jugador (equipo-jugadores (partido-local ptd))) ; mando a llamar busca-jugador con las listas de ambos equipos
      (busca-jugador? nom-jugador (equipo-jugadores (partido-visitante ptd)))))



; EJERCICIO 8
;; buscar-jugadores : partido ( jugador - > boolean ) - > ( listof jugador )
( define ( buscar-jugadores p pred )
   (append (filter pred (equipo-jugadores (partido-local p)))
         (filter pred (equipo-jugadores (partido-visitante p))) )

   )


; PUNTOS EXTRAS

; EJERCICIO 9

;; resultado-partido : partido - > symbol
( define ( resultado-partido p )
   (cond
     [(not (partido-finalizado p))
      (error 'resultado-partido "El partido no ha finalizado") ]
     [(> (partido-goles-local p) (partido-goles-visitante p)) 'gana-local ]
     [(> (partido-goles-visitante p) (partido-goles-local p)) 'gana-visitante ]
     [else 'empate ]
     )
   )


; EJERCICIO 10
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

(probar-partido)
