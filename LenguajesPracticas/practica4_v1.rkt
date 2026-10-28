#lang plai

;; ============================================================
;; PARSER DE CFWAE+ -- CÓDIGO DEL DOCUMENTO, ANOTADO
;; ============================================================
;; Leyenda de las anotaciones:
;;   [ERROR n]  -> error o inconsistencia (n = número en mi análisis)
;;   [FALTA]    -> algo que el documento no incluye
;;   [SOBRA]    -> redundancia o cosa innecesaria / simplificable
;;   [DISEÑO]   -> decisión de diseño (no es error, conviene mencionarla)
;;   [NOTA]     -> aclaración
;;
;; El código es el del documento, unido en un solo archivo.
;; No está corregido: las anotaciones señalan los problemas.
;; ============================================================


;; [FALTA] El documento nunca muestra el encabezado del archivo.
;; Hay que suponer #lang plai (por define-type) y que first, second,
;; third, fourth, empty? y remove-duplicates vienen de racket/list.
;; Esta línea la agregué yo para que el archivo sea ejecutable.
#lang plai


;; ------------------------------------------------------------
;; ASA (sección 1 del documento)
;; ------------------------------------------------------------
(define-type CFWAE+
  [id   (i symbol?)]
  [num  (n number?)]            ;; [DISEÑO] number? también acepta flotantes, racionales y complejos
  [bool (b boolean?)]
  [op   (f procedure?)
        (args (listof CFWAE+?))]
  [ifc  (condition CFWAE+?)
        (then CFWAE+?)
        (else CFWAE+?)]
  [fun  (params (listof symbol?))
        (body CFWAE+?)]
  [app  (fun-expr CFWAE+?)
        (args (listof CFWAE+?))])

;; ------------------------------------------------------------
;; Funciones auxiliares (secciones 7, 9, 11)
;; ------------------------------------------------------------

;; operator? : symbol -> lista o #f
;; [ERROR] Aunque termina en "?", member devuelve una lista (o #f),
;; no un booleano. Funciona por ser valor "verdadero", pero es poco limpio.
;; [SOBRA] Esta lista de operadores se repite abajo en get-operator.
;; Una sola tabla (hash o lista de asociación símbolo -> procedimiento)
;; serviría para ambas funciones.
(define (operator? s)
  (member s
          '(+ - * /
            modulo expt
            add1 sub1
            = < > <= >=
            not)))

;; get-operator : symbol -> procedure
;; [SOBRA] El else es inalcanzable: parse solo la llama si operator? ya dio verdadero.
(define (get-operator op)
  (case op
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(modulo) modulo]
    [(expt) expt]
    [(add1) add1]
    [(sub1) sub1]
    [(=) =]
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(not) not]
    [else
     (error 'parse
            "Syntax Error: expresion mal formada en parse")]))

;; symbols? : lista -> boolean
;; [SOBRA] Envoltorio trivial de (andmap symbol? lst);
;; se podría usar directamente.
(define (symbols? lst)
  (andmap symbol? lst))

;; repeated? : lista -> boolean
(define (repeated? lst)
  (not (= (length lst)
          (length (remove-duplicates lst)))))

;; binding-names : lista de bindings -> lista de símbolos
;; [ERROR 1] Asume que bindings es una lista de listas. Si no lo es
;; (ej. {with {x 5} x}), (map first ...) falla con un error interno
;; de Racket y no con el "Syntax Error" del parser.
(define (binding-names bindings)
  (map first bindings))

;; binding-values : lista de bindings -> lista de s-expressions
;; [ERROR 1] Mismo problema; además, un binding con más de 2 elementos
;; (ej. {x 1 2}) pasa sin error porque solo se usa second.
(define (binding-values bindings)
  (map second bindings))


;; ------------------------------------------------------------
;; parse-with* (sección 12)
;; ------------------------------------------------------------
;; parse-with* : list s-expression -> CFWAE+
;; [ERROR 1] No valida nada: ni que binding sea lista de 2 elementos,
;; ni que name sea símbolo (con {with* {{1 2}} x} construye (fun '(1) ...)).
;; [DISEÑO] No verifica nombres repetidos, y es lo correcto: with* es
;; shadowing secuencial, a diferencia de with y fun.
(define (parse-with* bindings body)
  (cond
    [(empty? bindings)
     (parse body)]
    [else
     (let* ([binding (first bindings)]
            [name (first binding)]
            [value (second binding)])
       (app
        (fun (list name)
             (parse-with* (rest bindings)
                          body))
        (list (parse value))))]))


;; ------------------------------------------------------------
;; parse (secciones 2 a 16)
;; ------------------------------------------------------------
;; parse : s-expression -> CFWAE+
(define (parse sexp)
  (cond
    ;; Números
    [(number? sexp)
     (num sexp)]

    ;; Booleanos
    [(boolean? sexp)
     (bool sexp)]

    ;; Identificadores
    ;; [ERROR 5] Cualquier símbolo se acepta como id, incluso los
    ;; reservados: 'if, 'fun, 'with, 'with*, '+ solos se vuelven (id ...).
    ;; Tampoco se prohíben como parámetros: en {fun {+} {+ 1 2}} el + del
    ;; cuerpo se parsea como operador y no como el parámetro.
    [(symbol? sexp)
     (id sexp)]

    ;; Expresiones compuestas
    [(list? sexp)
     (cond

       ;; IF
       ;; [SOBRA] (pair? sexp) se repite en todos los casos; bastaría
       ;; verificar una sola vez que la lista no es vacía.
       [(and (pair? sexp)
             (equal? (first sexp) 'if))
        (if (= (length sexp) 4)
            (ifc (parse (second sexp))
                 (parse (third sexp))
                 (parse (fourth sexp)))
            (error 'parse
                   "Syntax Error: expresion mal formada en parse"))]

       ;; FUN  (es el único caso con validación completa)
       [(and (pair? sexp)
             (equal? (first sexp) 'fun))
        (cond
          ;; [SOBRA] El mismo mensaje de error se repite unas 10 veces en
          ;; todo el parser. Un auxiliar (syntax-error) lo resolvería.
          ;; Además el mensaje dice "en parse" cuando 'parse ya lo indica.
          [(not (= (length sexp) 3))
           (error 'parse
                  "Syntax Error: expresion mal formada en parse")]
          [(not (list? (second sexp)))
           (error 'parse
                  "Syntax Error: expresion mal formada en parse")]
          [(not (symbols? (second sexp)))
           (error 'parse
                  "Syntax Error: expresion mal formada en parse")]
          [(repeated? (second sexp))
           (error 'parse
                  "Syntax Error: expresion mal formada en parse")]
          [else
           (fun (second sexp)
                (parse (third sexp)))])]
       ;; [DISEÑO] {fun {} 5} (cero parámetros) es válido.

       ;; WITH
       ;; [ERROR 1] A diferencia de fun, aquí NO se valida:
       ;;   - longitud 3: {with} o {with {{x 1}}} hacen fallar a third
       ;;     con error interno de Racket;
       ;;   - que bindings sea lista de listas de 2 elementos;
       ;;   - que los nombres sean símbolos.
       ;; Solo se valida que los nombres no se repitan.
       [(and (pair? sexp)
             (equal? (first sexp) 'with))
        (let* ([bindings (second sexp)]
               [body (third sexp)]
               [names (binding-names bindings)]
               ;; [ERROR 4] "values" oculta la función values de Racket.
               ;; Funciona, pero conviene renombrarla (ej. vals).
               [values (binding-values bindings)])
          (if (repeated? names)
              (error 'parse
                     "Syntax Error: expresion mal formada en parse")
              (app
               (fun names
                    (parse body))
               (map parse values))))]

       ;; WITH*
       ;; [FALTA] En el documento este caso queda como "..." (sección 16).
       ;; Nunca se muestra la llamada a parse-with*. La línea de abajo
       ;; la completé yo.
       ;; [ERROR 1] Tampoco hay validación de longitud ni de bindings.
       [(and (pair? sexp)
             (equal? (first sexp) 'with*))
        (parse-with* (second sexp) (third sexp))]

       ;; OPERADORES
       ;; [ERROR 2] En la sección 7 la condición es
       ;;   (and (pair? sexp) (symbol? (first sexp)) (operator? (first sexp)))
       ;; y en la sección 16 solo (and (pair? sexp) (operator? (first sexp))).
       ;; Son equivalentes en la práctica, pero el documento muestra dos versiones.
       ;; [ERROR 3] No se valida aridad: {+}, {add1}, {not 1 2}, {sub1 1 2}
       ;; se aceptan aquí y fallan después en el intérprete con errores de
       ;; Racket. Hay que verificarla aquí o decir que se delega al intérprete.
       [(and (pair? sexp)
             (operator? (first sexp)))
        (op (get-operator (first sexp))
            (map parse (rest sexp)))]

       ;; APLICACIÓN
       ;; [DISEÑO] Se aceptan aplicaciones como {5 3}; el error aparece
       ;; hasta el intérprete. {f} (cero argumentos) también es válido.
       [(pair? sexp)
        (app
         (parse (first sexp))
         (map parse (rest sexp)))]

       ;; [NOTA] Este else solo alcanza a la lista vacía '().
       [else
        (error 'parse
               "Syntax Error: expresion mal formada en parse")])]

    ;; [NOTA] Este else alcanza a valores que no son número, booleano,
    ;; símbolo ni lista (cadenas, caracteres, etc.).
    [else
     (error 'parse
            "Syntax Error: expresion mal formada en parse")]))

