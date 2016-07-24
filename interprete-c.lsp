; ************************************************************
; Funcion principal
; ************************************************************
(defun run (prg val &optional (mem nil))
(if (null prg) nil
 (cond
 
  ; Hace la definicón de las variables correspondientes
  ((eq (caar prg) 'int) (run (cdr prg) val (def-mem prg mem)))

  ; Ejecuta el main el programa
  ((eq (caar prg) 'main) (ejecutar (cadar prg) val mem))

  ; Si no hay main a ejecutar termina la ejecución
  (T 'no-hay-main)
)))


; ************************************************************
; Carga de variables en memoria
; ************************************************************
(defun def-mem (prg mem)
(if (null mem) (asignar-variables (mapcar (lambda(x)(cons x nil)) (cdar prg)))
 (append mem (asignar-variables (mapcar (lambda(x)(cons x nil)) (cdar prg)))))
)


; ************************************************************
; Dada una lista de listas empareja variable con valor usado solo 
; en la definicion inicial de variables
; ************************************************************
(defun asignar-variables (list)
(if (null list) nil
 (if (eq (car (nth 1 list)) '=) 
  (cons (append (car list) (nth 2 list)) (asignar-variables (nthcdr 3 list)))
  (cons (car list) (asignar-variables (cdr list)))))
)


; ************************************************************
; Dada una variable y un valor, le asigna ese valor a esa variable
; ************************************************************
(defun asignar-valor (var val mem)
(if (es-var var mem)
 (cons (list var val) (remove var mem :key 'car))
 (error "(ERROR_VARIABLE_NO_DECLARADA)"))
)

; Devuelve si la variable pertenece a la memoria
(defun es-var (var mem)
(not (null (member var mem :key 'car)))
)


; ************************************************************
; Ejecuta el programa
; ************************************************************
(defun ejecutar (prg val mem &optional (sal nil))
(if (null prg) sal
 (cond 

  ; SCANF
  ((eq (caar prg) 'scanf) (ejecutar (cdr prg) (cdr val) 
   (asignar-valor (nth 1 (car prg)) (car val) mem) sal))

  ; PRINTF
  ((eq (caar prg) 'printf) (ejecutar (cdr prg) val mem 
   (append sal (salida (cdar prg) mem))))

  ; ASIGNACIÓN DE VARIABLES
  ((eq (nth 1 (car prg)) '=) (ejecutar (cdr prg) val 
   (asignar-valor (caar prg) (evaluar (cddar prg) mem) mem) sal))

  ; IF THEN ELSE
  ((eq (caar prg) 'if) (if (eq (evaluar (nth 1 (car prg)) mem) 1)
   (ejecutar (append (nth 2 (car prg)) (cdr prg)) val mem sal)
   (if (eq (nth 3 (car prg)) 'else)
    (ejecutar (append (nth 4 (car prg)) (cdr prg)) val mem sal)
    (ejecutar (cdr prg) val mem sal))))

  ; WHILE
  ((eq (caar prg) 'while) (if (eq (evaluar (nth 1 (car prg)) mem) 1)
   (ejecutar (append (nth 2 (car prg)) prg) val mem sal)
   (ejecutar (cdr prg) val mem sal)))

  ; ++ VAR -> VAR = VAR + 1
  ((eq (caar prg) '++) 
   (ejecutar (cons (append (cdar prg) '(=) (cdar prg) '(+ 1)) (cdr prg)) val mem sal))

  ; -- VAR -> VAR = VAR - 1
  ((eq (caar prg) '--) 
   (ejecutar (cons (append (cdar prg) '(=) (cdar prg) '(- 1)) (cdr prg)) val mem sal))

  ; VAR ++ -> VAR = VAR + 1
  ((eq (nth 1 (car prg)) '++) 
   (ejecutar (cons (append (list (caar prg)) '(=) (list (caar prg)) '(+ 1)) (cdr prg)) val mem sal))

  ; VAR -- -> VAR = VAR + 1
  ((eq (nth 1 (car prg)) '--) 
   (ejecutar (cons (append (list (caar prg)) '(=) (list (caar prg)) '(- 1)) (cdr prg)) val mem sal))

  ; VAR <+= -= *= /= %=>  ... -> VAR = VAR <+ - * / %> (...)
  ((not (null (member (nth 1 (car prg)) *mapa-op-asignacion* :key 'car)))
   (ejecutar (cons (append (list (caar prg)) '(=) (list (caar prg))
    (buscar-operador (nth 1 (car prg))) (list (cddar prg))) (cdr prg)) val mem sal))

  ; (ejecutar ((PRG)) val mem sal) -> (ejecutar (PRG) val mem sal)
  (T (ejecutar (car prg) val mem sal))
 )
))


; ************************************************************
; Devuelve la salida imprimible de una expresión
; ************************************************************
(defun salida (exp mem)
(cond
 ((null exp) nil)
 ((reduce (lambda (x y) (and x y)) 
  (mapcar (lambda(x)(es-var x mem)) exp) :initial-value T) 
  (append (list (valor-var (car exp) mem)) (salida (cdr exp) mem)))
 (T (list (evaluar exp mem)))
))


; ************************************************************
; Devuelve el valor de una variable
; ************************************************************
(defun valor-var (var mem)
(if (es-var var mem)
 (if (null (cadr (find var mem :key 'car))) 0 (cadr (find var mem :key 'car)))
 (error "(ERROR_VARIABLE_NO_DECLARADA)"))
)


; ************************************************************
; Evalua una expresión y devuelve el resultado
; ************************************************************
(defun evaluar (exp mem)
(cond 

 ; NIL -> NIL
 ((null exp) nil)

 ; NUMERO -> NUMERO
 ((numberp exp) exp)

 ; EVALUA LISTAS
 ((listp exp) 
  (cond 

   ; (evaluar (exp) mem) -> (evaluar exp mem)
   ((eq (length exp) 1) (evaluar (car exp) mem))

   ; OPERACIONES ARITMÉTICAS
   ((reduce (lambda (x y) (or x y)) (mapcar #'es-operador exp) :initial-value nil) 
    (apply (operador-a-funcion (nth (pos-op-menor-peso exp) exp)) 
     (cons (evaluar (butlast exp (- (length exp) (pos-op-menor-peso exp))) mem)
      (list (evaluar (nthcdr (+ '1 (pos-op-menor-peso exp)) exp) mem)))))

   ; EVALUA EL SEGUNDO MIEMBRO DE LA ASIGNACIÓN
   ((eq (nth 1 exp) '=) (evaluar (cddr exp) mem))

   ; EVALUA CADA COMPONENTE DE LA EXPRESIÓN
   (T (mapcar (lambda (x)(evaluar x mem)) exp))))

 ; VALOR DE LA VARIABLE
 (T (valor-var exp mem))
))


; ************************************************************
; Dado un operador devuelve una función que devuelve 1 o 0
; ************************************************************
(defun funcion-booleana (op)
; NIL -> 0; !=NIL -> 1
(lambda (x y) (if (funcall op x y) 1 0)))


; ************************************************************
; Devuelve una función válida de Lisp a partir de un operador binario de C.
; ************************************************************
(defun operador-a-funcion (op)
(cond
 ((not (null (member op '(< > <= >=)))) (funcion-booleana op))
 ((eq op '==) (funcion-booleana 'eq))
 ((eq op '!=) (funcion-booleana (lambda (x y) (not (eq x y)))))
 ((eq op '%) 'mod)
 (T op)
))


; ************************************************************
; Operadores artméticos combinados con asignación.
; ************************************************************
(defparameter *mapa-op-asignacion* '((+= +) (-= -) (*= *) (/= /) (%= %)))


; ************************************************************
; Operadores aritméticos y de comparación.
; ************************************************************
(defparameter *mapa-pesos-op*
'((== 1) (!= 1) (< 2) (> 2) (<= 2) (>= 2) (+ 3) (- 3) (* 4) (/ 5) (% 6)))


; ************************************************************
; Retorna el operador de LISP asociado a un operador de asignación.
; ************************************************************
(defun buscar-operador (op)
(cdr (find op *mapa-op-asignacion* :key 'car)))


; ************************************************************
; Verifica si un símbolo es un operador en C.
; ************************************************************
(defun es-operador (op)
(not (null (member op *mapa-pesos-op* :key 'car))))


; ************************************************************
; Retorna el peso de un operador en C. Mientras más grande su peso, mayor su 
; precedencia.
; ************************************************************
(defun peso-operador (op)
(cadr (find op *mapa-pesos-op* :key 'car)))


; ************************************************************
; Devuelve la posición en una lista del operador con menor peso
; ************************************************************
(defun pos-op-menor-peso (lista &optional (pos 0) (peso 9))
(cond
 ((null lista) pos)
 ((es-operador (car lista)) (if (> peso (peso-operador (car lista))) 
  (pos-op-menor-peso (cdr lista) (+ 1 pos) (peso-operador (car lista)))
  (pos-op-menor-peso (cdr lista) pos peso)))
 (T (pos-op-menor-peso (cdr lista) pos peso))
))


; ************************************************************
; Devuelve si hay un elemento en una lista multinivel
; ************************************************************
(defun buscar (elem lista)
(if (null lista) nil
 (if (listp lista) 
  (or (eq (find elem lista) elem) 
   (buscar elem (car lista))
   (buscar elem (cdr lista)))
  (eq elem lista))
))

  
  ; ************************************************************
; Recorre recursivamente una operación aritmetica que incluye asignaciones
; y realiza las asignaciones
; ************************************************************
(defun asignar-valor-recur (lista mem)
(if (null (buscar '= lista)) mem
 (asignar-valor (car lista) (evaluar (cddr lista) 
  (asignar-valor-recur (cddr lista) mem)) 
  (asignar-valor-recur (cddr lista) mem))
))