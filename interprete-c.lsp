; Funcion principal
(defun run (prg val &optional (mem nil))
(if (null prg) nil
 (cond
  ((eq (caar prg) 'int) (run (cdr prg) val (def-mem prg mem)))
  ((eq (caar prg) 'main) (ejecutar (cadar prg) val mem))
  (T 'no-hay-main)
)))

; Carga de variables en memoria
(defun def-mem (prg mem)
(if (null mem) (asignar-variables (mapcar (lambda(x)(cons x nil)) (cdar prg)))
 (append mem (asignar-variables (mapcar (lambda(x)(cons x nil)) (cdar prg)))))
)

; Dada una lista de listas empareja variable con valor
(defun asignar-variables (list)
(if (null list) nil
 (if (eq (car (nth 1 list)) '=) 
  (cons (append (car list) (nth 2 list)) (asignar-variables (nthcdr 3 list)))
  (cons (car list) (asignar-variables (cdr list)))))
)

; Dada una variable y un valor, le asigna ese valor a esa variable
(defun asignar-valor (var val mem)
(if (es-var var mem)
 (cons (list var val) (remove var mem :key 'car))
 (error "ERROR_VARIABLE_NO_DECLARADA."))
)

; Devuelve si la variable pertenece a la memoria
(defun es-var (var mem)
(not (null (member var mem :key 'car)))
)

; Ejecuta el programa
(defun ejecutar (prg val mem &optional (sal nil))
(if (null prg) sal
 (cond 
  ((eq (caar prg) 'scanf) (ejecutar (cdr prg) (cdr val) 
   (asignar-valor (nth 1 (car prg)) (car val) mem) sal))
  ((eq (caar prg) 'printf) (ejecutar (cdr prg) val mem 
   (append sal (salida (cdar prg) mem))))
  ((eq (nth 1 (car prg)) '=) (ejecutar (cdr prg) val 
   (asignar-valor (caar prg) (evaluar (cddar prg) mem) mem) sal))
 )
))

; Devuelve la salida imprimible de una expresión
(defun salida (exp mem)
(cond
 ((null exp) nil)
 ((reduce (lambda (x y) (and x y)) (mapcar (lambda(x)(es-var x mem)) exp) :initial-value T) 
  (append (list (valor-var (car exp) mem)) (salida (cdr exp) mem)))
 (T (evaluar exp mem))
))

; Devuelve el valor de una variable
(defun valor-var (var mem)
(if (es-var var mem)
 (if (null (cadr (find var mem :key 'car))) 0 (cadr (find var mem :key 'car)))
 (error "ERROR_VARIABLE_NO_DECLARADA."))
)

; Evalua una expresión y devuelve el resultado
(defun evaluar (exp mem)
(cond 
 ((null exp) nil)
 ((numberp exp) exp)
 ((listp exp) 
  (cond 
   ((eq (length exp) 1) (evaluar (car exp) mem))
   ((reduce (lambda (x y) (or x y)) (mapcar #'es-operador exp) :initial-value nil) 
    (apply (operador-a-funcion (nth (pos-op-menor-peso exp) exp)) 
     (cons (evaluar (butlast exp (- (length exp) (pos-op-menor-peso exp))) mem)
     (list (evaluar (nthcdr (+ '1 (pos-op-menor-peso exp)) exp) mem)))))
   (T (mapcar (lambda (x)(evaluar x mem)) exp))))
 (T (valor-var exp mem))
))

; Dado un operador devuelve una función que devuelve 1 o 0
(defun funcion-booleana (op)
(lambda (x y) (if (funcall op x y) 1 0))
)

; Devuelve una función válida de Lisp a partir de un operador binario de C.
(defun operador-a-funcion (op)
(cond
 ((member op '(< > <= >=)) (funcion-booleana op))
 ((eq op '==) (funcion-booleana 'eq))
 ((eq op '!=) (funcion-booleana (lambda (x y) (not (eq x y)))))
 ((eq op '%) 'mod)
 (t op)
))

(defparameter *mapa-op-asignacion* '((+= +) (-= -) (*= *) (/= /) (%= %)))

(defparameter *mapa-pesos-op*
'((== 1) (!= 1) (< 2) (> 2) (<= 2) (>= 2) (+ 3) (- 3) (* 4) (/ 5) (% 6)))

; Retorna el operador de LISP asociado a un operador de asignación.
(defun buscar-operador (op)
(cadr (find op *mapa-op-asignacion* :key 'car)))

; Verifica si un símbolo es un operador en C.
(defun es-operador (op)
(not (null (member op *mapa-pesos-op* :key 'car))))

; Retorna el peso de un operador en C. Mientras más grande su peso, mayor su 
; precedencia.
(defun peso-operador (op)
(cadr (find op *mapa-pesos-op* :key 'car)))

; Devuelve la posición en una lista del operador con menor peso
(defun pos-op-menor-peso (lista &optional (pos 0) (peso 9))
(cond
 ((null lista) pos)
 ((es-operador (car lista)) (if (> peso (peso-operador (car lista))) 
  (pos-op-menor-peso (cdr lista) (+ 1 pos) (peso-operador (car lista)))
  (pos-op-menor-peso (cdr lista) pos peso)))
 (T (pos-op-menor-peso (cdr lista) pos peso))
))