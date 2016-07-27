; ************************************************************
; Funcion principal
; ************************************************************
(defun evaluar(exp &optional (amb))
; EVALUA ATOMOS
(if (atom exp)

 ; NUMERO O AMBIENTE NULO -> EXP
 (if (or (numberp exp) (null amb)) exp

  ; VALOR DE EXP
  (valor exp amb))

 ; EVALUA LISTAS
 (cond

  ; QUOTE
  ((eq (car exp) 'quote) (cadr exp))

  ; AND
  ((eq (car exp) 'and) (and (evaluar (cadr exp) amb) (evaluar (caddr exp) amb)))

  ; OR
  ((eq (car exp) 'or) (or (evaluar (cadr exp) amb) (evaluar (caddr exp) amb)))

  ; CAR
  ((eq (car exp) 'car) (evaluar (car (evaluar (cadr exp) amb)) amb))

  ; LIST
  ((eq (car exp) 'list) (cdr exp))

  ; CDR
  ((eq (car exp) 'cdr) (evaluar (cdr (evaluar (cadr exp) amb)) amb))

  ; (LAMBDA) -> LAMBDA
  ((eq (car exp) 'lambda) exp)

  ; LAMBDA EN AMBIENTE
  ((buscar 'lambda amb) (evaluar exp (crear-varios amb)))

  ; EVALUA LISTAS DE LISTAS
  ((listp (car exp)) (cond 

   ; ((LAMBDA))
   ((eq (caar exp) 'lambda) (apply (car exp) (evaluar (cdr exp) amb)))

   ; EVALUA MIEMBRO A MIEMBRO DE LA LISTA
   (T (mapcar (lambda (x) (evaluar x amb)) exp))))

   ; / FN
   ((and (eq (car exp) '/) (or (not (null (member (nth 1 exp) amb))) (es-funcion (car exp))))
    (if (<= (length (evaluar (nth 2 exp) amb)) 1) (car (evaluar (nth 2 exp) amb))
     (evaluar (append (list '/) (list (evaluar (nth 1 exp) amb)) (list (append (butlast (evaluar (nth 2 exp) amb) 2) 
      (list (evaluar (list (nth 1 exp) (car (last (butlast (evaluar (nth 2 exp) amb)))) 
       (car (last (evaluar (nth 2 exp) amb)))) amb))))) amb)))

  ; ES FUNCION ESTANDAR
  ((es-funcion (car exp)) (apply (car exp) (mapcar (lambda (x) (evaluar x amb)) 
                (cdr exp))))

  ; WHILE
  ((eq (car exp) 'while) (if (evaluar (list (nth 1 exp) (evaluar (nth 3 exp) amb)) amb) 
   (evaluar (append (butlast exp) (list (evaluar (list (nth 2 exp) 
    (evaluar (nth 3 exp) amb)) amb))) amb) 
   (evaluar (nth 3 exp) amb)))

   ; EJECUTA FUNCIONES DEFINIDAS EN EL AMBIENTE
  ((and (not (null (member (car exp) amb))) (eq (car amb) 'amb-def)) (apply (car exp) (cdr exp)))

  ; EVALUA MIEMBRO A MIEMBRO
  (T (mapcar (lambda (x) (evaluar x amb)) exp))
 )
))


; ************************************************************
; Busca valor en el ambiente y devuelve el asociado, 
; si no lo encuentra devuelve el parámetro
; ************************************************************
(defun valor (var amb)
(if (null amb) var
 (if (eq var (car amb)) (cadr amb)
  (valor var (cddr amb)))
))

  
; ************************************************************
; Devuelve si la funcion se puede usar en un apply o no
; ************************************************************
(defun es-funcion (fn)
(not (null (member fn '(append + - * / < > eq atom null listp numberp length mapcar)))))


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
; Arma la función definida en el ambiente que tiene 2 parámetros
; ************************************************************
(defun crear-2-param (name op)
(car (cons name (setf (symbol-function name) (lambda (x y) (apply op (list x y)))))))


; ************************************************************
; Arma la función definida en el ambiente que tiene un solo parámetro
; ************************************************************
(defun crear-1-param (name op)
(car (cons name (setf (symbol-function name) (lambda (var) (funcall op var))))))


; ************************************************************
; Arma las funciones definidas en el ambiente
; ************************************************************
(defun crear-varios (ambiente)
(cons 'amb-def (mapcar (lambda (x) 
 (if (> (length (nth 1 (cadr x))) 1)
  (crear-2-param (car x) (cadr x)) 
  (crear-1-param (car x) (cadr x)))) 
 (armar-lista ambiente))
))


; ************************************************************
; Con una lista de pares variable valor arma una lista de listas de pares
; ************************************************************
(defun armar-lista (lista)
(if (< (length lista) '2) nil
 (cons (list (car lista) (nth 1 lista)) (armar-lista (cddr lista)))
))