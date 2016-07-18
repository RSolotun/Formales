; Funcion principal
(defun evaluar(exp &optional (amb))
(if (atom exp)
 (if (or (numberp exp) (null amb)) exp
  (valor exp amb))
 (cond
  ((eq (car exp) 'quote) (cadr exp))
  ((eq (car exp) 'and) (and (evaluar (cadr exp) amb) (evaluar (caddr exp) amb)))
  ((eq (car exp) 'or) (or (evaluar (cadr exp) amb) (evaluar (caddr exp) amb)))
  ((eq (car exp) 'car) (evaluar (car (evaluar (cadr exp) amb)) amb))
  ((eq (car exp) 'list) (cdr exp))
  ((eq (car exp) 'cdr) (evaluar (cdr (evaluar (cadr exp) amb)) amb))
  ((eq (car exp) 'lambda) exp)
  ((buscar 'lambda amb) (evaluar exp (crear-varios amb)))
  ((listp (car exp)) (if (eq (caar exp) 'lambda) (apply (car exp) (cdr exp)) amb))
  ((es-funcion (car exp)) (apply (car exp) (mapcar (lambda (x) (evaluar x amb)) 
                (cdr exp))))
  ((eq amb 'amb-def) (apply (car exp) (cdr exp)))
  (T (mapcar (lambda (x) (evaluar x amb)) exp))
 )
))

; Busca valor en el ambiente y devuelve el asociado
(defun valor (valor amb)
(if (null amb) nil
 (if (eq valor (car amb)) (cadr amb)
  (valor valor (cddr amb)))))

; Devuelve si la funcion se puede usar en un apply o no
(defun es-funcion (fn)
(member fn '(append + - * / < > eq atom null listp numberp length mapcar)))

; Devuelve si hay un elemento en una lista multinivel
(defun buscar (elem lista)
(if (null lista) nil
 (if (listp lista) 
  (or (eq (find elem lista) elem) (buscar elem (cadr lista)) (buscar elem (cddr lista)))
  (eq elem lista))))


; Arma la función definida en el ambiente
(defun crear (name op)
(setf (symbol-function name) (lambda (var) (funcall op var))))

; Arma las funciones definidas en el ambiente
(defun crear-varios (ambiente)
(if (mapcar (lambda (x) (crear (car x) (cadr x))) (armar-lista ambiente)) 'amb-def nil))

; Con una lista de pares variable valor arma una lista de listas de pares
(defun armar-lista (lista)
(if (< (length lista) '2) nil
 (cons (list (car lista) (nth 1 lista)) (armar-lista (cddr lista)))))
