; Funcion principal
(defun evaluar(exp &optional (amb))
(if (atom exp)
 (if (or (numberp exp) (null amb)) exp
  (buscar exp amb))
 (cond
  ((eq (car exp) 'quote) (cadr exp))
  ((eq (car exp) 'and) (and (evaluar (cadr exp) amb) (evaluar (caddr exp) amb)))
  ((eq (car exp) 'or) (or (evaluar (cadr exp) amb) (evaluar (caddr exp) amb)))
  ((eq (car exp) 'car) (evaluar (car (evaluar (cadr exp) amb)) amb))
  ((eq (car exp) 'list) (cdr exp))
  ((eq (car exp) 'cdr) (evaluar (cdr (evaluar (cadr exp) amb)) amb))
  ((eq (car exp) 'lambda) exp)
  ((listp (car exp)) (if (eq (caar exp) 'lambda) (apply (car exp) (cdr exp)) amb))
  ((eq (car exp) 'mapcar) (mapcar (lambda (x) (evaluar (list (evaluar (cadr exp)) x) nil)) 
    (evaluar (caddr exp) nil)))
  ((es-funcion (car exp)) (apply (car exp) (cdr exp)))
  (T (mapcar (lambda (x) (evaluar x amb)) exp))
 )
))

; Busca un valor en el ambiente y devuelve el asociado
(defun buscar (valor amb)
  (if (null amb) nil
  (if (eq valor (car amb)) (cadr amb)
    (buscar valor (cddr amb)))
)
)

; Devuelve si la funcion se puede usar en un apply o no
(defun es-funcion (fn)
(member fn '(append + - * / < > eq atom null listp numberp length))
)