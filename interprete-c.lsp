; Funcion principal
(defun run (prg val &optional (mem nil))
(if (null prg) nil
 (cond
  ((eq (caar prg) 'int) (run (cdr prg) val (memoria prg mem)))
;  ((eq (caar prg) 'main) (ejecutar prg val mem))
  (T mem);'no-hay-main)
)))

; Carga de variables en memoria
(defun memoria (prg mem)
(if (null mem) (asignar-valor (mapcar (lambda(x)(cons x nil)) (cdar prg)))
(append mem (asignar-valor (mapcar (lambda(x)(cons x nil)) (cdar prg)))))
)

; Dada una lista de listas empareja variable con valor
(defun asignar-valor (list)
(if (null list) nil
 (if (eq (car (nth 1 list)) '=) 
  (cons (append (car list) (nth 2 list)) (asignar-valor (nthcdr 3 list)))
  (cons (car list) (asignar-valor (cdr list)))))
)
