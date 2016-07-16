; Verifica si dos nodos a y b son iguales.
(defun nodos-iguales (a b)
  (or (equal a b) 
      (and (listp a) (listp b) (equal a (reverse b)))))

; Devuelve los nodos vecinos de un nodo en un grafo dado.
(defun vecinos (nodo grafo dicc)
  (valores (cadr (find (indice nodo dicc) grafo :key 'car :test 'nodos-iguales)) dicc))

; Dada una lista de nodos de grafo, devuelve sus valores acordes al diccionario
(defun valores (lista dicc)
  (list (valor (car lista) dicc) (valor (cadr lista) dicc)))

; Dado un indice del grafo devuelve el valor del nodo según el diccionario
(defun valor (indice dicc)
  (cadr (find indice dicc :key 'car :test 'nodos-iguales)))

; Dado un valor del nodo según el diccionario devuelve el indice del grafo
(defun indice (valor dicc)
  (car (find valor dicc :key 'cadr :test 'nodos-iguales)))

; Devuelve todas las posibles "expansiones" de una trayectoria dada agregando 
; los vecinos del primer nodo que aún no formen parte de la misma.
(defun expandir-trayectoria (trayectoria grafo dicc)
  (mapcar (lambda (vecino) (cons vecino trayectoria))
          (set-difference (vecinos (car trayectoria) grafo dicc) trayectoria)))

; Imprime un camino que conecta los nodos inicio y fin, o nil 
; en caso de no existir camino alguno.
(defun gps (i f grafo dicc &optional (tray (list (list i))))
  (imprimir-camino (camino i f grafo dicc)))

; Devuelve un camino que conecta los nodos inicio y fin, o nil 
; en caso de no existir camino alguno.
(defun camino (i f grafo dicc &optional (tray (list (list i))))
  (if (null tray) nil
      (if (nodos-iguales (caar tray) f)
          (reverse (car tray))
          (camino i f grafo dicc
               (append (expandir-trayectoria (car tray) grafo dicc) 
                     (cdr tray))))))

; Imprime un camino en un formato legible.
(defun imprimir-camino (camino)
  (if (> (length camino) 1)
      (imprimir-camino-desde camino 
                             (car (intersection (car camino) (cadr camino) 
                                                :test 'equal)) '-1)))

(defun imprimir-camino-desde (camino calle-anterior cuadras)
  (if camino 
      (if (member calle-anterior (cadr camino) :test 'equal)
          ; Sigue por la misma calle.
          (progn 
          (setq cuadras (+ cuadras '1))
          (imprimir-camino-desde (cdr camino) calle-anterior cuadras))
          ; Llega a destino.
          (if (null (cdr camino)) (progn  
            (setq cuadras (+ cuadras '1))
            (format t "Recorrer ~s cuadras por ~s hasta llegar a destino." cuadras calle-anterior))
               ; Agarra una calle nueva.
              (progn    
                (setq cuadras (+ cuadras '1))
                (format t "Recorrer ~s cuadras por ~s y doblar en ~s~%" cuadras calle-anterior 
                        (otra-calle calle-anterior (car camino)))
                (imprimir-camino-desde (cdr camino) 
                                       (otra-calle calle-anterior (car camino)) '0))))))

; Dada una calle y una esquina (una lista con dos calles) devuelve la calle de
; la esquina que no es la calle dada. Ejemplo: 
; (otra-calle-en-equina "A" ("B" "A")) -> "B"
(defun otra-calle (calle esquina)
  (find-if (lambda (x) (not (equal calle x))) esquina))
