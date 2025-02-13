;; la funcion se encarga de tomar un dato y operarlo como factorial
(defun mostrar-factorial (n)
  (check-type n (integer 0 *))
  (loop with resultado = 1
        for i from 1 to n
        do (setf resultado (* resultado i))
        finally (format t "~&~D! = ~D" n resultado)))
