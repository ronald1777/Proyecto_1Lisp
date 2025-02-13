(defun mostrar-factorial (n)
  (check-type n (integer 0 *))
  (loop with resultado = 1
        for i from 1 to n
        do (setf resultado (* resultado i))
        finally (format t "~&~D! = ~D" n resultado)))