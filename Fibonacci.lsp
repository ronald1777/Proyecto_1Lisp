(defun fibonacci (n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (t (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(format t "Ingrese la cantidad de términos de la secuencia de Fibonacci: ")
(let ((numTerminos (parse-integer (read-line))))
  (if (<= numTerminos 0)
      (format t "Ingrese un número mayor a 0.~%")
      (progn
        (format t "Secuencia de Fibonacci:~%")
        (dotimes (i numTerminos)
          (format t "~A " (fibonacci i))))))