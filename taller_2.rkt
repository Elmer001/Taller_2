#lang racket

;; ejercicio 1
(define (contar-positivos lst)
  (length (filter (lambda (x) (> x 0)) lst)))

(display "1. El numero de elementos positivos es: ")
(contar-positivos '(3 -2 7 0 -5 9))

;; ejercicio 2
(define (cuadrados-pares lst)
  (map (lambda (x) (* x x))
       (filter even? lst)))

(display "2. La lista con los elementos pares al cuadrado es: ")
(cuadrados-pares '(1 2 3 4 5 6 7 8))

;; ejercicio 3
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(display "3. El factorial del numero es: ")
(factorial 5)

;; ejercicio 4
(define (cubos lst)
  (map (lambda (x) (* x x x)) lst))

(display "4. La lista de elementos al cubo es: ")
(cubos '(2 3 4))

;; ejercicio 5
(define (suma-impares lst)
  (foldl (lambda (x acc) (+ x acc)) 0 (filter odd? lst)))

(display "5. La suma de todos los elementos impares es: ")
(suma-impares '(1 2 3 4 5 6 7))

;; ejercico 6
(define (tiene-negativos? lst)
  (ormap (lambda (x) (< x 0)) lst))

(display "6. Hay numeros negativos: ")
(tiene-negativos? '(5 9 -3 2))

;; ejercicio 7
(define (suma-acumulada lst)
  (define (aux restante acumulado suma)
    (if (null? restante)
        (reverse acumulado)
        (aux (cdr restante)
             (cons (+ suma (car restante)) acumulado)
             (+ suma (car restante)))))
  (aux lst '() 0))


(display "7. La suma acumulada es: ")
(suma-acumulada '(1 2 3 4))

;; ejercico 8
(define (concatenar-cadenas lst)
  (foldl string-append "" lst))

(display "8. La nueva cadena es: ")
(concatenar-cadenas '("Hola" " " "Mundo"))

;; ejercicio 9
(define (dobles-mayores-5 lst)
  (map (lambda (x) (* 2 x)) (filter (lambda (x) (> x 5)) lst)))

(display "9. Lista con los dobles de los numeros mayores a 5: ")
(dobles-mayores-5 '(3 6 8 2 10))

;; ejercicio 10
(define (invertir-lista lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

(display "10. Lista invertida: ")
(invertir-lista '(1 2 3 4))

;; ejercicio 11
(define (aplicar-a-lista f lst)
  (map f lst))

    ;; funcion a probar
(define (cuadrado x) (* x x))

(display "11. La nueva lista aplicando una funcion sobre la original: ")
(aplicar-a-lista cuadrado '(1 2 3 4))

;; ejercicio 12
(define (promedio-mayores-5 lst)
  (let* ((filtrados (filter (lambda (x) (> x 5)) lst))
         (count (length filtrados)))
    (if (> count 0)
        (/ (foldl + 0 filtrados) (exact->inexact count))
        0)))

(display "12. El promedio de los numeros mayores a 5 es: ")
(promedio-mayores-5 '(3 8 10 4 9 2 7))
