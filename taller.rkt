#lang racket

(displayln "======================================")
;;contar positivos en una lista
(define (contar-positivos lista)
    (length (filter (lambda (x) (> x 0)) lista)))
(display "Contar positivos en lista: (3 -2 7 0 -5 9) => ")
(displayln (contar-positivos '(3 -2 7 0 -5 9)))
(displayln "----------------------")

;; cuadrados pares en una lista
(define (cuadrados-pares lista)
    (map (lambda (x) (* x x))
         (filter even? lista)))
(display "Cuadrados de números pares en lista: (1 2 3 4 5 6 7 8) => ")
(displayln (cuadrados-pares '(1 2 3 4 5 6 7 8)))
(displayln "----------------------")

;; factorial de un numero
(define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))
(display "Factorial de 5 => ")
(displayln (factorial 5))
(displayln "----------------------")

;; elevar cada numero al cubo en una lista
(define (elevar-al-cubo lista)
    (map (lambda (x) (expt x 3)) lista))
(display "Elevar al cubo cada número en lista: (2 3 4) => ")
(displayln (elevar-al-cubo '(2 3 4)))
(displayln "----------------------")

;; suma todos los numeros impares en una lista
(define (suma-impares lista)
    (foldl + 0 (filter odd? lista)))
(display "Suma de números impares en lista: (1 2 3 4 5 6 7) => ")
(displayln (suma-impares '(1 2 3 4 5 6 7)))
(displayln "----------------------")

;;determinar si la lista tiene numeros negativos
(define (tiene-negativos? lista)
    (not (null? (filter (lambda (x) (< x 0)) lista))))
(display "¿La lista tiene números negativos? (5 9 -3 2) => ")
(displayln (tiene-negativos? '(5 9 -3 2)))
(displayln "----------------------")

;;calcular suma acumulada de una lista
(define (suma-acumulada lista)
    (define sum (foldl (lambda (x acc) (+ x acc)) 0 lista))
    (define (helper lst acc)
        (if (null? lst)
            '()
            (cons (+ (car lst) acc)
                  (helper (cdr lst) (+ (car lst) acc)))))
    (helper lista 0))
(display "Suma acumulada de lista: (1 2 3 4) => ")
(displayln (suma-acumulada '(1 2 3 4)))
(displayln "----------------------")

;;concatenar cadenas en una lista
(define (concatenar-cadenas lista)
    (foldl string-append "" lista))
(display "Concatenar cadenas en lista: (\"mundo \" \"Hola \" ) => ")
(displayln (concatenar-cadenas '("mundo" "Hola " )))
(displayln "----------------------")

;;generar lista con el doble de los numeros mayores a 5
(define (doble-mayores-a-cinco lista)
    (map (lambda (x) (* 2 x))
         (filter (lambda (x) (> x 5)) lista)))
(display "Doble de números mayores a 5 en lista: (3 6 8 2 10) => ")
(displayln (doble-mayores-a-cinco '(3 6 8 2 10)))
(displayln "----------------------")

;;invertir una lista
(define (invertir-lista lista)
    (foldl (lambda (x acc) (cons x acc)) '() lista))
(display "Invertir lista: (1 2 3 4) => ")
(displayln (invertir-lista '(1 2 3 4)))
(displayln "----------------------")

;;funcion q recibe funcion cuadrado y lista, devuelve lista con elementos al cuadrado
(define (aplicar-cuadrado funcion lista)
    (map funcion lista))

    ;; definir la función cuadrado
(define (cuadrado x)
    (* x x))
(display "Aplicar función cuadrado a lista: (1 2 3 4) => ")
(displayln (aplicar-cuadrado cuadrado '(1 2 3 4)))
(displayln "----------------------")

;;promedio de numeros mayores a 5 en una lista
(define (promedio-mayores-a-cinco lista)
    (define mayores (filter (lambda (x) (> x 5)) lista))
    (if (null? mayores)
        0
        (/ (foldl + 0 mayores) (length mayores))))
(display "Promedio de números mayores a 5 en lista: (3 8 10 4 9 2 7) => ")
(displayln (promedio-mayores-a-cinco '(3 8 10 4 9 2 7)))
(displayln "----------------------")
