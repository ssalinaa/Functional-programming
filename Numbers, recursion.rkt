#lang racket

;Задача 1. Да се напише функция mymin, която приема два аргумента и връща по-малкия от тях.

(define (mymin a b)
  (if (< a b)
      a
      b))

(mymin 6 11)

;Задача 2. Да се дефинира функцията inside? x a b, която проверява дали числото x се намира в затворения интервал [a, b].

(define (inside? x a b)
  (if (and (>= x a) (<= x b))
      #t
      #f))

(inside? 5 6 8)

;Задача 3. Да се напише функция myfunc, която пресмята средно аритметично на квадратите на 2 числа.

(define (square x)
  (* x x))

(define (myfunc a b)
  (/ (+ (square a) (square b)) 2))

(myfunc 1 2)

; или

(define (average x y)
  (/ (+ x y) 2))

(define (func a b)
  (average (square a) (square b)))

(func 1 2)

;Задача 4. Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи. 
;Да се напише и итеративно решение.
;Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)

; 0 1 2 3 4 5  6 ...
; 1 1 2 3 5 8 13 ...

; fib(0) = 1
; fib(1) = 1
; fib(n) = fib(n-2) + fib(n-1), когато n>1

; Рекурсивно решение

(define (myfib n)
  (if (<= n 1)
       1
     (+ (myfib (- n 2)) (myfib (- n 1)))))

(myfib 6)

; Итеративно решение

(define (fib-iter n)
  (define (helper i previous current)
    (if (= i n)
        current
        (helper (+ i 1) current (+ previous current))))
 (helper 0 0 1))

(fib-iter 6)

;Задача 5. Факториел

; fact(0) = 1
; fact(n) = n * fact(n-1)

;Рекурсивно решение

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(fact 4)

; (fact 3) -> (* 3 (fact 2)) -> (* 3 (* 2 (fact 1))) -> (* 3 (* 2 (* 1 (fact 0)))) -> (* 3 (* 2 (* 1 1))) -> (* 3 (* 2 1)) -> (* 3 2) -> 6

; Итеративно решение

(define (fact-iter n)
  (define (helper k res)
    (if (= k 0)
        res
        (helper (- k 1) (* res k))))
  (helper n 1))

(fact-iter 4)

; (fact-iter 3) -> (helper 3 1) -> (helper 2 3) -> (helper 1 6) -> (helper 0 6) -> 6

; Задача 6.
; Write the "Hello, world" procedure.

(define (print-hello)
  (display "Hello,world!"))

(print-hello)

#|
Задача 7.
Define procedures that return:

the smaller of two whole numbers:
using a built-in procedure;
using if-else;
using guards;
the last digit of a number.
the quotient of the division of two whole numbers.
the quotient and remainder of the division of two whole numbers.
a whole number without its last digit.
the quotient and remainder of the division of two real numbers.
the quotient of the division of two real numbers.
the average of two whole numbers.
Test cases:

(= (my-min-built-in-p 5 6) 5)

(= (my-min-if -60 -15) -60)
(= (my-min-if 15 60) 15)
(= (my-min-if 60 15) 15)

(= (my-min-guard 15 60) 15)
(= (my-min-guard 60 15) 15)

(= (last-digit 154) 4)

(= (quotient-whole 64 2) 32)

(div-whole 154 17) ; 9 1/17

(= (remove-last-digit 154) 15)

(= (div-real 154 10) 15.4)

(= (round-two-dig pi) 3.14)

(= (average-whole 5 1542) 773.5)

|#

(define (my-min-built-in-p x y)
  (min x y))

(define (my-min-if x y)
  (if (< x y)
      x
      y))

(define (my-min-guard x y)
  (cond
    [(< x y) x]
    [else y]))

(define (last-digit n)
  (remainder n 10))

(define (quotient-whole n d)
  (quotient n d))

(define (div-whole n d)
  (/ n d))

(define (remove-last-digit n)
  (quotient n 10))

(define (div-real x y)
  (/ (* x 1.0) y))

(define (average-whole x y)
  (/ (+ x y) 2))

#|
Задача 8.
Define two procedures that return the sum of the cubes of two numbers:

using a built-in procedure;
without using built-in procedures.
Test cases:

(= (sum-cubes-pow 5 1) 126)
(= (sum-cubes-pow 10 50) 126000)

(= (sum-cubes-no-pow 5 1) 126)
(= (sum-cubes-no-pow 10 50) 126000)
|#

(define (sum-cubes-pow x y)
  (+ (expt x 3) (expt y 3)))

(define (sum-cubes-no-pow x y)
  (+ (* x x x) (* y y y)))

#|
Задача 9.
Define predicates that check:

whether two numbers are not equal:

in one line without using if-else;
using guards.

whether a whole number x is between two whole numbers - start and end:

in one line without using if-else;
with a local helper procedure that uses boolean operators.
Test cases:

(equal? (are-not-equal-one-line? 5 2) #t)
(equal? (are-not-equal-one-line? 5 5) #f)

(equal? (are-not-equal-guards? 5 2) #t)
(equal? (are-not-equal-guards? 5 5) #f)

(equal? (inside-one-line? 1 5 4) #t) ; start = 1, end = 5, x = 4
(equal? (inside-one-line? 5 1 4) #t)
(equal? (inside-one-line? 10 50 200) #f)
(equal? (inside-one-line? 10 50 1) #f)

(equal? (inside-boolean-ops? 1 5 4) #t)
(equal? (inside-boolean-ops? 5 1 4) #t)
(equal? (inside-boolean-ops? 10 50 200) #f)
(equal? (inside-boolean-ops? 10 50 1) #f)
|#

(define (are-not-equal-one-line? a b) (not (= a b)))

(define (are-not-equal-guards? a b)
  (if (= a b)
      #t
      #f))

(define (inside-one-line? start end x)
  (and (<= start x) (<= x end)))

(define (inside-boolean-ops? start end x)
  (define (between? a b c)
    (and (<= a c) (<= b c)))
  (between? start end x))

#|
Задача 10.
Define two procedures that return whether a number is even:

using if-else;
using guards.
Test cases:

(equal? (is-even-if 2) "Yes")
(equal? (is-even-if 15452) "Yes")
(equal? (is-even-if 321) "No")

(equal? (is-even-guards 2) "Yes")
(equal? (is-even-guards 15452) "Yes")
(equal? (is-even-guards 321) "No")
|#

(define (is-even-if n)
  (if (= (remainder n 2) 0)
      "Yes"
      "No"))

(is-even-if 2)

(define (is-even-guards n)
  (cond [(even? n) "Yes"]
        [else "No"]))

#|
Задача 11.
The Euclidean Algorithm for finding GCD(x,y) is as follows:

If x = 0 then GCD(x,y)=y.
If y = 0 then GCD(x,y)=x.
Write x in quotient remainder form (x = A⋅y + R). Find GCD(y,R) (since GCD(x,y)=GCD(y,R)).
Using the above algorithm, define a procedure for calculating the GCD of two numbers.

Implementation detail: Use guards!

Test cases:

(= (my-gcd 5 13) 1)
(= (my-gcd 13 1235) 13)
|#

(define (my-gcd x y)
  (cond
    [(= x 0) y]
    [(= y 0) x]
    [else (my-gcd y (remainder x y))]))

#|
Задача 12.
Define two predicates that check whether a year is a leap year.

The first should be called leap-year-one-line?. It should implement Method I using boolean logical operators.

The second should be called is-leap-year-guards?. It should implement Method II using guards.

Recap:

Method I: A year is leap year if it is a multiple of 400 or it is a multiple of 4 but not of 100.

Method II:
    if year is divisible by 400 then is_leap_year
    else if year is divisible by 100 then not_leap_year
    else if year is divisible by 4 then is_leap_year
    else not_leap_year 
Test cases:

(equal? (leap-year-one-line? 2020) #t)
(equal? (leap-year-one-line? 1988) #t)
(equal? (leap-year-one-line? 1600) #t)
(equal? (leap-year-one-line? 2400) #t)
(equal? (leap-year-one-line? 2023) #f)
(equal? (leap-year-one-line? 1700) #f)
(equal? (leap-year-one-line? 1800) #f)
(equal? (leap-year-one-line? 2100) #f)

(equal? (is-leap-year-guards? 2020) #t)
(equal? (is-leap-year-guards? 1988) #t)
(equal? (is-leap-year-guards? 1600) #t)
(equal? (is-leap-year-guards? 2400) #t)
(equal? (is-leap-year-guards? 2023) #f)
(equal? (is-leap-year-guards? 1700) #f)
(equal? (is-leap-year-guards? 1800) #f)
(equal? (is-leap-year-guards? 2100) #f)
|#

(define (leap-year-one-line? year)
  (and (divisible-by? year 4) (or (not (divisible-by? year 100)) (divisible-by? year 400))))

(define (divisible-by? n divisor)
  (= (remainder n divisor) 0))

(define (is-leap-year-guards? year)
  (cond
    [(and (divisible-by? year 4) (or (not (divisible-by? year 100) (divisible-by? year 400))))] #t
    [else #f]))

(define (divisible-by? n divisor)
  (= (remainder n divisor) 0))

#|
Задача 13.
John has a backpack. With it he can carry k kilograms. An item from the supermarket weighs w kilograms.

Define a predicate that accepts three numbers - c (number of products), k and w and returns whether John is capable of buying all the products in one trip to the supermarket.

Implementation detail: Use guards and make sure to throw errors!

Test cases:

(equal? (can-carry? 5 15 3) #t)
(equal? (can-carry? 1 5 4) #t)
(equal? (can-carry? 13 25 2) #f)
(equal? (can-carry? 24 104.44 21.12) #f)
(equal? (can-carry? 51 34.75 19.852) #f)
(equal? (can-carry? 42 95.11 0.51) #t)

; (can-carry? -13 25 2) ; error: The number of products was negative
; (can-carry? 13 -25 2) ; error: John's hosting capacity was negative
; (can-carry? 13 25 -2) ; error: The weight of a product was negative
|#

(define (can-carry? c k w)
  (cond
    [ (< c 0) (error "The number of products was negative")]
    [ (< k 0) (error "John's carrying capacity was negative")]
    [ (< w 0) (error "The weight of a product was negative")]
    [ (<= (* c w) k) #t]
    [else #f]))

#|
Задача 14.
Each day a plant is growing by up-speed meters. Each night that plants height decreases by down-speed meters due to the lack of sun light. Initially, plant is 0 meters tall. We plant the seed at the beginning of a day. We want to know how many days it will take for the plant to reach a certain height.

Test cases:

(= (growing-plant 5 2 5) 1)
(= (growing-plant 5 2 6) 2)
(= (growing-plant 10 9 4) 1)
(= (growing-plant 100 10 910) 10) ; up-speed=100, down-speed=10, desired-height=910
Explanations:

For up-speed = 100, down-speed = 10 and desired-height = 910, the output should be 10.

  After day 1   --> 100
  After night 1 --> 90
  After day 2   --> 190
  After night 2 --> 180
  After day 3   --> 280
  After night 3 --> 270
  After day 4   --> 370
  After night 4 --> 360
  After day 5   --> 460
  After night 5 --> 450
  After day 6   --> 550
  After night 6 --> 540
  After day 7   --> 640
  After night 7 --> 630
  After day 8   --> 730
  After night 8 --> 720
  After day 9   --> 820
  After night 9 --> 810
  After day 10  --> 910 
For up-speed = 10, down-speed = 9 and desired-height = 4, the output should be 1 because the plant reaches the desired height at day 1 (10 meters).

  After day 1 --> 10
|#

(define (growing-plant up-speed down-speed desired-height)
  (if (<= up-speed down-speed)
      (if (= desired-height 0)
          0
          -1)
      (ceiling (/ (- desired-height up-speed) (- up-speed down-speed)))))

#|
Задача 15.
A snail crawls up a column. During the day it crawls up some distance. During the night it sleeps, so it slides down for some distance (less than it crawls up during the day).

Your procedure accepts three arguments:

The height of the column;
The distance that the snail crawls during the day;
The distance that the snail slides down during the night.
Calculate number of days the snail will need to reach the top of the column.

Test cases:

(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)
|#

(define (snail height day-distance night-slide)
  (if (<= day-distance night-slide)
      -1
      (ceiling (/ (- height day-distance) (- day-distance night-slide)))))

#|
Задача 16.
Define a procedure that reverses a non-negative number by implementing a linearly iterative process.

Test cases:

(= (rev 1) 1)
(= (rev 123) 321)
(= (rev 987654321) 123456789)
|#

(define (rev n)
  (define (iter number reversed)
    (if (= number 0)
        reversed
        (iter (quotient number 10) (+ (* reversed 10) (remainder number 10)))))
  (iter n 0))