;; ================================================================================
;; Exercise 1.1
;;
;; Below is a sequence of expressions. What is the result printed by the
;; interpreter in response to each ex- pression? Assume that the sequence is to
;; be evaluated in the order in which it is presented.

;; 10
10

;; (+ 5 3 4)
12

;; (- 9 1)
8

;; (/ 6 2)
3

;; (+ (* 2 4) (- 4 6))
6

;; (define a 3)

;; (define b (+ a 1))

;; (+ a b (* a b))
19

;; (= a b)
#f

;; (if (and (> b a) (< b (* a b)))
;;     b
;;     a)
4

;; (cond ((= a 4) 6)
;;       ((= b 4) (+ 6 7 a))
;;       (else 25))
16

;; (+ 2 (if (> b a) b a))
6

;; (* (cond ((> a b) a)
;;          ((< a b) b)
;;          (else - 1))
;;    (+ a 1))
16
;; ================================================================================



;; ================================================================================
;; Exercise 1.2
;;
;; Translate the following expression into prefix form
;; 5 + 4 + (2 - (3 - (6 + 4/5)))
;; -----------------------------
;;        3(6 - 2)(2 - 7)

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
;; ================================================================================



;; ================================================================================
;; Exercise 1.3
;;
;; Define a procedure that takes three numbers as arguments and returns the sum
;; of the squares of the two larger numbers

(define (ex1.3 a b c)
  (cond ((and (>= a c) (>= b c)) (+ (square a) (square b)))
        ((and (>= a b) (>= c b)) (+ (square a) (square c)))
        ((and (>= b a) (>= c a)) (+ (square b) (square c)))))
;; ================================================================================



;; ================================================================================
;; Exercise 1.4
;;
;; Observe that our model of evaluation allows for combinations whose operators
;; are compound expres- sions. Use this observation to describe the behavior of
;; the following procedure:
;; (define (a-plus-abs-b a b)
;;   ((if (> b 0) + -) a b))

;; (a-plus-abs-b a b) = a + |b|
;; ================================================================================



;; ================================================================================
;; Exercise 1.5
;;
;; Ben Bitdiddle has invented a test to determine whether the interpreter he is
;; faced with is using applicative-order evaluation or normal-order evaluation.
;; He defines the following two procedures:
;;
;; (define (p) (p))
;; (define (test x y)
;;   (if (= x 0) 0 y))
;;
;; Then he evaluates the expression
;;
;; (test 0 (p))
;;
;; What behavior will Ben observe with an interpreter that uses
;; applicative-order evaluation? What behavior will he observe with an
;; interpreter that uses normal-order evaluation? Explain your answer. (Assume
;; that the evaluation rule for the special form if is the same whether the in-
;; terpreter is using normal or applicative order: The predi- cate expression is
;; evaluated first, and the result determines whether to evaluate the consequent
;; or the alternative ex- pression.)

;; Applicative: procedure will never return since evaluating (p) results in an
;; infinite recursion
;;
;; Normal: procedure returns 0 since id doesn't need to evaluate (p)
;; ================================================================================



;; ================================================================================
;; Exercise 1.6
;;
;; Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
;; "Why can't I just define it as an ordinary procedure in terms of cond?" she
;; asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she
;; defines a new version of if:
;;
;; (define (new-if predicate then-clause else-clause)
;;   (cond (predicate then-clause)
;;         (else else-clause)))
;;
;; Eva demonstrates the program for Alyssa:
;;
;; (new-if (= 2 3) 0 5)
;; 5
;; (new-if (= 2 3) 0 5)
;; 0
;;
;; Delighted, Alyssa uses new-if to rewrite the square-root program:
;;
;; (define (sqrt-iter guess x)
;;   (new-if (good-enough? guess x)
;;           guess
;;           (sqrt-iter (improve guess x) x)))
;;
;; What happens when Alyssa attempts to use this to compute square roots?
;; Explain.

;; Procedure call will result in an infinite recursion since new-if evaluates
;; both its arguments, one of which is (sqrt-iter ...)
;; ================================================================================



;; ================================================================================
;; Exercise 1.7
;;
;; The good-enough? test used in computing square roots will not be very
;; effective for finding the square roots of very small numbers. Also, in real
;; computers, arithmetic operations are almost always performed with limited
;; precision. This makes our test inadequate for very large numbers. Explain
;; these statements, with examples showing how the test fails for small and
;; large numbers. An alternative strategy for implementing good-enough? is to
;; watch how guess changes from one iteration to the next and to stop when the
;; change is a very small fraction of the guess. Design a square-root procedure
;; that uses this kind of end test. Does this work better for small and large
;; numbers?

;; Small numbers: if x is close to the tolerance in good-enough?, many
;; completely innaccurate guesses will be within that tolerance
;;
;; Large numbers: if x is very large, its difference with the next bigger
;; number or the previous smaller number will always be (much) bigger than the
;; tolerance and the procedure will never complete for most inputs

(define (sqrt x)
  (define (sqrt-iter guess)
    (define next-guess (improve guess))
    (if (good-enough? guess next-guess)
      guess
      (sqrt-iter next-guess)))

  (define (good-enough? prev-guess guess)
    (<= (abs (- (/ guess prev-guess) 1.0)) 0.0001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2))

  (sqrt-iter 1.0))
;; ================================================================================



;; ================================================================================
;; Exercise 1.8
;;
;; Newton's method for cube roots is based on the fact that if y is an
;; approximation to the cube root of x, then a better approximation is given by
;; the value
;;
;; x/(y^2) + 2y
;; ------------
;;      3
;;
;; Use this formula to implement a cube-root procedure analogous to the
;; square-root procedure. (In Section 1.3.4 we will see how to implement
;; Newton's method in general as an abstraction of these square-root and
;; cube-root procedures.)

(define (cbrt x)
  (define (cbrt-iter guess)
    (define next-guess (improve guess))
    (if (good-enough? guess next-guess)
      guess
      (cbrt-iter next-guess)))

  (define (good-enough? prev-guess guess)
    (<= (abs (- (/ guess prev-guess) 1.0)) 0.0001))

  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (cbrt-iter 1.0))
;; ================================================================================



;; ================================================================================
;; Exercise 1.9
;;
;; Each of the following two procedures defines a method for adding two positive
;; integers in terms of the procedures inc, which increments its argument by 1,
;; and dec, which decrements its argument by 1.
;;
;; (define (+ a b)
;;   (if (= a 0) b (inc (+ (dec a) b))))
;; (define (+ a b)
;;   (if (= a 0) b (+ (dec a) (inc b))))
;;
;; Using the substitution model, illustrate the process generated by each
;; procedure in evaluating (+ 4 5). Are these processes iterative or recursive?

;; First process, recursive:
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9
;;
;; Second process, iterative:
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9
;; ================================================================================



;; ================================================================================
;; Exercise 1.10
;;
;; The following procedure computes a mathematical function called Ackermann's
;; function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;; What are the values of the following expressions?
;; (A 1 10)
;; (A 2 4)
;; (A 3 3)

(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 32768)
65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 32768)
65536

;; Consider the following procedures, where A is the procedure defined above:
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

;; Give concise mathematical definitions for the functions computed by the
;; procedures f, g, and h for positive integer values of n. For example, (k n)
;; computes 5n^2.

;; (f n) = 2*n
;; (g n) = 2^n
;; (h n) = (2^n)^n
;; (k n) = 5n^2
;; ================================================================================



;; ================================================================================
;; Exercise 1.11
;;
;; A function f is defined by the rule that
;;
;;         | n if n < 3
;; f(n) = <|
;;         | f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3
;;
;; Write a procedure that computes f by means of a recursive process. Write a
;; procedure that computes f by means of an iterative process.

(define (f n) (if (< n 3) n (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f n)
  (define (iter a b c count)
    (if (= count n)
        a
        (iter b c (+ c (* 2 b) (* 3 a)) (+ count 1))))
  (iter 0 1 2 0))
;; ================================================================================



;; ================================================================================
;; Exercise 1.12
;;
;; The following pattern of numbers is called Pascal's triangle.
;;
;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1
;;    ...
;;
;; The numbers at the edge of the triangle are all 1, and each number inside
;; the triangle is the sum of the two numbers above it. Write a procedure that
;; computes elements of Pascal's triangle by means of a recursive process.

(define (pascal row el)
  (cond ((or (> el row) (< el 1)) 0)
        ((or (= el row) (= el 1)) 1)
        (else (+ (pascal (- row 1) (- el 1)) (pascal (- row 1) el)))))
;; ================================================================================



;; ================================================================================
;; Exercise 1.13
;;
;; Prove that Fib(n) is the closest integer to (phi^n)/sqrt(5), where phi = (1
;; + sqrt(5))/2. Hint: Let psi = (1 - sqrt(5))/2. Use induction and the
;; definition of the Fibonacci numbers (see Section 1.2.2) to prove that Fib(n)
;; = (phi^n - psi^n)/sqrt(5)

;; phi^2 = phi + 1
;; psi^2 = psi + 1
;;
;; Fib(n) = (phi^n - psi^n) / sqrt(5)
;;
;; Fib(0) = (phi^0 - psi^0) / sqrt(5) = (1 - 1) / sqrt(5) = 0 / sqrt(5) = 0
;; Fib(1) = (phi^1 - psi^1) / sqrt(5)
;;    = ((1 + sqrt(5))/2 - (1 - sqrt(5))/2) / sqrt(5)
;;    = (1 + sqrt(5) - 1 + sqrt(5)) / (2 * sqrt(5))
;;    = (2 * sqrt(5)) / (2 * sqrt(5))
;;    = 1
;; Fib(n) = Fib(n - 1) + Fib(n - 2)
;;    = (phi^(n-1) - psi^(n-1)) / sqrt(5) + (phi^(n-2) - psi^(n-2)) / sqrt(5)
;;    = (phi^(n-1) + phi^(n-2) - psi^(n-1) - psi^(n-2)) / sqrt(5)
;;    = (phi^(n-2) * (phi + 1) - psi^(n-2) * (psi + 1)) / sqrt(5)
;;    = (phi^(n-2) * phi^2 - psi^(n-2) * psi^2) / sqrt(5)
;;    = (phi^n - psi^n) / sqrt(5)
;;    = Fib(n)
;;
;; |psi^n / sqrt(5)| < 0.5
;;    => Fib(n) is the closest int to:
;; Fib(n) + psi^n / sqrt(5)
;;    = (phi^n - psi^n) / sqrt(5) + psi^n / sqrt(5)
;;    = phi^n / sqrt(5) - psi^n / sqrt(5) + psi^n / sqrt(5)
;;    = phi^n / sqrt(5)
;; ================================================================================



;; ================================================================================
;; Exercise 1.14
;;
;; Draw the tree illustrating the process generated by the count-change
;; procedure of Section 1.2.2 in making change for 11 cents. What are the
;; orders of growth of the space and number of steps used by this process as
;; the amount to be changed increases?

;; (count-change 11)
;; (cc 11 5)
;; (cc 11 4) (cc -39 5)
;; (cc 11 3) (cc -14 4) 0
;; (cc 11 2) (cc 1 3) 0 -
;; (cc 11 1) (cc 6 2) (cc 1 2) (cc -9 3) - -
;; (cc 11 0) (cc 10 1) (cc 6 1) (cc 1 2) (cc 1 1) (cc -4 2) 0 - -
;; 0 (cc 10 0) (cc 9 1) (cc 6 0) (cc 5 1) (cc 1 1) (cc -4 2) (cc 1 0) (cc 0 1) 0 - - -
;; - 0 (cc 9 0) (cc 8 1) 0 (cc 5 0) (cc 4 1) (cc 1 0) (cc 0 1) 0 0 1 - - - -
;; - - 0 (cc 8 0) (cc 7 1) - 0 (cc 4 0) (cc 3 1) 0 1 - - - - - - -
;; - - - 0 (cc 7 0) (cc 6 1) - - 0 (cc 3 0) (cc 2 1) - - - - - - - - -
;; - - - - 0 (cc 6 0) (cc 5 1) - - - 0 (cc 2 0) (cc 1 1) - - - - - - - - -
;; - - - - - 0 (cc 5 0) (cc 4 1) - - - - 0 (cc 1 0) (cc 0 1) - - - - - - - - -
;; - - - - - - 0 (cc 4 0) (cc 3 1) - - - - - 0 1 - - - - - - - - -
;; - - - - - - - 0 (cc 3 0) (cc 2 1) - - - - - - - - - - - - - - - -
;; - - - - - - - - 0 (cc 2 0) (cc 1 1) - - - - - - - - - - - - - - - -
;; - - - - - - - - - 0 (cc 1 0) (cc 0 1) - - - - - - - - - - - - - - - -
;; - - - - - - - - - - 0 1 - - - - - - - - - - - - - - - -
;;
;; Space: linear
;; Time: exponential
;; ================================================================================



;; ================================================================================
;; Exercise 1.15
;;
;; Thee sine of an angle (specified in radians) can be computed by making use of
;; the approximation sin x ~= x if x is sufficiently small, and the trigonometric
;; identity
;;
;; sin x = 3 * sin x/3 - 4 * (sin x/3)^3
;;
;; to reduce the size of the argument of sin. (For purposes of this exercise an
;; angle is considered "sufficiently small" if its magnitude is not greater than
;; 0.1 radians.) These ideas are incorporated in the following procedures:

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

;; a. How many times is the procedure p applied when (sine 12.15) is evaluated?
;; b. What is the order of growth in space and number of steps (as a function
;;    of a) used by the process generated by the sine procedure when (sine a)
;;    is evaluated?

;; a. 5 times
;; b. Both space and time grow logarithmically
;; ================================================================================



;; ================================================================================
;; Exercise 1.16
;;
;; Design a procedure that evolves an iterative exponentiation process that
;; uses successive squaring and uses a logarithmic number of steps, as does
;; fast-expt. (Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2),
;; keep, along with the exponent n and the base b, an additional state variable
;; a, and define the state transformation in such a way that the product ab^n is
;; unchanged from state to state. At the beginning of the process a is taken to
;; be 1, and the answer is given by the value of a at the end of the process.
;; In general, the technique of defining an invariant quantity that remains
;; unchanged from state to state is a powerful way to think about the design of
;; iterative algorithms.)

(define (fast-exp b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))
;; ================================================================================



;; ================================================================================
;; Exercise 1.17
;;
;; The exponentiation algorithms in this section are based on performing
;; exponentiation by means of repeated multiplication. In a similar way, one
;; can perform integer multiplication by means of repeated addition. The
;; following multiplication procedure (in which it is assumed that our language
;; can only add, not multiply) is analogous to the expt procedure:

(define (* a b)
  (if (= b 0)
    0
    (+ a (* a (- b 1)))))

;; This algorithm takes a number of steps that is linear in b. Now suppose we
;; include, together with addition, operations double, which doubles an
;; integer, and halve, which divides an (even) integer by 2. Using these,
;; design a multiplication procedure analogous to fast-expt that uses a
;; logarithmic number of steps.

(define (* a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (double (* a (halve b))))
        (else (+ a (* a (- b 1))))))
;; ================================================================================



;; ================================================================================
;; Exercise 1.18
;;
;; Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure
;; that generates an iterative process for multiplying two integers in terms of
;; adding, doubling, and halving and uses a logarithmic number of steps.

(define (* a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (iter acc a b)
    (cond ((= b 0) acc)
          ((even? b) (iter acc (double a) (halve b)))
          (else (iter (+ acc a) a (- b 1)))))

  (iter 0 a b))
;; ================================================================================



;; ================================================================================
;; Exercise 1.19
;;
;; There is a clever algorithm for computing the Fibonacci numbers in a
;; logarithmic number of steps. Recall the transformation of the state
;; variables a and b in the fib-iter process of Section 1.2.2: a <- a + b and b
;; <- a. Call this transformation T, and observe that applying T over and over
;; again n times, starting with 1 and 0, produces the pair Fib(n + 1) and
;; Fib(n). In other words, the Fibonacci numbers are produced by applying T^n,
;; the nth power of the transformationT , starting with the pair (1, 0). Now
;; consider T to be the special case of p = 0 and q = 1 in a family of
;; transformations T_pq, where T_pq transforms the pair (a, b) according to a
;; <- bq + aq + ap and b <- bp + aq. Show that if we apply such a
;; transformation T_pq twice, the effect is the same as using a single
;; transformation T_p'q' of the same form, and compute p' and q' in terms of p
;; and q. This gives us an explicit way to square these transformations, and
;; thus we can compute T^n using successive squaring, as in the fast-expt
;; procedure. Put this all together to com- plete the following procedure,
;; which runs in a logarithmic number of steps:

;; T_pq:
;; a <- bq + aq + ap
;; b <- bp + aq
;;
;; T_p'q':
;; a <- (2pq + qq)b + (2pq + qq)a + (qq + pp)a
;; b <- (pp + qq)b + (2pq + qq)a
;;
;; p' = p^2 + q^2
;; q' = 2pq + q^2

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
;; ================================================================================



;; ================================================================================
;; Exercise 1.20
;;
;; The process that a procedure generates is of course dependent on the rules
;; used by the interpreter. As an example, consider the iterative gcd procedure
;; given above. Suppose we were to interpret this procedure using normal-order
;; evaluation, as discussed in Section 1.1.5. (The normal-order-evaluation rule
;; for if is described in Exercise 1.5.) Using the substitution method (for
;; normal order), illustrate the process generated in evaluating (gcd 206 40)
;; and indicate the remainder operations that are actually performed. How many
;; remainder operations are actually performed in the normal-order evaluation
;; of (gcd 206 40)? In the applicative-order evaluation?

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Applicative order
;; Results in 4 remainder operations
(gcd 206 40)
(gcd 40 6)
(gcd 6 4)
(gcd 4 2)
(gcd 2 0)
2

;; Normal order
;; Results in 18 remainder operations, assuming that we evaluate b in every
;; call to (gcd a b) to calculate (= b 0) (this accounts for 14 remainder
;; operations) and then pass the original unevaluated expression for b to (gcd
;; b (remainder a b)). In the end we need to evaluate a which accounts for the
;; remaining 4 remainder operations
(gcd 206 40)
(gcd 40
     (remainder 206 40))
(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40))))
(gcd (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40 (remainder 206 40)))))
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
2
;; ================================================================================



;; ================================================================================
;; Exercise 1.21
;;
;; Use the smallest-divisor procedure to find the smallest divisor of each of
;; the following numbers: 199, 1999, 19999.

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(smallest-divisor 199)
199
(smallest-divisor 1999) ;; was probably supposed to take a long time in the 80's
1999
(smallest-divisor 19999)
7
;; ================================================================================



;; ================================================================================
;; Exercise 1.22
;;
;; Most Lisp implementations include a primitive called runtime that returns an
;; integer that specifies the amount of time the system has been running
;; (measured, for example, in microseconds). The following timed-prime-test
;; procedure, when called with an integer n, prints n and checks to see if n is
;; prime. If n is prime, the procedure prints three asterisks followed by the
;; amount of time used in performing the test.

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  ;; (= n (smallest-divisor n))) ;; copied from the book, returns true for 1
  (and (not (= 1 n)) (= n (smallest-divisor n))))

;; (define runtime real-time-clock) ;; for mit-scheme
(define (runtime) (truncate (* 1000000 (real-time)))) ;; for gambit-c

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; Using this procedure, write a procedure search-for-primes that checks the
;; primality of consecutive odd integers in a specified range. Use your
;; procedure to find the three smallest primes larger than 1000; larger than
;; 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to
;; test each prime. Since the testing algorithm has order of growth of
;; Theta(sqrt(n)), you should expect that testing for primes around 10,000
;; should take about sqrt(10) times as long as testing for primes around 1000.
;; Do your timing data bear this out? How well do the data for 100,000 and
;; 1,000,000 support the Theta(sqrt(n)) prediction? Is your result compatible
;; with the notion that programs on your machine run in time proportional to
;; the number of steps required for the computation?

(define (search-for-primes lo hi)
  (cond ((> lo hi))
        ((odd? lo) (timed-prime-test lo) (search-for-primes (+ 2 lo) hi))
        (else (search-for-primes (+ 1 lo) hi))))

(search-for-primes 1000 1050)
;; ...
;; 1009 *** 31.
;; 1011
;; 1013 *** 30.
;; 1015
;; 1017
;; 1019 *** 29.
;; ...

(search-for-primes 10000 10050)
;; ...
;; 10007 *** 86.
;; 10009 *** 147.
;; ...
;; 10037 *** 89.
;; ...

(search-for-primes 100000 100050)
;; 100001
;; 100003 *** 758.
;; ...
;; 100019 *** 254.
;; ...
;; 100043 *** 353.
;; ...

(search-for-primes 1000000 1000050)
;; 1000001
;; 1000003 *** 791.
;; ...
;; 1000033 *** 906.
;; 1000035
;; 1000037 *** 865.
;; ...

;; Yes, ignoring the occasional spikes, the timing results are increased
;; roughly sqrt(10) times each time n is increased 10 times
;; ================================================================================



;; ================================================================================
;; Exercise 1.23
;;
;; The smallest-divisor procedure shown at the start of this section does lots
;; of needless testing: After it checks to see if the number is divisible by 2
;; there is no point in checking to see if it is divisible by any larger even
;; numbers. This suggests that the values used for test-divisor should not be 2,
;; 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, ...
;; To implement this change, define a procedure next that returns 3 if its
;; input is equal to 2 and otherwise returns its input plus 2. Modify the
;; smallest-divisor procedure to use (next test-divisor) instead of (+
;; test-divisor 1). With timed-prime-test incorporating this modified version
;; of smallest-divisor, run the test for each of the 12 primes found in
;; Exercise 1.22. Since this modification halves the number of test steps, you
;; should expect it to run about twice as fast. Is this expectation confirmed?
;; If not, what is the observed ratio of the speeds of the two algorithms, and
;; how do you explain the fact that it is different from 2?

(define (next x) (if (= x 2) 3 (+ x 2)))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

;; The new version is, on average, slightly faster, but not twice as fast.
;; Possibly because remainder operation is faster with even integers so
;; find-divisor spent the bulk of its execution time on odd integers anyway.
;; ================================================================================



;; ================================================================================
;; Exercise 1.24
;;
;; Modify the timed-prime-test procedure of Exercise 1.22 to use fast-prime?
;; (the Fermat method), and test each of the 12 primes you found in that
;; exercise. Since the Fermat test has Theta(log(n)) growth, how would you
;; expect the time to test primes near 1,000,000 to compare with the time
;; needed to test primes near 1000? Do your data bear this out? Can you explain
;; any discrepancy you find?

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
           (square (expmod base (/ exp 2) m))
           m))
        (else
          (remainder
            (* base (expmod base (- exp 1) m))
            m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; (define runtime real-time-clock) ;; for mit-scheme
(define (runtime) (truncate (* 1000000 (real-time)))) ;; for gambit-c

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(search-for-primes 1000 1050)      
;; ...
;; 1009 *** 53.
;; 1011
;; 1013 *** 61.
;; 1015
;; 1017
;; 1019 *** 63.
;; ...

(search-for-primes 10000 10050)
;; ...
;; 10007 *** 73.
;; 10009 *** 79.
;; ...
;; 10037 *** 72.
;; ...

(search-for-primes 100000 100050)
;; 100001
;; 100003 *** 89.
;; ...
;; 100019 *** 87.
;; ...
;; 100043 *** 84.
;; ...

(search-for-primes 1000000 1000050)
;; 1000001
;; 1000003 *** 88.
;; ...
;; 1000033 *** 83.
;; 1000035
;; 1000037 *** 85.
;; ...

;; On average, mutiplying n by a constant factor increases execution time by a
;; constant amount, as expected of log(n). Some discrepancies may be due to the
;; usage of random since it may generate both very large and very small numbers
;; ================================================================================



;; ================================================================================
;; Exercise 1.25
;;
;; Alyssa P. Hacker complains that we went to a lot of extra work in writing
;; expmod. After all, she says, since we already know how to compute
;; exponentials, we could have simply written

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; Is she correct? Would this procedure serve as well for our fast prime
;; tester? Explain.

;; Both procedures run in log(n) time, but the original expmod never deals with
;; numbers larger than m, whereas the new one finds the exponent first, which
;; can and, in our prime tester, will be huge. It will run slower and/or,
;; depending, on the integer representation used, may give incorrect results.
;; ================================================================================



;; ================================================================================
;; Exercise 1.26
;;
;; Louis Reasoner is having great difficulty doing Exercise 1.24. His
;; fast-prime? test seems to run more slowly than his prime? test. Louis calls
;; his friend Eva Lu Ator over to help. When they examine Louis's code, they
;; find that he has rewritten the expmod procedure to use an explicit
;; multiplication, rather than calling square:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base
                        (expmod base (- exp 1) m))
                     m))))

;; "I don't see what difference that could make," says Louis. "I do." says Eva.
;; "By writing the procedure like that, you have transformed the Theta(log(n))
;; process into a Theta(n) process." Explain.

;; In the original expmod, log(n) is achieved by dividing the exponent by 2 in
;; the second cond branch. This means a single step reduces the problem size by
;; half which is exactly what makes it log(n). But after switching to explicit
;; multiplication, we evaluate the halved problem twice, thus negating the
;; log(n) benefits and going back to linear time.
;; ================================================================================



;; ================================================================================
;; Exercise 1.27
;;
;; Demonstrate that the Carmichael numbers listed in Footnote 1.47 really do
;; fool the Fermat test. That is, write a procedure that takes an integer n and
;; tests whether a^n is congruent to a modulo n for every a < n, and try your
;; procedure on the given Carmichael numbers.

(define (prime-or-carmichael? n)
  (define (iter a)
    (cond ((>= a n) #t)
          ((= (expmod a n n) a) (iter (+ a 1)))
          (else #f)))
    (iter 1))
;; ================================================================================



;; ================================================================================
;; Exercise 1.28
;;
;; One variant of the Fermat test that cannot be fooled is called the
;; Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an alternate
;; form of Fermat's Little Theorem, which states that if n is a prime number
;; and a is any positive integer less than n, then a raised to the (n-1)-st
;; power is congruent to 1 modulo n. To test the primality of a number n by the
;; Miller-Rabin test, we pick a random number a < n and raise a to the (n-1)-st
;; power modulo n using the expmod procedure. However, whenever we perform the
;; squaring step in expmod, we check to see if we have discovered a "nontrivial
;; square root of 1 modulo n," that is, a number not equal to 1 or n-1 whose
;; square is equal to 1 modulo n. It is possible to prove that if such a
;; nontrivial square root of 1 exists, then n is not prime. It is also possible
;; to prove that if n is an odd number that is not prime, then, for at least
;; half the numbers a < n, computing a^(n-1) in this way will reveal a
;; nontrivial square root of 1 modulo n. (This is why the Miller-Rabin test
;; cannot be fooled.) Modify the expmod procedure to signal if it discovers a
;; nontrivial square root of 1, and use this to implement the Miller-Rabin test
;; with a procedure analogous to fermat-test. Check your procedure by testing
;; various known primes and non-primes. Hint: One convenient way to make expmod
;; signal is to have it return 0.

(define (expmod base exp m)
  (define (half x)
    (cond ((= 0 x) 0)
          ((and (not (= 1 x))
                (not (= (- m 1) x))
                (= (remainder m (square x)) 1)) 0)
          (else (remainder (square x) m))))

  (define (prev x)
    (if (= 0 x) 0 (remainder (* base x) m)))

  (cond ((= exp 0) 1)
        ((even? exp) (half (expmod base (/ exp 2) m)))
        (else (prev (expmod base (- exp 1) m)))))

(define (miller-rabin-test n)
  (= 1 (expmod (+ 1 (random-integer (- n 1)))
               (- n 1)
               n)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))
;; ================================================================================



;; ================================================================================
;; Exercise 1.29
;;
;; Simpson's Rule is a more accurate method of numerical integration than the
;; method illustrated above. Using Simpson's Rule, the integral of a function f
;; between a and b is approximated as
;;
;;  h
;; --- (y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n-2) + 4y_(n-1) + y_n),
;;  3
;;
;; where h = (b - a)/n, for some even integer n, and y_k = f(a + kh).
;; (Increasing n increases the accuracy of the approximation.) Define a
;; procedure that takes as arguments f, a, b, and n and returns the value of
;; the integral, computed using Simpson's Rule. Use your procedure to integrate
;; cube between 0 and 1 (with n = 100 and n = 1000), and compare the results to
;; those of the integral procedure shown above.

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (simpson f a b n)
  (define (term k)
    (* (f (+ a (* k (/ (- b a) n))))
       (cond ((= 0 k) 1)
             ((= n k) 1)
             ((odd? k) 4)
             ((even? k) 2))))
  (define (inc x) (+ x 1))
  (/ (* (- b a) (sum term 0 inc n))
     (* 3.0 n)))
;; ================================================================================



;; ================================================================================
;; Exercise 1.30
;;
;; The sum procedure above generates a linear recursion. The procedure can be
;; rewritten so that the sum is performed iteratively. Show how to do this by
;; filling in the missing expressions in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))
;; ================================================================================



;; ================================================================================
;; Exercise 1.31
;;
;; a. The sum procedure is only the simplest of a vast number of similar
;; abstractions that can be captured as higher-order procedures. Write an
;; analogous procedure called product that returns the product of the values of
;; a function at points over a given range. Show how to define factorial in
;; terms of product. Also use product to compute approximations to Pi using the
;; formula
;;
;; Pi   2 * 4 * 4 * 6 * 6 * 8 * ...
;; -- = ---------------------------
;; 4    3 * 3 * 5 * 5 * 7 * 7 * ...
;;
;; b. If your product procedure generates a recursive process, write one that
;; generates an iterative process. If it generates an iterative process, write
;; one that generates a recursive process.

;; Recursive
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

;; Iterative
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (define (id x) x)
  (define (inc x) (+ x 1))
  (product id 1 inc n))

(define (pi-approx n)
  (define (inc x) (+ x 1))
  (define (numer i) (+ 2 (+ i (remainder i 2))))
  (define (denom i) (+ 3 (- i (remainder i 2))))
  (define (term i) (/ (numer i) (denom i)))
  (* 4.0 (product term 0 inc (- n 1))))
;; ================================================================================



;; ================================================================================
;; Exercise 1.32
;;
;; a. Show that sum and product (Exercise 1.31) are both special cases of a
;; still more general notion called accumulate that combines a collection of
;; terms, using some general accumulation function:
;;
;; (accumulate combiner null-value term a next b)
;;
;; accumulate takes as arguments the same term and range specifications as sum
;; and product, together with a combiner procedure (of two arguments) that
;; specifies how the current term is to be combined with the accumulation of
;; the preceding terms and a null-value that specifies what base value to use
;; when the terms run out. Write accumulate and show how sum and product can
;; both be defined as simple calls to accumulate.
;;
;; b. If your accumulate procedure generates a recursive process, write one
;; that generates an iterative process. If it generates an iterative process,
;; write one that generates a recursive process.

;; Recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner
                          null-value
                          term
                          (next a)
                          next b))))

;; Iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b) (accumulate + 0 term a next b))

(define (product term a next b) (accumulate * 1 term a next b))
;; ================================================================================



;; ================================================================================
;; Exercise 1.33
;;
;; You can obtain an even more general version of accumulate (Exercise 1.32) by
;; introducing the notion of a filter on the terms to be combined. That is,
;; combine only those terms derived from values in the range that satisfy a
;; specified condition. The resulting filtered-accumulate abstraction takes the
;; same arguments as accumulate, together with an additional predicate of one
;; argument that specifies the filter. Write filtered-accumulate as a procedure.
;; Show how to express the following using filtered-accumulate:
;;
;; a. the sum of the squares of the prime numbers in the interval a to b
;; (assuming that you have a prime? predicate already written)
;; b. the product of all the positive integers less than n that are relatively
;; prime to n (i.e., all positive integers i < n such that GCD(i, n) = 1).

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (prime-square-sum a b)
  (define (inc x) (+ x 1))
  (filtered-accumulate prime? + 0 square a inc b))

(define (relative-prime-prod n)
  (define (id x) x)
  (define (inc x) (+ x 1))
  (define (filter i) (= 1 (gcd n i)))
  (filtered-accumulate filter * 1 id 2 inc (- n 1)))
;; ================================================================================



;; ================================================================================
;; Exercise 1.34
;;
;; Suppose we define the procedure

(define (f g) (g 2))

;; Then we have

(f square)
4

(f (lambda (z) (* z (+ z 1))))
6

;; What happens if we (perversely) ask the interpreter to evaluate the
;; combination (f f)? Explain.

(f f)
(f 2)
(2 2)
;; error: operator is not a procedure
;; ================================================================================



;; ================================================================================
;; Exercise 1.35
;;
;; Show that the golden ratio phi (Section 1.2.2) is a fixed point of the
;; transformation x -> 1 + 1/x, and use this fact to compute phi by means of
;; the fixed-point procedure.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

;; phi^2 = phi + 1
;; phi = (phi + 1) / phi = 1 + 1/phi

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
;; ================================================================================



;; ================================================================================
;; Exercise 1.36
;;
;; Modify fixed-point so that it prints the sequence of approximations it
;; generates, using the newline and display primitives shown in Exercise 1.22.
;; Then find a solution to x^x = 1000 by finding a fixed point of x ->
;; log(1000)/log(x). (Use Scheme's primitive log procedure, which computes
;; natural logarithms.) Compare the number of steps this takes with and without
;; average damping. (Note that you cannot start fixed-point with a guess of 1,
;; as this would cause division by log(1) = 0.)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (average-damp f)
  (define (average a b) (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

;; No average damp, 34 iterations:
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

;; Average damp, 9 iterations
(fixed-point (average-damp (lambda (x) (/ (log 1000) (log x)))) 2.0)
;; ================================================================================



;; ================================================================================
;; Exercise 1.37
;;
;; a. An infinite continued fraction is an expression of the form
;;
;;              N_1
;;     ---------------------
;;                 N_2
;; f = D_1 + ---------------
;;                    N_3
;;           D_2 + ---------
;;                 D_3 + ...
;;
;;
;; As an example, one can show that the infinite continued fraction expansion
;; with the N_i and the D_i all equal to 1 produces 1/phi, where phi is the
;; golden ratio (described in Section 1.2.2). One way to approximate an
;; infinite continued fraction is to truncate the expansion after a given
;; number of terms. Such a truncation---a so-called k-term finite continued
;; fraction---has the form
;;
;;        N_1
;; -----------------
;;           N_2
;; D_1 + -----------
;;      .
;;       .      N_k
;;        .  + -----
;;              D_k
;;
;; Suppose that n and d are procedures of one argument (the term index i) that
;; return the N_i and D_i of the terms of the continued fraction. Define a
;; procedure cont-frac such that evaluating (cont-frac n d k) computes the
;; value of the k-term finite continued fraction. Check your procedure by
;; approximating 1/phi using

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)

;; for successive values of k. How large must you make k in order to get an
;; approximation that is accurate to 4 decimal places?
;;
;; b. If your cont-frac procedure generates a recursive process, write one that
;; generates an iterative process. If it generates an iterative process, write
;; one that generates a recursive process.

;; Recursive
(define (cont-frac n d k)
  (define (recurse i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (recurse (+ i 1))))))
  (recurse 1))

;; Iterative
(define (cont-frac n d k)
  (define (iter i result)
    (if (< i 1)
      result
      (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

;; k = 12 is enough for 4 decimals of precions
;; ================================================================================



;; ================================================================================
;; Exercise 1.38
;;
;; In 1737, the Swiss mathematician Leonhard Euler published a memoir De
;; Fractionibus Continuis, which included a continued fraction expansion for e
;; - 2, where e is the base of the natural logarithms. In this fraction, the
;; N_i are all 1, and the D_i are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8,
;; ... . Write a program that uses your cont-frac procedure from Exercise 1.37
;; to approximate e, based on Euler's expansion.

(define (e-approx k)
  (+ 2 (cont-frac
         (lambda (i) 1.0)
         (lambda (i)
           (let ((m (remainder (- i 1) 3)))
             (if (even? m)
               1
               (* 2 (+ 1 (/ (- (- i 1) m) 3))))))
         k)))
;; ================================================================================



;; ================================================================================
;; Exercise 1.39
;;
;; A continued fraction representation of the tangent function was published in
;; 1770 by the German mathematician J.H. Lambert:
;;
;;                   x
;;         ---------------------
;;                     x^2
;; tan x = 1  -  ---------------
;;                        x^2
;;               3  -  ---------
;;                     5  -  ...
;;
;; where x is in radians. Define a procedure (tan-cf x k) that computes an
;; approximation to the tangent function based on Lambert's formula. k
;; specifies the number of terms to compute, as in Exercise 1.37.

(define (tan-cf x k)
  (define (n i) (if (= i 1) x (square x)))
  (define (d i) (- (* i 2) 1))
  (define (iter i result)
    (if (< i 1)
      result
      (iter (- i 1) (/ (n i) (- (d i) result)))))
  (iter k 0.0))
;; ================================================================================



;; ================================================================================
;; Exercise 1.40
;;
;; Define a procedure cubic that can be used together with the newtons-method
;; procedure in expressions of the form
;;
;; (newtons-method (cubic a b c) 1)
;;
;; to approximate zeros of the cubic x^3 + ax^2 + bx + c

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c) (lambda (x) (+ (* x x x) (* a (square x)) (* b x) c)))
;; ================================================================================



;; ================================================================================
;; Exercise 1.41
;;
;; Define a procedure double that takes a procedure of one argument as argument
;; and returns a procedure that applies the original procedure twice. For
;; example, if inc is a procedure that adds 1 to its argument, then (double
;; inc) should be a procedure that adds 2. What value is returned by
;;
;; (((double (double double)) inc) 5)

(define (double f) (lambda (x) (f (f x))))

(((double (double double)) inc) 5)
(((double (lambda (f) (double (double f)))) inc) 5)
(((lambda (g) ((lambda (f) (double (double f))) ((lambda (f) (double (double f))) g))) inc) 5)
(((lambda (f) (double (double f))) ((lambda (f) (double (double f))) inc)) 5)
((double (double (double (double inc)))) 5)
21
;; ================================================================================



;; ================================================================================
;; Exercise 1.42
;;
;; Let f and g be two one-argument functions. The composition f after g is
;; defined to be the function x -> f(g(x)). Define a procedure compose that
;; implements composition. For example, if inc is a procedure that adds 1 to
;; its argument,

((compose square inc) 6)
49

(define (compose f g) (lambda (x) (f (g x))))
;; ================================================================================



;; ================================================================================
;; Exercise 1.43
;;
;; If f is a numerical function and n is a positive integer, then we can form
;; the nth repeated application of f, which is defined to be the function whose
;; value at x is f(f(...(f(x))...)). For example, if f is the function x -> x +
;; 1, then the nth repeated application of f is the function x -> x + n. If f
;; is the operation of squaring a number, then the nth repeated application of
;; f is the function that raises its argument to the 2^n-th power. Write a
;; procedure that takes as inputs a procedure that computes f and a positive
;; integer n and returns the procedure that computes the nth repeated
;; application of f. Your procedure should be able to be used as follows:

((repeated square 2) 5)
625

;; Hint: You may find it convenient to use compose from Exercise 1.42.

(define (repeated f n)
  (define (iter n res)
    (cond ((< n 1) (lambda (x) x))
          ((= n 1) res)
          ((even? n) (double (iter (/ n 2) res)))
          (else (compose f (iter (- n 1) res)))))
  (iter n f))

;; Or
(define (repeated f n)
  (define (iter i res)
    (if (>= i n)
      res
      (iter (+ i 1) (compose f res))))
  (if (< n 1) (lambda (x) x) (iter 1 f)))
;; ================================================================================



;; ================================================================================
;; Exercise 1.44
;;
;; The idea of smoothing a function is an important concept in signal
;; processing. If f is a function and dx is some small number, then the
;; smoothed version of f is the function whose value at a point x is the
;; average of f(x - dx), f(x), and f(x + dx). Write a procedure smooth that
;; takes as input a procedure that computes f and returns a procedure that
;; computes the smoothed f. It is sometimes valuable to repeatedly smooth a
;; function (that is, smooth the smoothed function, and so on) to obtain the
;; n-fold smoothed function. Show how to generate the n-fold smoothed function
;; of any given function using smooth and repeated from Exercise 1.43.

(define (smooth f)
  (define dx 0.1)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define double-smooth (repeated smooth 2))
;; ================================================================================



;; ================================================================================
;; Exercise 1.45
;;
;; We saw in Section 1.3.3 that attempting to compute square roots by naively
;; finding a fixed point of y -> x/y does not converge, and that this can be
;; fixed by average damping. The same method works for finding cube roots as
;; fixed points of the average-damped y -> x/(y^2). Unfortunately, the process
;; does not work for fourth roots---a single average damp is not enough to make
;; a fixed-point search for y -> x/(y^3) converge. On the other hand, if we
;; average damp twice (i.e., use the average damp of the average damp of y ->
;; x/[y^3]) the fixed-point search does converge. Do some experiments to
;; determine how many average damps are required to compute nth roots as a
;; fixed-point search based upon repeated average damping of y -> x/(y^[n-1]).
;; Use this to implement a simple procedure for computing nth roots using
;; fixed-point, average-damp, and the repeated procedure of Exercise 1.43.
;; Assume that any arithmetic operations you need are available as primitives.

;; Experiments show that average damp needs to be applied log_2(n) times
(define (root n r)
  (let ((damps (floor (/ (log n) (log 2)))))
    (fixed-point ((repeated average-damp damps)
                  (lambda (x) (/ r (expt x (- n 1)))))
                 1.0)))
;; ================================================================================



;; ================================================================================
;; Exercise 1.46
;;
;; Several of the numerical methods described in this chapter are instances of
;; an extremely general computational strategy known as iterative improvement.
;; Iterative improvement says that, to compute something, we start with an
;; initial guess for the answer, test if the guess is good enough, and
;; otherwise improve the guess and continue the process using the improved
;; guess as the new guess. Write a procedure iterative-improve that takes two
;; procedures as arguments: a method for telling whether a guess is good enough
;; and a method for improving a guess. iterative-improve should return as its
;; value a procedure that takes a guess as argument and keeps improving the
;; guess until it is good enough. Rewrite the sqrt procedure of Section 1.1.7
;; and the fixed-point procedure of Section 1.3.3 in terms of
;; iterative-improve.

(define (iterative-improve good-enough? improve)
  (define (iter guess) (if (good-enough? guess) guess (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define (sqrt n)
  (define (good-enough? g) (> 0.001 (abs (- n (square g)))))
  (define (improve g) (/ (+ g (/ n g)) 2))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define (good-enough? g) (> 0.001 (abs (- g (f g)))))
  ((iterative-improve good-enough? f) first-guess))
;; ================================================================================
