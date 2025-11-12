;; ================================================================================
;; Exercise 2.1
;;
;; Define a better version of make-rat that handles both positive and negative
;; arguments. make-rat should normalize the sign so that if the rational number
;; is positive, both the numerator and denominator are positive, and if the
;; rational number is negative, only the numerator is negative.

(define (gcd a b) (if (= b 0) (abs a) (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (norm-sign (if (< d 0) - +)))
    (cons (norm-sign (/ n g))
          (norm-sign (/ d g)))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.2
;;
;; Consider the problem of representing line segments in a plane. Each segment
;; is represented as a pair of points: a starting point and an ending point.
;; Define a constructor make-segment and selectors start-segment and
;; end-segment that define the representation of segments in terms of points.
;; Furthermore, a point can be represented as a pair of numbers: the x
;; coordinate and the y coordinate. Accordingly, specify a constructor
;; make-point and selectors x-point and y-point that define this
;; representation. Finally, using your selectors and constructors, define a
;; procedure midpoint-segment that takes a line segment as argument and returns
;; its midpoint (the point whose coordinates are the average of the coordinates
;; of the endpoints). To try your procedures, you'll need a way to print
;; points:

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (midpoint-segment s)
  (define (average a b) (/ (+ a b) 2.0))
  (define (average-point a b)
    (let ((x1 (x-point a))
          (x2 (x-point b))
          (y1 (y-point a))
          (y2 (y-point b)))
      (make-point (average x1 x2) (average y1 y2))))
  (average-point (start-segment s) (end-segment s)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.3
;;
;; Implement a representation for rectangles in a plane. (Hint: You may want to
;; make use of Exercise 2.2.) In terms of your constructors and selectors,
;; create procedures that compute the perimeter and the area of a given
;; rectangle. Now implement a different representation for rectangles. Can you
;; design your system with suitable abstraction barriers, so that the same
;; perimeter and area procedures will work using either representation?

(define (rect-perim rect) (* 2 (+ (width rect) (height rect))))

(define (rect-area rect) (* (width rect) (height rect)))

;; Rectangle as 4 points
(define (width rect)
  (define (x-distance a b) (abs (- (x-point a) (x-point b))))
  (x-distance (car (car rect)) (cdr (cdr rect))))

(define (height rect)
  (define (y-distance a b) (abs (- (y-point a) (y-point b))))
  (y-distance (car (car rect)) (cdr (car rect))))

(define (make-rect origin width height)
  (let ((a origin)
        (b (make-point (x-point origin) (+ height (y-point origin))))
        (c (make-point (+ width (x-point origin)) (+ height (y-point origin))))
        (d (make-point (+ width (x-point origin)) (y-point origin))))
    (cons (cons a b) (cons c d))))

;; Rectangle as a diagonal
(define (width rect)
  (define (x-distance a b) (abs (- (x-point a) (x-point b))))
  (x-distance (start-segment rect) (end-segment rect)))

(define (height rect)
  (define (y-distance a b) (abs (- (y-point a) (y-point b))))
  (y-distance (start-segment rect) (end-segment rect)))

(define (make-rect origin width height)
  (make-segment origin
                (make-point (+ width (x-point origin))
                            (+ height (y-point origin)))))

;; Rectangle as origin + width + height
(define (width rect) (car (cdr rect)))
(define (height rect) (cdr (cdr rect)))
(define (make-rect origin width height) (cons origin (cons width height)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.4
;;
;; Here is an alternative procedural representation of pairs. For this
;; representation, verify that (car (cons x y)) yields x for any objects x and
;; y

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))

;; What is the corresponding definition of cdr? (Hint: To verify that this
;; works, make use of the substitution model of Section 1.1.5.)

(define (cdr z) (z (lambda (p q) q)))

(car (cons x y))
(car (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
x
;; ================================================================================



;; ================================================================================
;; Exercise 2.5
;;
;; Show that we can represent pairs of nonnegative integers using only numbers
;; and arithmetic operations if we represent the pair a and b as the integer
;; that is the product (2^a)(3^b). Give the corresponding definitions of the
;; procedures cons, car, and cdr.

(define (cons a b) (* (expt 2 a) (expt 3 b)))

(define (count-power x b)
  (define (iter x count)
    (if (= 0 (remainder x b))
      (iter (/ x b) (+ 1 count))
      count))
  (iter x 0))

(define (car c) (count-power c 2))
(define (cdr c) (count-power c 3))
;; ================================================================================



;; ================================================================================
;; Exercise 2.6
;;
;; In case representing pairs as procedures wasn't mind-boggling enough,
;; consider that, in a language that can manipulate procedures, we can get by
;; without numbers (at least insofar as nonnegative integers are concerned) by
;; implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; This representation is known as Church numerals, after its inventor, Alonzo
;; Church, the logician who invented the lambda-calculus.
;;
;; Define one and two directly (not in terms of zero and add-1). (Hint: Use
;; substitution to evaluate (add-1 zero)). Give a direct definition of the
;; addition procedure + (not in terms of repeated application of add-1).

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

;; Here, a number n is represented by a function that takes another function f
;; and returns a function that applies f to its argument n times. For example:
(define (inc x) (+ x 1))
(define (toint n) ((n inc) 0))
(toint zero) ;; 0
(toint one) ;; 1
(toint (add two two)) ;; 4
;; ================================================================================



;; ================================================================================
;; Exercise 2.7

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

;; Alyssa's program is incomplete because she has not specified the
;; implementation of the interval abstraction. Here is a definition of the
;; interval constructor:

(define (make-interval a b) (cons a b))

;; Define selectors upper-bound and lower-bound to complete the implementation.

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

;; Or
(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.8
;;
;; Using reasoning analogous to Alyssa's, describe how the difference of two
;; intervals may be computed. Define a corresponding subtraction procedure,
;; called sub-interval.

;; By adding the inverse of an interval
(define (sub-interval a b)
  (define (inverse i)
    (make-interval
      (- (upper-bound i))
      (- (lower-bound i))))
  (add-interval a (inverse b)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.9
;;
;; The width of an interval is half of the difference between its upper and
;; lower bounds. The width is a measure of the uncertainty of the number
;; specified by the interval. For some arithmetic operations the width of the
;; result of combining two intervals is a function only of the widths of the
;; argument intervals, whereas for others the width of the combination is not a
;; function of the widths of the argument intervals. Show that the width of the
;; sum (or difference) of two intervals is a function only of the widths of the
;; intervals being added (or subtracted). Give examples to show that this is
;; not true for multiplication or division.

;; Addition:
;; Let x and y be intervals
;; Let ux = (upper-bound x)
;;     lx = (lower-bound x)
;;     uy = (upper-bound y)
;;     ly = (lower-bound y)
;;     
;;        ux - lx
;; w(x) = -------
;;           2
;; 
;;        uy - ly
;; w(y) = -------
;;           2
;;
;;            (ux + uy) - (lx + ly)   ux - lx   uy - ly
;; w(x + y) = --------------------- = ------- + ------- = w(x) + w(y)
;;                      2                2         2   
;;
;;
;; Multiplication:
;; x     = [0, 1], w(x)     = 0.5
;; y     = [0, 1], w(y)     = 0.5
;; x * y = [0, 1], w(x * y) = 0.5
;;
;; x     = [0, 2], w(x)     = 1
;; y     = [0, 2], w(y)     = 1
;; x * y = [0, 4], w(x * y) = 2
;;
;; x     = [0, 2], w(x)     = 1
;; y     = [0, 4], w(y)     = 2
;; x * y = [0, 8], w(x * y) = 4
;; ================================================================================



;; ================================================================================
;; Exercise 2.10
;;
;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder
;; and comments that it is not clear what it means to divide by an interval
;; that spans zero. Modify Alyssa's code to check for this condition and to
;; signal an error if it occurs.

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
    (error "Divisor interval spans zero")
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.11
;;
;; In passing, Ben also cryptically comments: "By testing the signs of the
;; endpoints of the intervals, it is possible to break mul-interval into nine
;; cases, only one of which requires more than two multiplications." Rewrite
;; this procedure using Ben's suggestion.

;; [+lx, +ux] * [+ly, +uy] = [lx * ly, ux * uy]
;; [+lx, +ux] * [-ly, +uy] = [ux * ly, ux * uy]
;; [+lx, +ux] * [-ly, -uy] = [ux * ly, lx * uy]
;; [-lx, +ux] * [+ly, +uy] = [lx * uy, ux * uy]
;; [-lx, +ux] * [-ly, +uy] = [min(lx * uy, ux * ly), ux * uy]
;; [-lx, +ux] * [-ly, -uy] = [ux * ly, lx * ly]
;; [-lx, -ux] * [+ly, +uy] = [lx * uy, ux * ly]
;; [-lx, -ux] * [-ly, +uy] = [lx * uy, lx * ly]
;; [-lx, -ux] * [-ly, -uy] = [ux * uy, lx * ly]


(define (mul-interval x y)
  (define (pos-pos i) (and (>= (lower-bound i) 0) (>= (upper-bound i) 0)))
  (define (neg-pos i) (and (<= (lower-bound i) 0) (>= (upper-bound i) 0)))
  (define (neg-neg i) (and (<= (lower-bound i) 0) (<= (upper-bound i) 0)))
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((and (pos-pos x) (pos-pos y)) (make-interval (* lx ly) (* ux uy)))
          ((and (pos-pos x) (neg-pos y)) (make-interval (* ux ly) (* ux uy)))
          ((and (pos-pos x) (neg-neg y)) (make-interval (* ux ly) (* lx uy)))
          ((and (neg-pos x) (pos-pos y)) (make-interval (* lx uy) (* ux uy)))
          ((and (neg-pos x) (neg-pos y)) (make-interval (min (* lx uy) (* ux ly)) (* ux uy)))
          ((and (neg-pos x) (neg-neg y)) (make-interval (* ux ly) (* lx ly)))
          ((and (neg-neg x) (pos-pos y)) (make-interval (* lx uy) (* ux ly)))
          ((and (neg-neg x) (neg-pos y)) (make-interval (* lx uy) (* lx ly)))
          ((and (neg-neg x) (neg-neg y)) (make-interval (* ux uy) (* lx ly))))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Define a constructor make-center-percent that takes a center and a
;; percentage tolerance and produces the desired interval. You must also define
;; a selector percent that produces the percentage tolerance for a given
;; interval. The center selector is the same as the one shown above.

(define (make-center-percent c p)
  (let ((width (/ (* c p) 100.0)))
    (make-interval (- c width) (+ c width))))

(define (percent i) (* 100.0 (/ (width i) (center i))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.13
;;
;; Show that under the assumption of small percentage tolerances there is a
;; simple formula for the approximate percentage tolerance of the product of
;; two intervals in terms of the tolerances of the factors. You may simplify
;; the problem by assuming that all numbers are positive.

;; Let x, y be the center values of the 2 intervals and dx, dy be the widths.
;; Then, since all numbers are positive
;;
;; [x - dx, x + dx] * [y - dy, y + dy]
;;   = [(x - dx)(y - dy), (x + dx)(y + dy)]
;;   = [x*y - x*dy - y*dx + dx*dy, x*y + x*dy + y*dx + dx*dy]
;;   ~ [x*y - (x*dy + y*dx), x*y + (x*dy + y*dx)] Since dx*dy ~ 0
;;
;; Therefore, x*y is the center and x*dy + y*dx is the width dxy of the product
;; of the intervals
;;
;; Let px, py, pxy be the percentage tolerances. Then
;;       dx         dy
;; px = ----  py = ----
;;       x          y
;;
;;        dxy    x*dy + y*dx     x*dy    y*dx    dy     dx
;; pxy = ----- = ----------- =  ----- + ----- = ---- + ---- = px + py
;;        x*y        x*y         x*y     x*y     y      x
;; ================================================================================



;; ================================================================================
;; Exercise 2.14
;;
;; Demonstrate that Lem is right. Investigate the behavior of the system on a
;; variety of arithmetic expressions. Make some intervals A and B, and use them
;; in computing the expressions A/A and A/B. You will get the most insight by
;; using intervals whose width is a small percentage of the center value.
;; Examine the results of the computation in center-percent form (see Exercise
;; 2.12).

(define a (make-center-percent 40 2))
(define b (make-center-percent 25 1))

;; A/A should conceptually result in 1.0 no matter what (unless it spans 0)
;; since the actual values are always the same. But our system treats the 2
;; parameters of div-interval as independent intervals even though the actual
;; arguments passed are a single interval
(div-interval a a)
;; (.96 . 1.04)

;; The center of the interval that is the result of division of 2 intervals is
;; close to the quotient of the centers of those intervals (bigger tolerance
;; results in bigger differences)
(center (div-interval a b))
;; 1.6
(/ (center a) (center b))
;; 1.6

;; Same applies to addition and multiplication
(center (add-interval a b))          
;; 65.
(+ (center a) (center b))
;; 65.
(center (mul-interval a b))
;; 1000.
(* (center a) (center b))  
;; 1000.

;; The tolerance of the interval that is the result of division of 2 intervals
;; is close to the sum of the tolerances of those intervals (bigger tolerance
;; results in bigger differences)
(percent (div-interval a b))
;; 3.0
(+ (percent a) (percent b))
;; 3.0

;; Same applies to multiplication (as was shown in 2.13)
(percent (mul-interval a b))
;; 3.0
(+ (percent a) (percent b))
;; 3.0


;; The tolerance of the sum seems to be (very) roughly the average of the
;; tolerances of the intervals being added
(percent (add-interval a b))
;; 1.62
(/ (+ (percent a) (percent b)) 2)
;; 1.5
;; ================================================================================



;; ================================================================================
;; Exercise 2.15
;;
;; Eva Lu Ator, another user, has also noticed the different intervals computed
;; by different but algebraically equivalent expressions. She says that a
;; formula to compute with intervals using Alyssa's system will produce tighter
;; error bounds if it can be written in such a form that no variable that
;; represents an uncertain number is repeated. Thus, she says, par2 is a
;; "better" program for parallel resistances than par1. Is she right? Why?

;; Yes, because if an uncertain quality, represented as an interval, is
;; repeated in a formula, our procedures treat its instances as independent
;; intervals, as if they were multiple uncertain qualities that happen to have
;; the same bounds
;; ================================================================================



;; ================================================================================
;; Exercise 2.16
;;
;; Explain, in general, why equivalent algebraic expressions may lead to
;; different answers. Can you devise an interval-arithmetic package that does
;; not have this shortcoming, or is this task impossible? (Warning: This
;; problem is very difficult.)

;; Because algebraic expressions have the concept of identity represented by
;; the variables, whereas our intervals are simply values. It doesn't matter
;; that we may bind an interval to a name and pass it as argument to multiple
;; procedure parameters: the procedure sees its parameters as different.
;;
;; It is possible to create an interval-arithmetic package without this
;; problem, but it'd have to offer means to separately construct an algebraic
;; expression with abstract variables and separately evaluate it by passing the
;; real values of each variable. In the simplest form, it could then test all
;; combinations of lower and upper bounds of each interval and find the minimum
;; and maximum values. This will have 2^n (n is the number of unique intervals)
;; performance but the implementation will be relatively simple
;;
;; Here I'm pretending I don't know about lists and nil yet since they haven't
;; been mentioned in the book at this point

;; Expression evaluation
(define (make-getter head)
  (define (get i head)
    (cond ((not head) #f)
          ((= i 0) (car head))
          (else (get (- i 1) (cdr head)))))
  (lambda (i) (get i head)))

(define (concat ahead bhead)
  (define (iter head)
    (if (not head)
      bhead
      (cons (car ahead) (concat (cdr ahead) bhead))))
  (iter ahead))

(define (map f head)
  (if (not head)
    #f
    (cons (f (car head)) (map f (cdr head)))))

(define (minmax head)
  (define (iter min-val max-val head)
    (cond ((not head) (cons min-val max-val))
          ((not min-val) (iter (car head) (car head) (cdr head)))
          ((> (car head) max-val) (iter min-val (car head) (cdr head)))
          ((< (car head) min-val) (iter (car head) max-val (cdr head)))
          (else (iter min-val max-val (cdr head)))))
  (iter #f #f head))

(define (combinations im)
  (define (go im tail)
    (if (not im)
      tail
      (let ((lo (lower-bound (car im)))
            (up (upper-bound (car im)))
            (new-tail (go (cdr im) tail)))
        (concat (map (lambda (tail) (cons lo tail)) new-tail)
                (map (lambda (tail) (cons up tail)) new-tail)))))
  (go im (cons #f #f)))

(define (evaluate expr im)
  (let ((results (map (lambda (m) (expr (make-getter m))) (combinations im))))
    (let ((bounds (minmax results)))
      (make-interval (car bounds) (cdr bounds)))))


;; Expression construction
(define (cons-var i) (lambda (m) (m i)))
(define (cons-const x) (lambda (m) x))
(define (cons-op op x y) (lambda (m) (op (x m) (y m))))


;; Example
(define r1 (make-center-percent 42.0 2.0))
(define r2 (make-center-percent 42.0 2.0))
(define intervals (cons r1 (cons r2 #f)))

(define self-div (cons-op / (cons-var 0) (cons-var 0))) ;; dividing an interval by itself
(define self-sub (cons-op - (cons-var 0) (cons-var 0))) ;; subtracting an interval from itself
(define same-div (cons-op / (cons-var 0) (cons-var 1))) ;; dividing an interval by an equal one
(define same-sub (cons-op - (cons-var 0) (cons-var 1))) ;; subtracting an interval from an equal one

;; R_1 * R_2
;; ---------
;; R_1 + R_2
(define expr-1
  (cons-op /
           (cons-op * (cons-var 0) (cons-var 1))
           (cons-op + (cons-var 0) (cons-var 1)))) 

;;       1
;; -------------
;; 1/R_1 + 1/R_2
(define expr-2
  (cons-op /
           (cons-const 1)
           (cons-op +
                    (cons-op / (cons-const 1) (cons-var 0))
                    (cons-op / (cons-const 1) (cons-var 1)))))

(evaluate self-div intervals)             
;; (1. . 1.)
(evaluate same-div intervals)
;; (.96 . 1.04)
(evaluate self-sub intervals)
;; (0. . 0.)
(evaluate same-sub intervals)
;; (-1.68 . 1.68)
(evaluate expr-1 intervals)
;; (20.58 . 21.42)
(evaluate expr-2 intervals) 
;; (20.58 . 21.42)
;; ================================================================================



;; ================================================================================
;; Exercise 2.17
;;
;; Define a procedure last-pair that returns the list that contains only the
;; last element of a given (nonempty) list:

(last-pair (list 23 72 149 34))
(34)

(define (last-pair head)
  (cond ((null? head) (error "Empty list"))
        ((null? (cdr head)) (car head))
        (else (last-pair (cdr head)))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.18
;;
;; Define a procedure reverse that takes a list as argument and returns a list
;; of the same elements in reverse order:

(reverse (list 1 4 9 16 25))
(25 16 9 4 1)

(define (reverse head)
  (define (iter head tail)
    (if (null? head)
      tail
      (iter (cdr head) (cons (car head) tail))))
  (iter head '()))
;; ================================================================================



;; ================================================================================
;; Exercise 2.19
;;
;; Consider the change-counting program of Section 1.2.2. It would be nice to
;; be able to easily change the currency used by the program, so that we could
;; compute the number of ways to change a British pound, for example. As the
;; program is written, the knowledge of the currency is distributed partly into
;; the procedure first-denomination and partly into the procedure count-change
;; (which knows that there are five kinds of U.S. coins). It would be nicer to
;; be able to supply a list of coins to be used for making change.
;;
;; We want to rewrite the procedure cc so that its second argument is a list of
;; the values of the coins to use rather than an integer specifying which coins
;; to use. We could then have lists that defined each kind of currency:

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; We could then call cc as follows:

(cc 100 us-coins)
292

;; To do this will require changing the program cc somewhat. It will still have
;; the same form, but it will access its second argument differently, as
;; follows:

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination
                   coin-values))
             (cc (- amount
                    (first-denomination
                      coin-values))
                 coin-values)))))

;; Define the procedures first-denomination, except-first-denomination, and
;; no-more? in terms of primitive operations on list structures. Does the order
;; of the list coin-values affect the answer produced by cc? Why or why not?

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

;; The order of coins does not matter since it does not matter in the algorithm
;; description. Alternatively, the answer depends on the number of times the
;; procedure was called with amount = 0, and this amount is reached by
;; subtracting coin values from the amount. It doesn't matter what order we do
;; it in so the order of coins doesn't matter
;; ================================================================================



;; ================================================================================
;; Exercise 2.20
;;
;; The procedures +, *, and list take arbitrary numbers of arguments. One way
;; to define such procedures is to use define with dotted-tail notation. In a
;; procedure definition, a parameter list that has a dot before the last
;; parameter name indicates that, when the procedure is called, the initial
;; parameters (if any) will have as values the initial arguments, as usual, but
;; the final parameter's value will be a list of any remaining arguments. For
;; instance, given the definition
;;
;; (define (f x y . z) ⟨body⟩)
;;
;; the procedure f can be called with two or more arguments. If we evaluate
;;
;; (f 1 2 3 4 5 6)
;;
;; then in the body of f, x will be 1, y will be 2, and z will be the list (3 4
;; 5 6). Given the definition
;;
;; (define (g . w) ⟨body⟩)
;;
;; the procedure g can be called with zero or more arguments. If we evaluate
;;
;; (g 1 2 3 4 5 6)
;;
;; then in the body of g, w will be the list (1 2 3 4 5 6).
;;
;; Use this notation to write a procedure same-parity that takes one or more
;; integers and returns a list of all the arguments that have the same even-odd
;; parity as the first argument. For example,

(same-parity 1 2 3 4 5 6 7)
(1 3 5 7)
(same-parity 2 3 4 5 6 7)
(2 4 6)

(define (filter pred head)
  (cond ((null? head) '())
        ((pred (car head)) (cons (car head) (filter pred (cdr head))))
        (else (filter pred (cdr head)))))

(define (same-parity head . tail)
  (cons head (filter (lambda (x) (= (remainder head 2) (remainder x 2))) tail)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.21
;;
;; The procedure square-list takes a list of numbers as argument and returns a
;; list of the squares of those numbers.

(square-list (list 1 2 3 4))
(1 4 9 16)

;; Here are two different definitions of square-list. Complete both of them by
;; filling in the missing expressions:

(define (square-list items)
  (if (null? items)
    '()
    (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))
;; ================================================================================



;; ================================================================================
;; Exercise 2.22
;;
;; Louis Reasoner tries to rewrite the first square-list procedure of Exercise
;; 2.21 so that it evolves an iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items '()))

;; Unfortunately, defining square-list this way produces the answer list in the
;; reverse order of the one desired. Why?
;;
;; Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items nil))

;; This doesn't work either. Explain.

;; In the first case it doesn't work because he moves "right" in the things
;; list and "left" in the answer list. The second case doesn't work because
;; cons does not concatenate lists, it appends a head to a tail. The first
;; argument is an element, the second is a list of elements. You cannot switch
;; them up.
;; ================================================================================



;; ================================================================================
;; Exercise 2.23
;;
;; The procedure for-each is similar to map. It takes as arguments a procedure
;; and a list of elements. However, rather than forming a list of the results,
;; for-each just applies the procedure to each of the elements in turn, from
;; left to right. The values returned by applying the procedure to the elements
;; are not used at all—for-each is used with procedures that perform an action,
;; such as printing. For example,

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
57
321
88

;; The value returned by the call to for-each (not illustrated above) can be
;; something arbitrary, such as true. Give an implementation of for-each.

(define (for-each f head)
  (cond ((null? head) #t)
        (else (f (car head)) (for-each f (cdr head)))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.24
;;
;; Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the
;; result printed by the interpreter, the corresponding box-and-pointer
;; structure, and the interpretation of this as a tree (as in Figure 2.6).

;; (1 (2 (3 4)))
;;
;; ┌───┬───┐     ┌───┬───┐
;; │ ┃ │ ━━━━━━━🭬│ ┃ │ ╱ │
;; └─┃─┴───┘     └─┃─┴───┘
;;   ┃             ┃
;;   🭭             🭭
;; ┌───┐         ┌───┬───┐     ┌───┬───┐
;; │ 1 │         │ ┃ │ ━━━━━━━🭬│ ┃ │ ╱ │
;; └───┘         └─┃─┴───┘     └─┃─┴───┘
;;                 ┃             ┃
;;                 🭭           ┌───┬───┐     ┌───┬───┐
;;               ┌───┐         │ ┃ │ ━━━━━━━🭬│ ┃ │ ╱ │
;;               │ 2 │         └─┃─┴───┘     └─┃─┴───┘
;;               └───┘           ┃             ┃
;;                               🭭             🭭
;;                             ┌───┐         ┌───┐
;;                             │ 3 │         │ 4 │
;;                             └───┘         └───┘
;;    (1 (2 (3 4)))
;;  𜰵               𜰶
;; 1             (2 (3 4))
;;              𜰵         𜰶
;;             2         (3 4)
;;                      𜰵     𜰶
;;                     3       4
;; ================================================================================



;; ================================================================================
;; Exercise 2.25
;;
;; Give combinations of cars and cdrs that will pick 7 from each of the
;; following lists:
;;
;; (1 3 (5 7) 9)
;; ((7))
;; (1 (2 (3 (4 (5 (6 7))))))

(car
  (cdr
    (car
      (cdr
        (cdr
          (list 1 3 (list 5 7) 9))))))
(car
  (car
    (list (list 7))))

(car
  (cdr
    (car
      (cdr
        (car
          (cdr
            (car
              (cdr
                (car
                  (cdr
                    (car
                      (cdr
                        (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.26
;;
;; Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

;; What result is printed by the interpreter in response to evaluating each of
;; the following expressions:

(append x y)
(cons x y)
(list x y)

;; (1 2 3 4 5 6)
;; ((1 2 3) 4 5 6)
;; ((1 2 3) (4 5 6))
;; ================================================================================



;; ================================================================================
;; Exercise 2.27
;;
;; Modify your reverse procedure of Exercise 2.18 to produce a deep-reverse
;; procedure that takes a list as argument and returns as its value the list
;; with its elements reversed and with all sublists deep-reversed as well. For
;; example,

(define x (list (list 1 2) (list 3 4)))
x
;; ((1 2) (3 4))
(reverse x)
;; ((3 4) (1 2))
(deep-reverse x)
;; ((4 3) (2 1))

(define (deep-reverse head)
  (define (iter head tail)
    (if (null? head)
      tail
      (let ((el (if (pair? (car head))
                  (deep-reverse (car head))
                  (car head))))
        (iter (cdr head) (cons el tail)))))
  (iter head '()))
;; ================================================================================



;; ================================================================================
;; Exercise 2.28
;;
;; Write a procedure fringe that takes as argument a tree (represented as a
;; list) and returns a list whose elements are all the leaves of the tree
;; arranged in left-to-right order. For example,

(define x (list (list 1 2) (list 3 4)))
(fringe x)
;; (1 2 3 4)
(fringe (list x x))
;; (1 2 3 4 1 2 3 4)

(define (fringe tree)
  (cond ((null? tree) '())
        ((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.29
;;
;; A binary mobile consists of two branches, a left branch and a right branch.
;; Each branch is a rod of a certain length, from which hangs either a weight
;; or another binary mobile. We can represent a binary mobile using compound
;; data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number) together with
;; a structure, which may be either a number (representing a simple weight) or
;; another mobile:

(define (make-branch length structure)
  (list length structure))

;; a. Write the corresponding selectors left-branch and right-branch, which
;; return the branches of a mobile, and branch-length and branch-structure,
;; which return the components of a branch.
;;
;; b. Using your selectors, define a procedure total-weight that returns the
;; total weight of a mobile.
;;
;; c. A mobile is said to be balanced if the torque applied by its top-left
;; branch is equal to that applied by its top-right branch (that is, if the
;; length of the left rod multiplied by the weight hanging from that rod is
;; equal to the corresponding product for the right side) and if each of the
;; submobiles hanging off its branches is balanced. Design a predicate that
;; tests whether a binary mobile is balanced.
;;
;; d. Suppose we change the representation of mobiles so that the constructors
;; are

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

;; How much do you need to change your programs to convert to the new
;; representation?

;; a
(define left-branch car)
(define right-branch cadr)

(define branch-length car)
(define branch-structure cadr)

;; b
(define (total-weight m)
  (if (pair? m)
    (+ (total-weight (branch-structure (left-branch m)))
       (total-weight (branch-structure (right-branch m))))
    m))

;; c
(define (balanced? m)
  (define (torque b)
    (* (branch-length b)
       (total-weight (branch-structure b))))
  (if (pair? m)
    (and (= (torque (left-branch m)) (torque (right-branch m)))
         (balanced? (branch-structure (left-branch m)))
         (balanced? (branch-structure (right-branch m))))
    #t))

;; d
(define left-branch car)
(define right-branch cdr)

(define branch-length car)
(define branch-structure cdr)
;; ================================================================================



;; ================================================================================
;; Exercise 2.30
;;
;; Define a procedure square-tree analogous to the square-list procedure of
;; Exercise 2.21. That is, square-tree should behave as follows:

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

;; Define square-tree both directly (i.e., without using any higher-order
;; procedures) and also by using map and recursion.

(define (square-tree tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (square-tree (car tree)) (square-tree (cdr tree))))
        (else (square tree))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))
;; ================================================================================



;; ================================================================================
;; Exercise 2.31
;;
;; Abstract your answer to Exercise 2.30 to produce a procedure tree-map with
;; the property that square-tree could be defined as

(define (square-tree tree) (tree-map square tree))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))
;; ================================================================================



;; ================================================================================
;; Exercise 2.32
;;
;; We can represent a set as a list of distinct elements, and we can represent
;; the set of all subsets of the set as a list of lists. For example, if the
;; set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3)
;; (1 2) (1 2 3)). Complete the following definition of a procedure that
;; generates the set of subsets of a set and give a clear explanation of why it
;; works:

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (ss) (cons (car s) ss)) rest)))))

;; For each element E of a set S, half of the subsets of S contain E and half
;; don't. Those that don't are the subsets of another set S' that conaints all
;; elements of S except E. To find all the subsets of S, we find all the
;; subsets of S' and for each subset produce 2 new subsets: the original and
;; with the added element E.
;; ================================================================================



;; ================================================================================
;; Exercise 2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

;; Fill in the missing expressions to complete the following definitions of
;; some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
;; ================================================================================



;; ================================================================================
;; Exercise 2.34
;;
;; Evaluating a polynomial in x at a given value of x can be formulated as an
;; accumulation. We evaluate the polynomial
;;
;; a_n * x^n + a_(n-1) * x^(n-1) + ... + a_1 * x + a_0
;;
;; using a well-known algorithm called Horner's rule, which structures the
;; computation as
;;
;; (...(a_n * x + a_(n-1)) * x + ... + a_1) * x + a_0
;;
;; In other words, we start with a_n, multiply by x, add a_(n-1), multiply by
;; x, and so on, until we reach a_0.
;;
;; Fill in the following template to produce a procedure that evaluates a
;; polynomial using Horner's rule. Assume that the coefficients of the
;; polynomial are arranged in a sequence, from a_0 through a_n.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

;; For example, to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would evaluate

(horner-eval 2 (list 1 3 0 5 0 1))
;; ================================================================================



;; ================================================================================
;; Exercise 2.35
;;
;; Redefine count-leaves from Section 2.2.2 as an accumulation:

(define (count-leaves t)
  (accumulate + 0 (map (lambda (t) (if (pair? t) (count-leaves t) 1)) t)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.36
;;
;; The procedure accumulate-n is similar to accumulate except that it takes as
;; its third argument a sequence of sequences, which are all assumed to have
;; the same number of elements. It applies the designated accumulation
;; procedure to combine all the first elements of the sequences, all the second
;; elements of the sequences, and so on, and returns a sequence of the results.
;; For instance, if s is a sequence containing four sequences, ((1 2 3) (4 5 6)
;; (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s) should be the
;; sequence (22 26 30). Fill in the missing expressions in the following
;; definition of accumulate-n:

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.37
;;
;; Suppose we represent vectors v = (v_i) as sequences of numbers, and matrices
;; m = (m_ij) as sequences of vectors (the rows of the matrix). For example,
;; the matrix
;;
;; ┌         ┐
;; │ 1 2 3 4 │
;; │ 4 5 6 6 │
;; │ 6 7 8 9 │
;; └         ┘
;;
;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this
;; representation, we can use sequence operations to concisely express the
;; basic matrix and vector operations. These operations (which are described in
;; any book on matrix algebra) are the following:
;;
;; (dot-product v w)     returns the sum Sum_i(v_i * w_i);
;; (matrix-*-vector m v) returns the vector t, where t_i = Sum_j(m_ij * v_j);
;; (matrix-*-matrix m n) returns the matrix p, where p_ij = Sum_k(m_ik * n_kj);
;; (transpose m)         returns the matrix n, where n_ij = m_ji.
;;
;; We can define the dot product as

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for computing
;; the other matrix operations. (The procedure accumulate-n is defined in
;; Exercise 2.36.)

(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi) (matrix-*-vector cols mi)) m)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.38
;;
;; The accumulate procedure is also known as fold-right, because it combines
;; the first element of the sequence with the result of combining all the
;; elements to the right. There is also a fold-left, which is similar to
;; fold-right, except that it combines elements working in the opposite
;; direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

;; What are the values of

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

;; Give a property that op should satisfy to guarantee that fold-right and
;; fold-left will produce the same values for any sequence.

(fold-right / 1 (list 1 2 3))
(/ 1 (/ 2 (/ 3 1)))
;; 3/2

(fold-left / 1 (list 1 2 3))
(/ (/ (/ 1 1) 2) 3)
;; 1/6

(fold-right list '() (list 1 2 3))
(list 1 (list 2 (list 3 '())))
;; (1 (2 (3 ())))

(fold-left list '() (list 1 2 3))
(list (list (list '() 1) 2) 3)
;; (((() 1) 2) 3)

;; (fold-right op i (list a b c))
;; (op a (op b (op c i)))
;;
;; (fold-left op i (list a b c))
;; (op (op (op i a) b) c)
;;
;; fold-right and fold-left will produce the same results if op is commutative
;; and associative
;; ================================================================================



;; ================================================================================
;; Exercise 2.39
;;
;; Complete the following definitions of reverse (Exercise 2.18) in terms of
;; fold-right and fold-left from Exercise 2.38:

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
;; ================================================================================



;; ================================================================================
;; Exercise 2.40
;;
;; Define a procedure unique-pairs that, given an integer n, generates the
;; sequence of pairs (i, j) with 1 <= j < i <= n. Use unique-pairs to simplify
;; the definition of prime-sum-pairs given above.

(define (enumerate-interval lo hi)
  (if (> lo hi)
    '()
    (cons lo (enumerate-interval (+ 1 lo) hi))))

(define (flatmap f head)
  (fold-right append '() (map f head)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (and (not (= 1 n)) (= n (smallest-divisor n))))


(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter (lambda (pair) (prime? (+ (car pair) (cadr pair))))
               (unique-pairs n))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.41
;;
;; Write a procedure to find all ordered triples of distinct positive integers
;; i, j, and k less than or equal to a given integer n that sum to a given
;; integer s.

(define (unique-triples n)
  (flatmap (lambda (pair)
             (map (lambda (k) (list (car pair) (cadr pair) k))
                  (enumerate-interval 1 (- (cadr pair) 1))))
           (flatmap (lambda (i)
                      (map (lambda (j) (list i j))
                           (enumerate-interval 2 (- i 1))))
                    (enumerate-interval 3 n))))

(define (ex241 n s)
  (filter (lambda (triple) (= s (+ (car triple) (cadr triple) (caddr triple))))
          (unique-triples n)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.42
;;
;; The "eight-queens puzzle" asks how to place eight queens on a chessboard so
;; that no queen is in check from any other (i.e., no two queens are in the
;; same row, column, or diagonal). One possible solution is shown in Figure
;; 2.8. One way to solve the puzzle is to work across the board, placing a
;; queen in each column. Once we have placed k - 1 queens, we must place the
;; kth queen in a position where it does not check any of the queens already on
;; the board. We can formulate this approach recursively: Assume that we have
;; already generated the sequence of all possible ways to place k - 1 queens in
;; the first k - 1 columns of the board. For each of these ways, generate an
;; extended set of positions by placing a queen in each row of the kth column.
;; Now filter these, keeping only the positions for which the queen in the k th
;; column is safe with respect to the other queens. This produces the sequence
;; of all ways to place k queens in the first k columns. By continuing this
;; process, we will produce not only one solution, but all solutions to the
;; puzzle.
;;
;; ┌───┬───┬───┬───┬───┬───┬───┬───┐
;; │   │   │   │   │   │ ♛ │   │   │
;; ├───┼───┼───┼───┼───┼───┼───┼───┤
;; │   │   │ ♛ │   │   │   │   │   │
;; ├───┼───┼───┼───┼───┼───┼───┼───┤
;; │ ♛ │   │   │   │   │   │   │   │
;; ├───┼───┼───┼───┼───┼───┼───┼───┤
;; │   │   │   │   │   │   │ ♛ │   │
;; ├───┼───┼───┼───┼───┼───┼───┼───┤
;; │   │   │   │   │ ♛ │   │   │   │
;; ├───┼───┼───┼───┼───┼───┼───┼───┤
;; │   │   │   │   │   │   │   │ ♛ │
;; ├───┼───┼───┼───┼───┼───┼───┼───┤
;; │   │ ♛ │   │   │   │   │   │   │
;; ├───┼───┼───┼───┼───┼───┼───┼───┤
;; │   │   │   │ ♛ │   │   │   │   │
;; └───┴───┴───┴───┴───┴───┴───┴───┘
;;
;; We implement this solution as a procedure queens, which returns a sequence
;; of all solutions to the problem of placing n queens on an n by n chessboard.
;; queens has an internal procedure queen-cols that returns the sequence of all
;; ways to place queens in the first k columns of the board.

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; In this procedure rest-of-queens is a way to place k - 1 queens in the first
;; k - 1 columns, and new-row is a proposed row in which to place the queen for
;; the kth column. Complete the program by implementing the representation for
;; sets of board positions, including the procedure adjoin-position, which
;; adjoins a new row-column position to a set of positions, and empty-board,
;; which represents an empty set of positions. You must also write the
;; procedure safe?, which determines for a set of positions, whether the queen
;; in the kth column is safe with respect to the others. (Note that we need
;; only check whether the new queen is safe---the other queens are already
;; guaranteed safe with respect to each other.)

(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))
(define (safe? k positions)
  (define (iter r d1 d2 head)
    (or (null? head)
        (let ((v (car head)))
          (and (not (or (= v r) (= v d1) (= v d2)))
               (iter r (+ d1 1) (- d2 1) (cdr head))))))
  (let ((r (car positions))
        (head (cdr positions)))
    (iter r (+ r 1) (- r 1) head)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.43
;;
;; Louis Reasoner is having a terrible time doing Exercise 2.42. His queens
;; procedure seems to work, but it runs extremely slowly. (Louis never does
;; manage to wait long enough for it to solve even the 6 by 6 case.) When Louis
;; asks Eva Lu Ator for help, she points out that he has interchanged the order
;; of the nested mappings in the flatmap, writing it as

(flatmap
  (lambda (new-row)
    (map (lambda (rest-of-queens)
           (adjoin-position new-row k rest-of-queens))
         (queen-cols (- k 1))))
  (enumerate-interval 1 board-size))

;; Explain why this interchange makes the program run slowly. Estimate how long
;; it will take Louis's program to solve the eight-queens puzzle, assuming that
;; the program in Exercise 2.42 solves the puzzle in time T.

;; The original implementation generates a linear recursive process where
;; queen-cols calls itself at most once, resulting in board-size total number
;; of calls. In the changed version, queen-cols calls itself board-size times
;; resulting in board-size^board-size total number of calls.
;;
;; If for board-size = 8, the time it takes for the original queens to complete
;; is T, then it takes T/8 to call queen-cols once. Since the new version calls
;; queen-cols 8^8 times, the time is is (8^8)/8 * T = 8^7 * T
;; ================================================================================



;; ================================================================================
;; Exercise 2.44
;;
;; Define the procedure up-split used by corner-split. It is similar to
;; right-split, except that it switches the roles of below and beside.

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.45
;;
;; right-split and up-split can be expressed as instances of a general
;; splitting operation. Define a procedure split with the property that
;; evaluating

(define right-split (split beside below))
(define up-split (split below beside))

;; produces procedures right-split and up-split with the same behaviors as the
;; ones already defined.

(define (split tr1 tr2)
  (define (do-split painter n)
    (if (= n 0)
      painter
      (let ((smaller (do-split painter (- n 1))))
        (tr1 painter (tr2 smaller smaller)))))
  do-split)
;; ================================================================================



;; ================================================================================
;; Exercise 2.46
;;
;; A two-dimensional vector v running from the origin to a point can be
;; represented as a pair consisting of an x-coordinate and a y-coordinate.
;; Implement a data abstraction for vectors by giving a constructor make-vect
;; and corresponding selectors xcor-vect and ycor-vect. In terms of your
;; selectors and constructor, implement procedures add-vect, sub-vect, and
;; scale-vect that perform the operations vector addition, vector subtraction,
;; and multiplying a vector by a scalar:
;;
;; (x_1, y_1) + (x_2, y_2) = (x_1 + x_2, y_1 + y_2)
;; (x_1, y_1) - (x_2, y_2) = (x_1 - x_2, y_1 - y_2)
;; s * (x, y) = (s * x, s * y)

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect x y)
  (make-vect (+ (xcor-vect x) (xcor-vect y))
             (+ (ycor-vect x) (ycor-vect y))))

(define (sub-vect x y)
  (make-vect (- (xcor-vect x) (xcor-vect y))
             (- (ycor-vect x) (ycor-vect y))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.47
;;
;; Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;; For each constructor supply the appropriate selectors to produce an
;; implementation for frames.

(define origin-frame car)

(define edge1-frame cadr)

(define edge2-frame caddr)
(define edge2-frame cddr)
;; ================================================================================



;; ================================================================================
;; Exercise 2.48
;;
;; A directed line segment in the plane can be represented as a pair of
;; vectors---the vector running from the origin to the start-point of the
;; segment, and the vector running from the origin to the end-point of the
;; segment. Use your vector representation from Exercise 2.46 to define a
;; representation for segments with a constructor make-segment and selectors
;; start-segment and end-segment.

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
;; ================================================================================



;; ================================================================================
;; Exercise 2.49
;;
;; Use segments->painter to define the following primitive painters:
;;
;; a. The painter that draws the outline of the designated frame.
;;
;; b. The painter that draws an "X" by connecting opposite corners of the frame.
;;
;; c. The painter that draws a diamond shape by connecting the midpoints of the
;; sides of the frame.
;;
;; d. The wave painter.

(define (connected-segments . points)
  (define (connect points)
    (if (or (null? points) (null? (cdr points)))
      '()
      (cons (make-segment (car points) (cadr points))
            (connect (cdr points)))))
  (fold-right append '() (map connect points)))

(define outline->painter
  (segments->painter
    (connected-segments
      (list (make-vect 0.0 0.0)
            (make-vect 0.0 1.0)
            (make-vect 1.0 1.0)
            (make-vect 1.0 0.0)
            (make-vect 0.0 0.0)))))

(define x->painter
  (let ((a (make-vect 0.0 0.0))
        (b (make-vect 0.0 1.0))
        (c (make-vect 1.0 1.0))
        (d (make-vect 1.0 0.0)))
    (segments->painter
      (list
        (make-segment a c)
        (make-segment b d)))))

(define diamond->painter
  (segments->painter
    (connected-segments
      (list
        (make-vect 0.0 0.5)
        (make-vect 0.5 1.0)
        (make-vect 1.0 0.5)
        (make-vect 0.5 0.0)
        (make-vect 0.0 0.5)))))

(define wave->painter
  (segments->painter
    (connected-segments
      (list
        (make-vect 0.0 0.8)
        (make-vect 0.15 0.61)
        (make-vect 0.3 0.65)
        (make-vect 0.4 0.67)
        (make-vect 0.35 0.87)
        (make-vect 0.38 1.0))
      (list
        (make-vect 0.6 1.0)
        (make-vect 0.65 0.87)
        (make-vect 0.62 0.67)
        (make-vect 0.8 0.67)
        (make-vect 1.0 0.40))
      (list
        (make-vect 0.0 0.65)
        (make-vect 0.16 0.45)
        (make-vect 0.31 0.58)
        (make-vect 0.38 0.52)
        (make-vect 0.34 0.0))
      (list
        (make-vect 0.48 0.0)
        (make-vect 0.55 0.3)
        (make-vect 0.62 0.0))
      (list
        (make-vect 0.75 0.0)
        (make-vect 0.65 0.48)
        (make-vect 1.0 0.2)))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.50
;;
;; Define the transformation flip-horiz, which flips painters horizontally, and
;; transformations that rotate painters counterclockwise by 180 degrees and 270
;; degrees.

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.51
;;
;; Define the below operation for painters. below takes two painters as
;; arguments. The resulting painter, given a frame, draws with the first
;; painter in the bottom of the frame and with the second painter in the top.
;; Define below in two different ways---first by writing a procedure that is
;; analogous to the beside procedure given above, and again in terms of beside
;; and suitable rotation operations (from Exercise 2.50).

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              (make-vect 1.0 0.0)
              split-point))
          (paint-top
            (transform-painter
              painter2
              split-point
              (make-vect 1.0 0.5)
              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.52
;;
;; Make changes to the square limit of wave shown in Figure 2.9 by working at
;; each of the levels described above. In particular:
;;
;; a. Add some segments to the primitive wave painter of Exercise 2.49 (to add
;; a smile, for example).
;;
;; b. Change the pattern constructed by corner-split (for example, by using
;; only one copy of the up-split and right-split images instead of two).
;;
;; c. Modify the version of square-limit that uses square-of-four so as to
;; assemble the corners in a different pattern. (For example, you might make
;; the big Mr. Rogers look outward from each corner of the square.)

;; a
(define wave->painter
  (segments->painter
    (connected-segments
      (list
        (make-vect 0.4 0.78)
        (make-vect 0.5 0.73)
        (make-vect 0.6 0.78))
      (list
        (make-vect 0.0 0.8)
        (make-vect 0.15 0.61)
        (make-vect 0.3 0.65)
        (make-vect 0.4 0.67)
        (make-vect 0.35 0.87)
        (make-vect 0.38 1.0))
      (list
        (make-vect 0.6 1.0)
        (make-vect 0.65 0.87)
        (make-vect 0.62 0.67)
        (make-vect 0.8 0.67)
        (make-vect 1.0 0.40))
      (list
        (make-vect 0.0 0.65)
        (make-vect 0.16 0.45)
        (make-vect 0.31 0.58)
        (make-vect 0.38 0.52)
        (make-vect 0.34 0.0))
      (list
        (make-vect 0.48 0.0)
        (make-vect 0.55 0.3)
        (make-vect 0.62 0.0))
      (list
        (make-vect 0.75 0.0)
        (make-vect 0.65 0.48)
        (make-vect 1.0 0.2)))))

;; b
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1)))
          (corner (corner-split painter (- n 1))))
      (beside (below painter up)
              (below right corner)))))

;; c
(define (identity x) x)
(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.53
;;
;; What would the interpreter print in response to evaluating each of the
;; following expressions?

(list 'a 'b 'c)
;; (a b c)

(list (list 'george))
;; ((george))

(cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;; (y1 y2)

(pair? (car '(a short list)))
;; #f

(memq 'red '((red shoes) (blue socks)))
;; #f

(memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

;; ================================================================================



;; ================================================================================
;; Exercise 2.54
;;
;; Two lists are said to be equal? if they contain equal elements arranged in
;; the same order. For example,

(equal? '(this is a list) '(this is a list))

;; is true, but

(equal? '(this is a list) '(this (is a) list))

;; is false. To be more precise, we can define equal? recursively in terms of
;; the basic eq? equality of symbols by saying that a and b are equal? if they
;; are both symbols and the symbols are eq?, or if they are both lists such
;; that (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b). Using
;; this idea, implement equal? as a procedure.

(define (equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        (else #f)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.55
;;
;; Eva Lu Ator types to the interpreter the expression

(car ''abracadabra)

;; To her surprise, the interpreter prints back quote. Explain.

(car ''abracadabra)
(car '(quote abracadabra))
'quote
;; ================================================================================



;; ================================================================================
;; Exercise 2.56

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; Show how to extend the basic differentiator to handle more kinds of
;; expressions. For instance, implement the differentiation rule
;;
;; d(u^n)                 du
;; ------ = n * u^(n-1) * --
;;   dx                   dx
;;
;; by adding a new clause to the deriv program and defining appropriate
;; procedures exponentiation?, base, exponent, and make-exponentiation. (You
;; may use the symbol ** to denote exponentiation.) Build in the rules that
;; anything raised to the power 0 is 1 and anything raised to the power 1 is
;; the thing itself.

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define base cadr)
(define exponent caddr)

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum
           (deriv (addend exp) var)
           (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product
             (multiplier exp)
             (deriv (multiplicand exp) var))
           (make-product
             (deriv (multiplier exp) var)
             (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (exponent exp)
           (make-product
             (make-exponentiation (base exp) (- (exponent exp) 1))
             (deriv (base exp) var))))
        (else
          (error "unknown expression type: DERIV" exp))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.57
;;
;; Extend the differentiation program to handle sums and products of arbitrary
;; numbers of (two or more) terms. Then the last example above could be
;; expressed as
;;
;; (deriv '(* x y (+ x 3)) 'x)
;;
;; Try to do this by changing only the representation for sums and products,
;; without changing the deriv procedure at all. For example, the addend of a
;; sum would be the first term, and the augend would be the sum of the rest of
;; the terms.

(define (make-sum . terms)
  (define (flatten terms)
    (fold-right append '() (map (lambda (t) (if (sum? t) (cdr t) (list t))) terms)))
  (define (add-constants const filtered terms)
    (cond ((null? terms)
           (cons const filtered))
          ((number? (car terms))
           (add-constants (+ const (car terms)) filtered (cdr terms)))
          (else
            (add-constants const (cons (car terms) filtered) (cdr terms)))))
  (let ((normalized (add-constants 0 '() (flatten terms))))
    (let ((const (car normalized))
          (rest (cdr normalized)))
      (cond ((null? rest) const)
            ((= 0 const) (if (null? (cdr rest)) (car rest) (cons '+ rest)))
            (else (cons '+ normalized))))))

(define (addend s) (cadr s))
(define (augend s) (if (null? (cdddr s)) (caddr s) (cons '+ (cddr s))))

(define (make-product . terms)
  (define (flatten terms)
    (fold-right append '() (map (lambda (t) (if (product? t) (cdr t) (list t))) terms)))
  (define (mul-constants const filtered terms)
    (cond ((null? terms)
           (cons const filtered))
          ((=number? (car terms) 0) (cons 0 '()))
          ((number? (car terms))
           (mul-constants (* const (car terms)) filtered (cdr terms)))
          (else
            (mul-constants const (cons (car terms) filtered) (cdr terms)))))
  (let ((normalized (mul-constants 1 '() (flatten terms))))
    (let ((const (car normalized))
          (rest (cdr normalized)))
      (cond ((null? rest) const)
            ((= 1 const) (if (null? (cdr rest)) (car rest) (cons '* rest)))
            (else (cons '* normalized))))))

(define (multiplier p) (cadr p))
(define (multiplicand p) (if (null? (cdddr p)) (caddr p) (cons '* (cddr p))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.58
;;
;; Suppose we want to modify the differentiation program so that it works with
;; ordinary mathematical notation, in which + and * are infix rather than
;; prefix operators. Since the differentiation program is defined in terms of
;; abstract data, we can modify it to work with different representations of
;; expressions solely by changing the predicates, selectors, and constructors
;; that define the representation of the algebraic expressions on which the
;; differentiator is to operate.
;;
;; a. Show how to do this in order to differentiate algebraic expressions
;; presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the
;; task, assume that + and * always take two arguments and that expressions are
;; fully parenthesized.
;;
;; b. The problem becomes substantially harder if we allow standard algebraic
;; notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses
;; and assumes that multiplication is done before addition. Can you design
;; appropriate predicates, selectors, and constructors for this notation such
;; that our derivative program still works?

;; a
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x) (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

;; b
(define (priority-cmp op1 op2)
  (cond ((eq? op1 op2) 0)
        ((and (eq? op1 '+) (eq? op2 '*)) -1)
        ((and (eq? op1 '*) (eq? op2 '+)) 1)
        (else (error "Unsupported operator"))))

(define (lowest-priority-op expr)
  (define (trim-parens expr) (if (null? (cdr expr)) (car expr) expr))
  (define (iter res lhs op rhs)
    (let ((res (if (or (null? res) (< (priority-cmp op (car res)) 0))
                 (cons op (list lhs rhs))
                 res))
          (next (cdr rhs)))
      (if (null? next)
        res
        (iter res (append lhs (list op (car rhs))) (car next) (cdr next)))))
  (let ((res (iter '() (list (car expr)) (cadr expr) (cddr expr))))
    (let ((op (car res))
          (lhs (cadr res))
          (rhs (caddr res)))
      (list op (trim-parens lhs) (trim-parens rhs)))))

(define (make-sum a1 a2)
  (define (flatten head)
    (fold-right append '() (map (lambda (t) (if (pair? t) t (list t))) head)))
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (flatten (list a1 '+ a2)))))

(define (make-product m1 m2)
  (define (flatten head)
    (fold-right append '() (map (lambda (t) (if (product? t) t (list t))) head)))
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (flatten (list m1 '* m2)))))

(define (sum? x)
  (and (pair? x) (eq? '+ (car (lowest-priority-op x)))))
(define (addend s) (cadr (lowest-priority-op s)))
(define (augend s) (caddr (lowest-priority-op s)))

(define (product? x)
  (and (pair? x) (eq? '* (car (lowest-priority-op x)))))
(define (multiplier p) (cadr (lowest-priority-op p)))
(define (multiplicand p) (caddr (lowest-priority-op p)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.59

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Implement the union-set operation for the unordered-list representation of
;; sets.

(define (union-set set1 set2)
  (if (null? set1)
    set2
    (adjoin-set (car set1) (union-set (cdr set1) set2))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.60
;;
;; We specified that a set would be represented as a list with no duplicates.
;; Now suppose we allow duplicates. For instance, the set {1, 2, 3} could be
;; represented as the list (2 3 2 1 3 2 2). Design procedures element-of-set?,
;; adjoin-set, union-set, and intersection-set that operate on this
;; representation. How does the efficiency of each compare with the
;; corresponding procedure for the non-duplicate representation? Are there
;; applications for which you would use this representation in preference to
;; the non-duplicate one?

;; unchanged
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; unchanged
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define adjoin-set cons)
(define union-set append)

;; element-of-set? is unchanged and still Theta(n), but the actual performance
;; may be worse since the length of the list representing the same set may be
;; much longer
;;
;; intersection-set is also unchanged and still Theta(n^2). The actual
;; performance may be worse for the same reason
;;
;; adjoin-set is now Theta(1) since it doesn't have to check if the element
;; being added is already in the set
;;
;; union-set is now Theta(n) since it executes n adjoin-set operations, each of
;; which executes in constant time
;;
;; Conclusion: new representation is better in situations where we mostly add
;; elements to sets and do unions.
;; ================================================================================



;; ================================================================================
;; Exercise 2.61
;;
;; Give an implementation of adjoin-set using the ordered representation. By
;; analogy with element-of-set? show how to take advantage of the ordering to
;; produce a procedure that requires on the average about half as many steps as
;; with the unordered representation.

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
        (else set)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.62
;;
;; Give a Theta(n) implementation of union-set for sets represented as ordered
;; lists.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set set1 (cdr set2))))
        (else (cons (car set1) (union-set (cdr set1) (cdr set2))))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.63

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define tree1 (make-tree
                7
                (make-tree 3
                           (make-tree 1 '() '())
                           (make-tree 5 '() '()))
                (make-tree 9
                           '()
                           (make-tree 11 '() '()))))
(define tree2 (make-tree
                3
                (make-tree 1 '() '())
                (make-tree 7
                           (make-tree 5 '() '())
                           (make-tree 9
                                      '()
                                      (make-tree 11 '() '())))))
(define tree3 (make-tree
                5
                (make-tree 3
                           (make-tree 1 '() '())
                           '())
                (make-tree 9
                           (make-tree 7 '() '())
                           (make-tree 11 '() '()))))

;; Each of the following two procedures converts a binary tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;; a. Do the two procedures produce the same result for every tree? If not, how
;; do the results differ? What lists do the two procedures produce for the
;; trees in Figure 2.16?
;;
;; b. Do the two procedures have the same order of growth in the number of
;; steps required to convert a balanced tree with n elements to a list? If not,
;; which one grows more slowly?

;; Results are the same
(tree->list-1 tree1)
;; (1 3 5 7 9 11)
(tree->list-1 tree2)
;; (1 3 5 7 9 11)
(tree->list-1 tree3)
;; (1 3 5 7 9 11)
(tree->list-2 tree1)
;; (1 3 5 7 9 11)
(tree->list-2 tree2)
;; (1 3 5 7 9 11)
(tree->list-2 tree3)
;; (1 3 5 7 9 11)

;; Both procedures recursively call themselves twice (once for each subtree)
;; per call to create sublists. However, in order to join the lists together,
;; the first procedure uses append (which is Theta(n)) whereas the second
;; procedure uses cons (which is Theta(1)).
;;
;; Therefore, both procedures will be called n times where n is the number of
;; nodes in the tree. The first procedure calls append n times which results in
;; Theta(n^2) and the second procedure will call cons n times which results in
;; Theta(n)
;; ================================================================================



;; ================================================================================
;; Exercise 2.64
;;
;; The following procedure list->tree converts an ordered list to a balanced
;; binary tree. The helper procedure partial-tree takes as arguments an integer
;; n and list of at least n elements and constructs a balanced tree containing
;; the first n elements of the list. The result returned by partial-tree is a
;; pair (formed with cons) whose car is the constructed tree and whose cdr is
;; the list of elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result
              (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result
                  (partial-tree
                    (cdr non-left-elts)
                    right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts
                    (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               right-tree)
                    remaining-elts))))))))

;; a. Write a short paragraph explaining as clearly as you can how partial-tree
;; works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).
;;
;; b. What is the order of growth in the number of steps required by list->tree
;; to convert a list of n elements?

;; In a balanced tree, the root entry will be the median element of the source
;; list. The left subtree will be a balanced tree created from the elements of
;; the source list to the left of the median. The right subtree will be created
;; from the elements to the right of the median. If the source list is empty,
;; the tree is also empty.
;;
;; This gives a simple recursive procedure:
;; 1. If the source list is empty, return an empty tree
;; 2. Otherwise, pick the median element from the source list
;; 3. Recursively build a balanced tree from the sublist to the left of the
;; median
;; 4. Recursively build a balanced tree from the sublist to the right of the
;; median
;; 5. Combine the median, the left subtree, and the right subtree
;;
;; For example:
;; [1 3 5 7 9 11]   <- source list
;; [1 3] 5 [7 9 11] <- left sublist, median, right sublist
;; [1 3]
;; [] 1 [3]
;; ()               <- empty tree
;; [3]
;; [] 3 []
;; ...              <- skipping calls on empty lists
;; (3 () ())        <- combined median and left/right subtrees
;; (1 () (3 () ()))
;; [7 9 11]
;; [7] 9 [11]
;; ...
;; (9 (7 () ()) (11 () ()))
;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;;
;;     5
;; 𜰵      𜰶
;; 1       9
;;  𜰶    𜰵  𜰶
;;   3  7   11
;;
;;
;; The order of growth is Theta(n) since partial-tree is called once for each
;; median element to create one node
;; ================================================================================



;; ================================================================================
;; Exercise 2.65
;;
;; Use the results of Exercise 2.63 and Exercise 2.64 to give Theta(n)
;; implementations of union-set and intersection-set for sets implemented as
;; (balanced) binary trees.

(define (union-set set1 set2)
  (define (ordered-list-union list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          ((< (car list1) (car list2))
           (cons (car list1) (ordered-list-union (cdr list1) list2)))
          ((> (car list1) (car list2))
           (cons (car list2) (ordered-list-union list1 (cdr list2))))
          (else (cons (car list1) (ordered-list-union (cdr list1) (cdr list2))))))
  (list->tree (ordered-list-union (tree->list-2 set1) (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (define (ordered-list-intersection list1 list2)
    (cond ((null? list1) '())
          ((null? list2) '())
          ((< (car list1) (car list2))
           (ordered-list-intersection (cdr list1) list2))
          ((> (car list1) (car list2))
           (ordered-list-intersection list1 (cdr list2)))
          (else (cons (car list1)
                      (ordered-list-intersection (cdr list1) (cdr list2))))))
  (list->tree (ordered-list-intersection (tree->list-2 set1) (tree->list-2 set2))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.66
;;
;; Implement the lookup procedure for the case where the set of records is
;; structured as a binary tree, ordered by the numerical values of the keys.

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        (else (entry set-of-records))))

(define make-record cons)
(define key car)
(define set-of-records
  (make-tree
    (make-record 5 'Five)
    (make-tree
      (make-record 3 'Three)
      (make-tree
        (make-record 1 'One)
        '()
        '())
      '())
    (make-tree
      (make-record 9 'Nine)
      (make-tree
        (make-record 7 'Seven)
        '()
        '())
      (make-tree
        (make-record 11 'Eleven)
        '()
        '()))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.67

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)   ; symbol
                             (cadr pair)) ; frequency
                  (make-leaf-set (cdr pairs))))))

;; Define an encoding tree and a sample message:

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; Use the decode procedure to decode the message, and give the result.

(decode sample-message sample-tree)
;; (A D A B B C A)
;; ================================================================================



;; ================================================================================
;; Exercise 2.68
;;
;; The encode procedure takes as arguments a message and a tree and produces
;; the list of bits that gives the encoded message.

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

;; encode-symbol is a procedure, which you must write, that returns the list of
;; bits that encodes a given symbol according to a given tree. You should
;; design encode-symbol so that it signals an error if the symbol is not in the
;; tree at all. Test your procedure by encoding the result you obtained in
;; Exercise 2.67 with the sample tree and seeing whether it is the same as the
;; original sample message.

(define (encode-symbol symbol tree)
  (cond ((null? tree) (error "Empty tree"))
        ((leaf? tree)
         (if (eq? symbol (symbol-leaf tree)) '() (error "Not found")))
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error ("Not found")))))

(encode (decode sample-message sample-tree) sample-tree)              
;; (0 1 1 0 0 1 0 1 0 1 1 1 0)
sample-message
;; (0 1 1 0 0 1 0 1 0 1 1 1 0)
;; ================================================================================



;; ================================================================================
;; Exercise 2.69
;;
;; The following procedure takes as its argument a list of symbol-frequency
;; pairs (where no symbol appears in more than one pair) and generates a
;; Huffman encoding tree according to the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; make-leaf-set is the procedure given above that transforms the list of pairs
;; into an ordered set of leaves. successive-merge is the procedure you must
;; write, using make-code-tree to successively merge the smallest-weight
;; elements of the set until there is only one element left, which is the
;; desired Huffman tree. (This procedure is slightly tricky, but not really
;; complicated. If you find yourself designing a complex procedure, then you
;; are almost certainly doing something wrong. You can take significant
;; advantage of the fact that we are using an ordered set representation.)

(define (successive-merge trees)
  (cond ((null? trees) (error "Empty tree set"))
        ((null? (cdr trees)) (car trees))
        (else (successive-merge
                (adjoin-set
                  (make-code-tree (car trees) (cadr trees))
                  (cddr trees))))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.70
;;
;; The following eight-symbol alphabet with associated relative frequencies was
;; designed to efficiently encode the lyrics of 1950s rock songs. (Note that
;; the "symbols" of an "alphabet" need not be individual letters.)
;;
;; A    2
;; GET  2
;; SHA  3
;; WAH  1
;; BOOM 1
;; JOB  2
;; NA   16
;; YIP  9
;;
;; Use generate-huffman-tree (Exercise 2.69) to generate a corresponding
;; Huffman tree, and use encode (Exercise 2.68) to encode the following
;; message:
;;
;; Get a job
;; Sha na na na na na na na na
;; Get a job
;; Sha na na na na na na na na
;; Wah yip yip yip yip yip yip yip yip yip
;; Sha boom
;;
;; How many bits are required for the encoding? What is the smallest number of
;; bits that would be needed to encode this song if we used a fixed-length code
;; for the eight-symbol alphabet?

(define song-pairs '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define song-tree (generate-huffman-tree song-pairs))

(define song '(
               GET A JOB
               SHA NA NA NA NA NA NA NA NA
               GET A JOB
               SHA NA NA NA NA NA NA NA NA
               WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
               SHA BOOM))

(length (encode song song-tree))
;; 84

(* (length song)
   (ceiling (/ (log (length song-pairs)) (log 2))))
;; 108
;; ================================================================================



;; ================================================================================
;; Exercise 2.71
;;
;; Suppose we have a Huffman tree for an alphabet of n symbols, and that the
;; relative frequencies of the symbols are 1, 2, 4, ..., 2^(n-1). Sketch the
;; tree for n = 5; for n = 10. In such a tree (for general n) how many bits are
;; required to encode the most frequent symbol? The least frequent symbol?
;;
;; {(A 1) (B 2) (C 4) (D 8) (E 16)}
;; {({A B} 3) (C 4) (D 8) (E 16)}
;; {({A B C} 7) (D 8) (E 16)}
;; {({A B C D} 15) (E 16)}
;; {({A B C D E} 31)}
;;
;; {(A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)}
;; {({A B} 3) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)}
;; {({A B C} 7) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)}
;; {({A B C D} 15) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)}
;; {({A B C D E} 31) (F 32) (G 64) (H 128) (I 256) (J 512)}
;; {({A B C D E F} 63) (G 64) (H 128) (I 256) (J 512)}
;; {({A B C D E F G} 127) (H 128) (I 256) (J 512)}
;; {({A B C D E F G H} 255) (I 256) (J 512)}
;; {({A B C D E F G H I} 511) (J 512)}
;; {({A B C D E F G H I J} 1023)}
;;
;; 1 bit for the most frequent symbol
;; n - 1 bits for the least frequent symbol
;; ================================================================================



;; ================================================================================
;; Exercise 2.72
;;
;; Consider the encoding procedure that you designed in Exercise 2.68. What is
;; the order of growth in the number of steps needed to encode a symbol? Be
;; sure to include the number of steps needed to search the symbol list at each
;; node encountered. To answer this question in general is difficult. Consider
;; the special case where the relative frequencies of the n symbols are as
;; described in Exercise 2.71, and give the order of growth (as a function of
;; n) of the number of steps needed to encode the most frequent and least
;; frequent symbols in the alphabet.

(define (encode-symbol symbol tree)
  (cond ((null? tree) (error "Empty tree"))
        ((leaf? tree)
         (if (eq? symbol (symbol-leaf tree)) '() (error "Not found")))
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error ("Not found")))))

;; Best case: the most frequent symbol takes 1 bit to encode so we only search
;; the symbol list of both branches once, which takes Theta(n) steps
;;
;; Worst case: the least frequent symbol takes (n - 1) bits to encode so we
;; search the symbol list of both branches (Theta(n) steps) (n - 1) times which
;; results in Theta(n^2) total steps
;; ================================================================================



;; ================================================================================
;; Exercise 2.73
;;
;; Section 2.3.2 described a program that performs symbolic differentiation:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product
                     (multiplier exp)
                     (deriv (multiplicand exp) var))
                   (make-product
                     (deriv (multiplier exp) var)
                     (multiplicand exp))))
        ;; ⟨more rules can be added here⟩
        (else (error "unknown expression type: DERIV" exp))))

;; We can regard this program as performing a dispatch on the type of the
;; expression to be differentiated. In this situation the "type tag" of the
;; datum is the algebraic operator symbol (such as +) and the operation being
;; performed is deriv. We can transform this program into data-directed style
;; by rewriting the basic derivative procedure as

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a. Explain what was done above. Why can't we assimilate the predicates
;; number? and variable? into the data-directed dispatch?
;;
;; b. Write the procedures for derivatives of sums and products, and the
;; auxiliary code required to install them in the table used by the program
;; above.
;;
;; c. Choose any additional differentiation rule that you like, such as the one
;; for exponents (Exercise 2.56), and install it in this data-directed system.
;;
;; d. In this simple algebraic manipulator the type of an expression is the
;; algebraic operator that binds it together. Suppose, however, we indexed the
;; procedures in the opposite way, so that the dispatch line in deriv looked
;; like
;;
;; ((get (operator exp) 'deriv) (operands exp) var)
;;
;; What corresponding changes to the derivative system are required?

;; a. Explicit dispatch by calling sum? and product? was replaced by looking up
;; the appropriate differentiation procedure in a dispatch table. The same
;; can't be done for number? and variable? because there is no operator to
;; dispatch on
;;
;; b, c
;; Deps
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

;; Mock table, uses at the moment unexplained set!
(define deriv-table
  (let ((entries '()))
    (define (equal? a b)
      (if (and (pair? a) (pair? b))
        (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (eq? a b)))
    (define (find op type entries)
      (cond ((null? entries) #f)
            ((and (eq? op (caar entries)) (equal? type (cadar entries)))
             (caddar entries))
            (else (find op type (cdr entries)))))
    (cons 
      (lambda (op type item) (set! entries (cons (list op type item) entries)))
      (lambda (op type) (find op type entries)))))
(define put (car deriv-table))
(define get (cdr deriv-table))

;; Solution
(define (install-addition-deriv-rule)
  (define addend car)
  (define augend cadr)
  (define (deriv-sum expr var)
    (make-sum (deriv (addend expr) var)
              (deriv (augend expr) var)))
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-multiplication-deriv-rule)
  (define multiplier car)
  (define multiplicand cadr)
  (define (deriv-product expr var)
    (make-sum (make-product
                (multiplier expr)
                (deriv (multiplicand expr) var))
              (make-product
                (deriv (multiplier expr) var)
                (multiplicand expr))))
  (put 'deriv '* deriv-product)
  'done)

(define (install-exponentiation-deriv-rule)
  (define base car)
  (define exponent cadr)
  (define (deriv-exponentiation expr var)
    (make-product
      (exponent expr)
      (make-product
        (make-exponentiation (base expr) (- (exponent expr) 1))
        (deriv (base expr) var))))
  (put 'deriv '** deriv-exponentiation)
  'done)

;; d. Swap the first 2 arguments of every put call
;; ================================================================================



;; ================================================================================
;; Exercise 2.74
;;
;; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company
;; consisting of a large number of independent divisions located all over the
;; world. The company's computer facilities have just been interconnected by
;; means of a clever network-interfacing scheme that makes the entire network
;; appear to any user to be a single computer. Insatiable's president, in her
;; first attempt to exploit the ability of the network to extract
;; administrative information from division files, is dismayed to discover
;; that, although all the division files have been implemented as data
;; structures in Scheme, the particular data structure used varies from
;; division to division. A meeting of division managers is hastily called to
;; search for a strategy to integrate the files that will satisfy headquarters'
;; needs while preserving the existing autonomy of the divisions.
;;
;; Show how such a strategy can be implemented with data-directed programming.
;; As an example, suppose that each division's personnel records consist of a
;; single file, which contains a set of records keyed on employees' names. The
;; structure of the set varies from division to division. Furthermore, each
;; employee's record is itself a set (structured differently from division to
;; division) that contains information keyed under identifiers such as address
;; and salary. In particular:
;;
;; a. Implement for headquarters a get-record procedure that retrieves a
;; specified employee's record from a specified personnel file. The procedure
;; should be applicable to any division's file. Explain how the individual
;; divisions' files should be structured. In particular, what type information
;; must be supplied?
;;
;; b. Implement for headquarters a get-salary procedure that returns the salary
;; information from a given employee's record from any division's personnel
;; file. How should the record be structured in order to make this operation
;; work?
;;
;; c. Implement for headquarters a find-employee-record procedure. This should
;; search all the divisions' files for the record of a given employee and
;; return the record. Assume that this procedure takes as arguments an
;; employee's name and a list of all the divisions' files.
;;
;; d. When Insatiable takes over a new company, what changes must be made in
;; order to incorporate the new personnel information into the central system?

;; Mock table, uses at the moment unexplained set!
(define insatiable-inc-table
  (let ((entries '()))
    (define (equal? a b)
      (if (and (pair? a) (pair? b))
        (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (eq? a b)))
    (define (find op type entries)
      (cond ((null? entries) #f)
            ((and (eq? op (caar entries)) (equal? type (cadar entries)))
             (caddar entries))
            (else (find op type (cdr entries)))))
    (cons 
      (lambda (op type item) (set! entries (cons (list op type item) entries)))
      (lambda (op type) (find op type entries)))))
(define put (car insatiable-inc-table))
(define get (cdr insatiable-inc-table))

;; Each division file should use the format (cons tag records) where tag is the
;; division tag and records is an division-specific data structure.
;;
;; Each returned record should also use the format (cons tag data)

(define (install-division-a-record)
  (define (tag rec) (cons 'division-a rec))
  (define name car)
  (define salary cadr)
  (define (get-record employee records)
    (cond ((null? records) #f)
          ((eq? employee (name (car records))) (tag (car records)))
          (else (get-record employee (cdr records)))))
  (put 'get-record 'division-a get-record)
  (put 'get-salary 'division-a salary)
  'done)

(define get-tag car)
(define get-data cdr)

(define (get-record employee file)
  (let ((getter (get 'get-record (get-tag file))))
    (if getter
      (getter employee (get-data file))
      (error "Unknown operation and/or division"))))

(define (get-salary record)
  (let ((getter (get 'get-salary (get-tag record))))
    (if getter
      (getter (get-data record))
      (error "Unknown operation and/or division"))))

(define (find-employee-record employee files)
  (if (null? files)
    #f
    (let ((record (get-record employee (car files))))
      (or record (find-employee-record employee (cdr files))))))

;; Sample file
(define a-file (cons 'division-a (list (list 'JohnDoe 100500))))
(install-division-a-record)
(get-record 'JohnDoe a-file)

;; When a new company is added, they need to provide an
;; install-division-*-record procedure that inserts all the necessary internal
;; lookup procedures to the dispatch table
;; ================================================================================



;; ================================================================================
;; Exercise 2.75
;;
;; Implement the constructor make-from-mag-ang in message-passing style. This
;; procedure should be analogous to the make-from-real-imag procedure given
;; above.

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG_ANG" op))))
  dispatch)
;; ================================================================================



;; ================================================================================
;; Exercise 2.76
;;
;; As a large system with generic operations evolves, new types of data objects
;; or new operations may be needed. For each of the three strategies---generic
;; operations with explicit dispatch, data-directed style, and
;; message-passing-style---describe the changes that must be made to a system
;; in order to add new types or new operations. Which organization would be
;; most appropriate for a system in which new types must often be added? Which
;; would be most appropriate for a system in which new operations must often be
;; added?

;; Explicit dispatch:
;; To add a type, add new clauses to all generic operations, potentially change
;; the names of some concrete procedures, potentially change the constructors
;; to use the added type. To add an operation, implement it for all existing
;; types, add add a generic version that chooses what concrete procedure to
;; call.
;;
;; Data-directed style: 
;; To add a type, write and call an installation procedure. To add an
;; operation, implement it for all existing types and add a generic version
;; that looks up the concrete procedure in the dispatch table.
;;
;; Message-passing style:
;; To add a type, write write a dispatch procedure. To add an operation,
;; implement it for all existing types and add a generic version that passes
;; the appropriate message to the dispatch procedure.
;;
;; Adding new types is a bit easier with message-passing style as there is no
;; need for a separate installation procedure. Adding new operations may be
;; easier with data-directed style since it doesn't necessarily require
;; changing the type itself---the entry in the dispatch table can be added
;; externally.
;; ================================================================================



;; ================================================================================
;; Exercise 2.77
;;
;; Louis Reasoner tries to evaluate the expression (magnitude z) where z is the
;; object shown in Figure 2.24. To his surprise, instead of the answer 5 he
;; gets an error message from apply-generic, saying there is no method for the
;; operation magnitude on the types (complex). He shows this interaction to
;; Alyssa P. Hacker, who says "The problem is that the complex-number selectors
;; were never defined for complex numbers, just for polar and rectangular
;; numbers. All you have to do to make this work is add the following to the
;; complex package:"

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

;; Describe in detail why this works. As an example, trace through all the
;; procedures called in evaluating the expression (magnitude z) where z is the
;; object shown in Figure 2.24. In particular, how many times is apply-generic
;; in voked? What procedure is dispatched to in each case?

(magnitude z)
(apply-generic 'magnitude z)
    (map type-tag (list z)) ;; Somewhere inside apply-generic
    ;; '(complex)
    (get 'magnitude '(complex))
    ;; magnitude
    (apply magnitude (map contents (list z)))
    (magnitude (contents z))
    (apply-generic 'magnitude (contents z))
        (map type-tag (list (contents z))) ;; inside another apply-generic
        ;; '(rectangular)
        (get 'magnitude '(rectangular))
        ;; magnitude-internal
        (apply magnitude-internal (map (contents (list (contents z)))))
        (magnitude-internal (contents (contents z)))

;; In other words, magnitude was originally intended to work on arguments of
;; type 'rectangular and 'polar whereas z is of type 'complex. To make it work
;; on 'complex types, we could either manually strip the first type tag with
;; (magnitude (contents z)) or let apply-generic do this automatically by
;; adding an appropriate entry to the dispatch table
;; ================================================================================



;; ================================================================================
;; Exercise 2.78
;;
;; The internal procedures in the scheme-number package are essentially nothing
;; more than calls to the primitive procedures +, -, etc. It was not possible
;; to use the primitives of the language directly because our type-tag system
;; requires that each data object have a type attached to it. In fact, however,
;; all Lisp implementations do have a type system, which they use internally.
;; Primitive predicates such as symbol? and number? determine whether data
;; objects have particular types. Modify the definitions of type-tag, contents,
;; and attach-tag from Section 2.4.2 so that our generic system takes advantage
;; of Scheme's internal type system. That is to say, the system should work as
;; before except that ordinary numbers should be represented simply as Scheme
;; numbers rather than as pairs whose car is the symbol scheme-number.

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((and (pair? datum) (symbol? (car datum))) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((and (pair? datum) (symbol? (car datum))) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (if (and (eq? type-tag 'scheme-number) (number? contents))
    contents
    (cons type-tag contents)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.79
;;
;; Define a generic equality predicate equ? that tests the equality of two
;; numbers, and install it in the generic arithmetic package. This operation
;; should work for ordinary numbers, rational numbers, and complex numbers.

(define (equ? a b) (apply-generic 'equ? a b))

(define (install-equ-package)
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational)
       (lambda (a b) (= (* (numer a) (denom b))
                        (* (numer b) (denom a)))))
  (put 'equ? '(complex complex)
       ;; Initially, I thought to create separate equ? implementations for
       ;; rectangular and polar representations and dispatch on them, but 1)
       ;; this requires access to internal accessors that expect untagged data
       ;; (here, a and b are tagged with 'rectangular/'polar) and 2) it would
       ;; error for a pair of complex numbers with different representations
       (lambda (a b) (and (= (real-part a) (real-part b))
                          (= (imag-part a) (imag-part b))))
       'done)
  ;; ================================================================================



;; ================================================================================
;; Exercise 2.80
;;
;; Define a generic predicate =zero? that tests if its argument is zero, and
;; install it in the generic arithmetic package. This operation should work for
;; ordinary numbers, rational numbers, and complex numbers.

(define (=zero? a) (apply-generic '=zero? a))

(define (install-zero-package)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put '=zero? '(complex) (lambda (x) (= (magnitude x) 0)))
  'done)
;; ================================================================================



;; ================================================================================
;; Exercise 2.81

(define (create-table)
  (let ((entries '()))
    (define (equal? a b)
      (if (and (pair? a) (pair? b))
        (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (eq? a b)))
    (define (find op type entries)
      (cond ((null? entries) #f)
            ((and (eq? op (caar entries)) (equal? type (cadar entries)))
             (caddar entries))
            (else (find op type (cdr entries)))))
    (cons 
      (lambda (op type item) (set! entries (cons (list op type item) entries)))
      (lambda (op type) (find op type entries)))))

(define dispatch-table (create-table))
(define put (car dispatch-table))
(define get (cdr dispatch-table))

(define coercion-table (create-table))
(define put-coercion (car coercion-table))
(define get-coercion (cdr coercion-table))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                    (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))

;; Louis Reasoner has noticed that apply-generic may try to coerce the
;; arguments to each other's type even if they already have the same type.
;; Therefore, he reasons, we need to put procedures in the coercion table to
;; coerce arguments of each type to their own type. For example, in addition to
;; the scheme-number->complex coercion shown above, he would do:
;;
;; (define (scheme-number->scheme-number n) n)
;; (define (complex->complex z) z)
;; (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
;; (put-coercion 'complex 'complex complex->complex)
;;
;; a. With Louis's coercion procedures installed, what happens if apply-generic
;; is called with two arguments of type scheme-number or two arguments of type
;; complex for an operation that is not found in the table for those types? For
;; example, assume that we've defined a generic exponentiation operation:
;;
;; (define (exp x y) (apply-generic 'exp x y))
;;
;; and have put a procedure for exponentiation in the Scheme-number package but
;; not in any other package:
;;
;; ;; following added to Scheme-number package
;; (put 'exp '(scheme-number scheme-number)
;;      (lambda (x y) (tag (expt x y))))
;;     ; using primitive expt
;;
;; What happens if we call exp with two complex numbers as arguments?
;;
;; b. Is Louis correct that something had to be done about coercion with
;; arguments of the same type, or does apply-generic work correctly as is?
;;
;; c. Modify apply-generic so that it doesn't try coercion if the two arguments
;; have the same type.

;; a. The initial procedure lookup will fail and apply-generic will call itself
;; with the same arguments, the initial procedure lookup will fail and... The
;; result is infinite recursion

;; b. Louis has been consistently wrong about everything so far in this book so
;; I'd be surprised if he was correct this time.
;;
;; If the coercion table doesn't have entries for self-conversion, the
;; procedure will work correctly, albeit making two unnecessary lookups in the
;; coercion table. If the entry does exist, we'll get an infinite recursion
;; instead of an error which should be fixed.
;;
;; So Louis was right that something should be done here, but for the wrong
;; reasons since his "solution" is literally what triggers the problem.

;; c.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (and (= (length args) 2)
                 (not (eq? (car type-tags) (cadr type-tags))))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                    (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.82
;;
;; Show how to generalize apply-generic to handle coercion in the general case
;; of multiple arguments. One strategy is to attempt to coerce all the
;; arguments to the type of the first argument, then to the type of the second
;; argument, and so on. Give an example of a situation where this strategy (and
;; likewise the two-argument version given above) is not sufficiently general.
;; (Hint: Consider the case where there are some suitable mixed-type operations
;; present in the table that will not be tried.)

(define (find-coercions target types)
  (define (get-or-id type)
    (if (eq? type target)
      (lambda (x) x)
      (get-coercion type target)))
  (define (iter res types)
    (if (null? types)
      res
      (let ((to-target (get-or-id (car types))))
        (if to-target (iter (cons to-target res) (cdr types)) #f))))
  (let ((coercions (iter '() types)))
    (if coercions (reverse coercions) #f)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (try-coercions target-types)
      (if (null? target-types)
        (error "No method for these types" (list op type-tags))
        (let ((convertors (find-coercions (car target-types) type-tags))
              (proc (get op (map (lambda (t) (car target-types)) type-tags))))
          (if (and proc convertors)
            (apply proc (map contents (map apply convertors (map list args))))
            (try-coercions (cdr target-types))))))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (try-coercions type-tags)))))


;; First case where this procedure fails is when there is an operation that
;; takes arguments of different types. Since we coerce everything to a single
;; type, we will never find the correct coercions to apply to each argument.
;; For example, an operation expects types (A, B) but we pass (B, A). Even if
;; there are coercions (A -> B) and (B -> A), we will only try to pass (A, A)
;; and (B, B).
;;
;; Second case is when the operation we're trying to find expects a parameter
;; of a type not present in the arguments we supplied. For example, an
;; operation expects parameter (B) but we pass (A). Even if there's a coercion
;; (A -> B), we won't find it.
;; ================================================================================



;; ================================================================================
;; Exercise 2.83
;;
;; Suppose you are designing a generic arithmetic system for dealing with the
;; tower of types shown in Figure 2.25: integer, rational, real, complex. For
;; each type (except complex), design a procedure that raises objects of that
;; type one level in the tower. Show how to install a generic raise operation
;; that will work for each type (except complex).

;; Deps
(define (type-tag datum)
  (cond ((integer? datum) 'integer)
        ((number? datum) 'real)
        ((and (pair? datum) (symbol? (car datum))) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((and (pair? datum) (symbol? (car datum))) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (cond ((and (eq? type-tag 'integer) (integer? contents)) contents)
        ((and (eq? type-tag 'real) (number? contents))
         (if (integer? contents) (cons 'real (* 1.0 contents)) (* 1.0 contents)))
        (else (cons type-tag contents))))

(define (install-integer-package)
  (put 'add '(integer integer) +)
  (put 'sub '(integer integer) -)
  (put 'mul '(integer integer) *)
  (put 'div '(integer integer) quotient)
  (put 'make 'integer truncate)
  'done)

(define (install-real-package)
  (put 'add '(integer integer) +)
  (put 'sub '(integer integer) -)
  (put 'mul '(integer integer) *)
  (put 'div '(integer integer) /)
  (put 'make 'real (lambda (x) (attach-tag 'real x)))
  'done)

;; Solution
(define (install-raise-package)
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'raise '(rational) (lambda (x) (attach-tag 'real (/ (numer x) (denom x)))))
  (put 'raise '(real) (lambda (x) (make-complex-from-real-imag x 0)))
  'done)

(define (raise x) (apply-generic 'raise x))
;; ================================================================================



;; ================================================================================
;; Exercise 2.84
;;
;; Using the raise operation of Exercise 2.83, modify the apply-generic
;; procedure so that it coerces its arguments to have the same type by the
;; method of successive raising, as discussed in this section. You will need to
;; devise a way to test which of two types is higher in the tower. Do this in a
;; manner that is "compatible" with the rest of the system and will not lead to
;; problems in adding new levels to the tower.

(define (try-raise arg type)
  (if (eq? (type-tag arg) type)
    arg
    (let ((raise-proc (get 'raise (list (type-tag arg)))))
      (if raise-proc
        (try-raise (raise-proc (contents arg)) type)
        #f))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (and (= (length args) 2)
                 (not (eq? (car type-tags) (cadr type-tags))))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((a1-raised (try-raise a1 type2))
                  (a2-raised (try-raise a2 type1)))
              (cond (a1-raised (apply-generic op a1-raised a2))
                    (a2-raised (apply-generic op a1 a2-raised))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.85
;;
;; This section mentioned a method for "simplifying" a data object by lowering
;; it in the tower of types as far as possible. Design a procedure drop that
;; accomplishes this for the tower described in Exercise 2.83. The key is to
;; decide, in some general way, whether an object can be lowered. For example,
;; the complex number 1.5 + 0i can be lowered as far as real, the complex
;; number 1 + 0i can be lowered as far as integer, and the complex number 2 +
;; 3i cannot be lowered at all. Here is a plan for determining whether an
;; object can be lowered: Begin by defining a generic operation project that
;; "pushes" an object down in the tower. For example, projecting a complex
;; number would involve throwing away the imaginary part. Then a number can be
;; dropped if, when we project it and raise the result back to the type we
;; started with, we end up with something equal to what we started with. Show
;; how to implement this idea in detail, by writing a drop procedure that drops
;; an object as far as possible. You will need to design the various projection
;; operations and install project as a generic operation in the system. You
;; will also need to make use of a generic equality predicate, such as
;; described in Exercise 2.79. Finally, use drop to rewrite apply-generic from
;; Exercise 2.84 so that it "simplifies" its answers.

(define (tagged? x)
  (or (and (pair? x) (symbol? (car x)))
      (number? x)))

(define (install-equ-package)
  (put 'equ? '(integer integer) =)
  (put 'equ? '(rational rational)
       (lambda (a b) (= (* (numer a) (denom b))
                        (* (numer b) (denom a)))))
  (put 'equ? '(real real) =)
  (put 'equ? '(complex complex)
       (lambda (a b) (and (= (real-part a) (real-part b))
                          (= (imag-part a) (imag-part b)))))
  'done)

(define (install-project-package)
  (put 'project '(complex)
       (lambda (x) ((get 'make 'real) (real-part x))))
  (put 'project '(real)
       (lambda (x)
         (define (iter n d)
           (if (integer? n)
             (make-rational n d)
             (iter (* 10 n) (* 10 d))))
         (iter x 1)))
  (put 'project '(rational)
       (lambda (x) (round (/ (numer x) (denom x)))))
  'done)

(define (drop x)
  (if (tagged? x)
    (let ((project (get 'project (list (type-tag x)))))
      (if project
        (let ((dropped (project (contents x))))
          (if (equ? x (raise dropped)) ;; assume raise is available
            (drop dropped)
            x))
        x))
    x))

(define (apply-generic op . args)
  (define (drop-proc proc)
    (cond ((eq? op 'project) proc)
          ((eq? op 'raise) proc)
          (else (lambda args (drop (apply proc args))))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply (drop-proc proc) (map contents args))
        (if (and (= (length args) 2)
                 (not (eq? (car type-tags) (cadr type-tags))))
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((a1-raised (try-raise a1 type2))
                  (a2-raised (try-raise a2 type1)))
              (cond (a1-raised (apply-generic op a1-raised a2))
                    (a2-raised (apply-generic op a1 a2-raised))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.86
;;
;; Suppose we want to handle complex numbers whose real parts, imaginary parts,
;; magnitudes, and angles can be either ordinary numbers, rational numbers, or
;; other numbers we might wish to add to the system. Describe and implement the
;; changes to the system needed to accommodate this. You will have to define
;; operations such as sine and cosine that are generic over ordinary numbers
;; and rational numbers.

(define (square-g x) (mul x x))

(define (install-scheme-number-ext-package)
  (define (tag f) (lambda (x) (attach-tag 'scheme-number (f x))))
  (put 'atan '(scheme-number scheme-number) (tag atan))
  (put 'sin '(scheme-number) (tag sin))
  (put 'cos '(scheme-number) (tag cos))
  (put 'sqrt '(scheme-number) (tag sqrt))
  'done)

(define (install-rational-ext-package)
  (define (real x) (attach-tag 'scheme-number (/ (numer x) (denom x))))
  (put 'atan '(rational rational) (lambda (a b) (atan-g (real a) (real b))))
  (put 'sin '(rational) (lambda (a) (sin-g (real a))))
  (put 'cos '(rational) (lambda (a) (cos-g (real a))))
  (put 'sqrt '(rational) (lambda (a) (sqrt-g (real a))))
  'done)

(define (atan-g a b) (apply-generic 'atan a b))
(define (sin-g a) (apply-generic 'sin a))
(define (cos-g a) (apply-generic 'cos a))
(define (sqrt-g a) (apply-generic 'sqrt a))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (add (square-g (real-part z))
               (square-g (imag-part z)))))
  (define (angle z)
    (atan-g (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cos-g a)) (mul r (sin-g a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (mul (magnitude z) (cos-g (angle z))))
  (define (imag-part z) (mul (magnitude z) (sin-g (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt-g (add (square-g x) (square-g y)))
          (atan-g y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
;; ================================================================================



;; ================================================================================
;; Exercise 2.87
;;
;; Install =zero? for polynomials in the generic arithmetic package. This will
;; allow adjoin-term to work for polynomials with coefficients that are
;; themselves polynomials.

;; inside (install-polynomial-package)
(put '=zero? '(polynomial) (lambda (p) (empty-termlist? (term-list p))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.88
;;
;; Extend the polynomial system to include subtraction of polynomials. (Hint:
;; You may find it helpful to define a generic negation operation.)

(define (negate x) (apply-generic 'negate x))

;; inside (install-scheme-number-package)
(put 'negate '(scheme-number) -)

;; inside (install-polynomial-package)
(define (sub-terms L1 L2)
  (add-terms L1 (map negate-term L2)))

(define (negate-term t)
  (make-term (order t) (negate (coeff t))))

(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (sub-terms (term-list p1) (term-list p2)))
    (error "Polys not in same var: SUB-POLY" (list p1 p2))))

(put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))
(put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.89
;;
;; Define procedures that implement the term-list representation described
;; above as appropriate for dense polynomials.

;; inside (install-polynomial-package)
(define (highest-order term-list) (- (length term-list) 1))

(define (adjoin-term term term-list)
  (define (replicate n x tail)
    (if (= 0 n)
      tail
      (replicate (- n 1) x (cons x tail))))

  (define (replace head tail count)
    (if (= count 0)
      (cons head (cdr tail))
      (cons (car tail) (replace head (cdr tail) (- count 1)))))

  (if (=zero? (coeff term))
    term-list
    (let ((highest (highest-order term-list))
          (current (order term)))
        (if (> current highest)
          (cons (coeff term) (replicate (- current highest 1) 0 term-list))
          (replace (coeff term) term-list (- highest current ))))))

(define (first-term term-list) (make-term (highest-order term-list) (car term-list)))
;; ================================================================================



;; ================================================================================
;; Exercise 2.90
;;
;; Suppose we want to have a polynomial system that is efficient for both
;; sparse and dense polynomials. One way to do this is to allow both kinds of
;; term-list representations in our system. The situation is analogous to the
;; complex-number example of Section 2.4, where we allowed both rectangular and
;; polar representations. To do this we must distinguish different types of
;; term lists and make the operations on term lists generic. Redesign the
;; polynomial system to implement this generalization. This is a major effort,
;; not a local change.

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (install-sparse-terms-package)
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((empty-termlist? term-list) (list term))
          ((> (order term) (order (first-term term-list)))
           (cons term term-list))
          ((< (order term) (order (first-term term-list)))
           (cons (first-term term-list) (adjoin-term term (rest-terms term-list))))
          (else (error "Term order conflict"))))

  (define (make-terms terms)
    (define (iter result terms)
      (if (null? terms)
        result
        (iter (adjoin-term (car terms) result) (cdr terms))))
    (iter (the-empty-termlist) terms))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1) (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1) (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (negate-term t) (make-term (order t) (negate (coeff t))))

  (define (negate-terms L) (map negate-term L))

  (define (tag x) (attach-tag 'sparse-terms x))
  (put 'add '(sparse-terms sparse-terms) (lambda (a b) (tag (add-terms a b))))
  (put 'mul '(sparse-terms sparse-terms) (lambda (a b) (tag (mul-terms a b))))
  (put '=zero? '(sparse-terms) empty-termlist?)
  (put 'negate '(sparse-terms) (lambda (L) (tag (negate-terms L))))
  (put 'make 'sparse-terms (lambda (terms) (tag (make-terms terms))))
  'done)

(define (install-dense-terms-package)
  (define (highest-order term-list) (- (length term-list) 1))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (make-term (highest-order term-list) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (adjoin-term term term-list)
    (define (replicate n x tail)
      (if (= 0 n)
        tail
        (replicate (- n 1) x (cons x tail))))
    (define (replace head tail count)
      (if (= count 0)
        (cons head (cdr tail))
        (cons (car tail) (replace head (cdr tail) (- count 1)))))
    (if (=zero? (coeff term))
      term-list
      (let ((highest (highest-order term-list))
            (current (order term)))
        (if (> current highest)
          (cons (coeff term) (replicate (- current highest 1) 0 term-list))
          (replace (coeff term) term-list (- highest current ))))))

  (define (make-terms terms)
    (define (iter result terms)
      (if (null? terms)
        result
        (iter (adjoin-term (car terms) result) (cdr terms))))
    (iter (the-empty-termlist) terms))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1) (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1) (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (negate-term t) (make-term (order t) (negate (coeff t))))

  (define (negate-terms L) (map negate-term L))

  (define (tag x) (attach-tag 'dense-terms x))
  (put 'add '(dense-terms dense-terms) (lambda (a b) (tag (add-terms a b))))
  (put 'mul '(dense-terms dense-terms) (lambda (a b) (tag (mul-terms a b))))
  (put '=zero? '(dense-terms) empty-termlist?)
  (put 'negate '(dense-terms) (lambda (L) (tag (negate-terms L))))
  (put 'make 'dense-terms (lambda (terms) (tag (make-terms terms))))
  'done)

(define (make-sparse-terms terms) ((get 'make 'sparse-terms) terms))
(define (make-dense-terms terms) ((get 'make 'dense-terms) terms))

(define (install-terms-coercion-package)
  (define (the-empty-termlist) '())
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (sparse-to-dense terms) (make-dense-terms terms))

  (define (dense-to-sparse terms)
    (define (highest-order term-list) (- (length term-list) 1))
    (define (first-term term-list) (make-term (highest-order term-list) (car term-list)))

    (cond ((empty-termlist? terms) '())
          ((=zero? (coeff (first-term terms))) (dense-to-sparse (rest-terms terms)))
          (else (cons (first-term terms)
                      (dense-to-sparse (rest-terms terms))))))

  (put-coercion 'sparse-terms 'dense-terms
                (lambda (st) (sparse-to-dense (contents st))))
  (put-coercion 'dense-terms 'sparse-terms
                (lambda (dt) (attach-tag 'sparse-terms (dense-to-sparse (contents dt)))))
  'done)


(define (install-polynomial-package)
  ;; internal procedures representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul (term-list p1) (term-list p2)))
      (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (negate-poly p)
    (make-poly (variable p) (negate (term-list p))))

  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))

  (put '=zero? '(polynomial) (lambda (p) (empty-termlist? (term-list p))))
  (put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  'done)

(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))
;; ================================================================================



;; ================================================================================
;; Exercise 2.91
;;
;; A univariate polynomial can be divided by another one to produce a
;; polynomial quotient and a polynomial remainder. For example,
;;
;; x^5 - 1
;; ------- = x^3 + x, remainder x - 1.
;; x^2 - 1
;;
;; Division can be performed via long division. That is, divide the
;; highest-order term of the dividend by the highest-order term of the divisor.
;; The result is the first term of the quotient. Next, multiply the result by
;; the divisor, subtract that from the dividend, and produce the rest of the
;; answer by recursively dividing the difference by the divisor. Stop when the
;; order of the divisor exceeds the order of the dividend and declare the
;; dividend to be the remainder. Also, if the dividend ever becomes zero,
;; return zero as both quotient and remainder.
;;
;; We can design a div-poly procedure on the model of add-poly and mul-poly.
;; The procedure checks to see if the two polys have the same variable. If so,
;; div-poly strips off the variable and passes the problem to div-terms, which
;; performs the division operation on term lists. div-poly finally reattaches
;; the variable to the result supplied by div-terms. It is convenient to design
;; div-terms to compute both the quotient and the remainder of a division.
;; div-terms can take two term lists as arguments and return a list of the
;; quotient term list and the remainder term list.
;;
;; Complete the following definition of div-terms by filling in the missing
;; expressions. Use this to implement div-poly, which takes two polys as
;; arguments and returns a list of the quotient and remainder polys.

;; inside (install-polynomial-package)
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
    (list (the-empty-termlist) (the-empty-termlist))
    (let ((t1 (first-term L1))
          (t2 (first-term L2)))
      (if (> (order t2) (order t1))
        (list (the-empty-termlist) L1)
        (let ((new-c (div (coeff t1) (coeff t2)))
              (new-o (- (order t1) (order t2))))
          (let ((rest-of-result
                  (div-terms (sub-terms L1
                                        (mul-terms (list (make-term new-o new-c))
                                                   L2))
                             L2)))
            (list (adjoin-term (make-term new-o new-c)
                               (car rest-of-result))
                  (cadr rest-of-result))))))))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (let ((result (div-terms (term-list p1) (term-list p2)))
          (var (variable p1)))
      (list (make-poly var (car result)) (make-poly var (cadr result))))
    (error "Polys not in same var: DIV-POLY" (list p1 p2))))

(put 'div '(polynomial polynomial) (lambda (p1 p2) (map tag (div-poly p1 p2))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.92
;;
;; By imposing an ordering on variables, extend the polynomial package so that
;; addition and multiplication of polynomials works for polynomials in
;; different variables. (This is not easy!)

;; Since we weren't told how to compare symbols, the left hand side polynomial
;; is always chosen to be the dominant one

;; inside (install-polynomial-package)
(define (add-poly p1 p2)
  (let ((var1 (variable p1))
        (var2 (variable p2)))
    (cond ((same-variable? var1 var2)
           (make-poly var1 (add-terms (term-list p1) (term-list p2))))
          ((= 0 (highest-order p1))
           (add-poly (make-poly var2 (term-list p1)) p2))
          ((= 0 (highest-order p2))
           (add-poly p1 (make-poly var1 (term-list p2))))
          (else
            (add-poly p1 (make-poly var1 (list (make-term 0 (tag p2)))))))))

(define (mul-poly p1 p2)
  (let ((var1 (variable p1))
        (var2 (variable p2)))
    (cond ((same-variable? var1 var2)
           (make-poly var1 (mul-terms (term-list p1) (term-list p2))))
          ((= 0 (highest-order p1))
           (mul-poly (make-poly var2 (term-list p1)) p2))
          ((= 0 (highest-order p2))
           (mul-poly p1 (make-poly var1 (term-list p2))))
          (else
            (mul-poly p1 (make-poly var1 (list (make-term 0 (tag p2)))))))))

(put-coercion 'scheme-number
              'polynomial
              (lambda (x) (tag (make-poly 'x (list (make-term 0 x))))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.93
;;
;; Modify the rational-arithmetic package to use generic operations, but change
;; make-rat so that it does not attempt to reduce fractions to lowest terms.
;; Test your system by calling make-rational on two polynomials to produce a
;; rational function:

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))

;; Now add rf to itself, using add. You will observe that this addition
;; procedure does not reduce fractions to lowest terms.

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))

  (define (make-rat n d) (list n d))

  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d) ((get 'make 'rational) n d))
;; ================================================================================



;; ================================================================================
;; Exercise 2.94
;;
;; Using div-terms, implement the procedure remainder-terms and use this to
;; define gcd-terms as above. Now write a procedure gcd-poly that computes the
;; polynomial GCD of two polys. (The procedure should signal an error if the
;; two polys are not in the same variable.) Install in the system a generic
;; operation greatest-common-divisor that reduces to gcd-poly for polynomials
;; and to ordinary gcd for ordinary numbers. As a test, try

(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(greatest-common-divisor p1 p2)

;; and check your result by hand.


(define (greatest-common-divisor a b) (apply-generic 'gcd a b))

;; inside (install-scheme-number-package)
(put 'gcd '(scheme-number scheme-number) gcd)

;; inside (install-polynomial-package)
(define (remainder-terms a b) (cadr (div-terms a b)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
    a
    (gcd-terms b (remainder-terms a b))))

(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (gcd-terms (term-list p1) (term-list p2)))
    (error "Polys not in same var: GCD-POLY" (list p1 p2))))

(put 'gcd '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.95
;;
;; Define P1, P2, and P3 to be the polynomials
;;
;; P1: x^2 - 2x + 1,
;; P2: 11x^2 + 7,
;; P3: 13x + 5.
;;
;; Now define Q1 to be the product of P1 and P2 and Q2 to be the product of P1
;; and P3, and use greatest-common-divisor (Exercise 2.94) to compute the GCD
;; of Q1 and Q2. Note that the answer is not the same as P1. This example
;; introduces noninteger operations into the computation, causing difficulties
;; with the GCD algorithm. To understand what is happening, try tracing
;; gcd-terms while computing the GCD or try performing the division by hand.

(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)
;; (polynomial x (2 1458/169) (1 -2916/169) (0 1458/169))
;; ================================================================================



;; ================================================================================
;; Exercise 2.96
;;
;; a. Implement the procedure pseudoremainder-terms, which is just like
;; remainder-terms except that it multiplies the dividend by the integerizing
;; factor described above before calling div-terms. Modify gcd-terms to use
;; pseudoremainder-terms, and verify that greatest-common-divisor now produces
;; an answer with integer coefficients on the example in Exercise 2.95.
;;
;; b. The GCD now has integer coefficients, but they are larger than those of
;; P1. Modify gcd-terms so that it removes common factors from the
;; coefficients of the answer by dividing all the coefficients by their
;; (integer) greatest common divisor.

(define (scale-terms terms factor)
  (map (lambda (t) (make-term (order t) (* factor (coeff t))))
       terms))

(define (pseudoremainder-terms a b)
  (let ((c (coeff (first-term b)))
        (o1 (order (first-term a)))
        (o2 (order (first-term b))))
    (let ((int-fac (expt c (+ (- o1 o2) 1))))
      (let ((a-scaled (scale-terms a int-fac)))
        (cadr (div-terms a-scaled b))))))

(define (gcd-terms a b)
  (define (coeff-gcd terms)
    (apply gcd (map coeff terms)))
  (if (empty-termlist? b)
    a
    (let ((res (gcd-terms b (pseudoremainder-terms a b))))
      (scale-terms res (/ 1 (coeff-gcd res))))))
;; ================================================================================



;; ================================================================================
;; Exercise 2.97
;;
;; a. Implement this algorithm as a procedure reduce-terms that takes two term
;; lists n and d as arguments and returns a list nn, dd, which are n and d
;; reduced to lowest terms via the algorithm given above. Also write a
;; procedure reduce-poly, analogous to add-poly, that checks to see if the two
;; polys have the same variable. If so, reduce-poly strips off the variable and
;; passes the problem to reduce-terms, then reattaches the variable to the two
;; term lists supplied by reduce-terms.
;;
;; b. Define a procedure analogous to reduce-terms that
;; does what the original make-rat did for integers:

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

;; and define reduce as a generic operation that calls apply-generic to
;; dispatch to either reduce-poly (for polynomial arguments) or reduce-integers
;; (for scheme-number arguments). You can now easily make the
;; rational-arithmetic package reduce fractions to lowest terms by having
;; make-rat call reduce before combining the given numerator and denominator to
;; form a rational number. The system now handles rational expressions in
;; either integers or polynomials. To test your program, try the example at the
;; beginning of this extended exercise:

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))
(add rf1 rf2)

;; See if you get the correct answer, correctly reduced to lowest terms.

(define (reduce a b) (apply-generic 'reduce a b))

;; inside (install-polynomial-package)
(define (reduce-terms n d)
  (define (coeff-gcd a b)
    (apply gcd (map coeff (append a b))))
  (let ((g (gcd-terms n d)))
    (let ((c (coeff (first-term g)))
          (o1 (max (order (first-term n)) (order (first-term d))))
          (o2 (order (first-term g))))
      (let ((int-fac (expt c (+ (- o1 o2) 1))))
        (let ((n-scaled (car (div-terms (scale-terms n int-fac) g)))
              (d-scaled (car (div-terms (scale-terms d int-fac) g))))
          (let ((gcd-factor (coeff-gcd n-scaled d-scaled)))
            (let ((nn (scale-terms n-scaled (/ 1 gcd-factor)))
                  (dd (scale-terms d-scaled (/ 1 gcd-factor))))
              (list nn dd))))))))

(define (reduce-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (let ((result (reduce-terms (term-list p1) (term-list p2)))
          (var (variable p1)))
      (list (make-poly var (car result)) (make-poly var (cadr result))))
    (error "Polys not in same var: REDUCE-POLY" (list p1 p2))))

(put 'reduce '(polynomial polynomial) (lambda (n d) (map tag (reduce-poly n d))))

;; inside (install-scheme-number-package)
(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(put 'reduce '(scheme-number scheme-number) (lambda (n d) (reduce-integers n d)))

;; inside (install-rational-package)
(define (make-rat n d) (reduce n d))
;; ================================================================================
