(require xrepl)
;p is a prime
;q is a prime 
;e is the encryption exponent
;m is the message converted to an integer
;n = p*q
;phi(n) = (p-1)*(q-1)
;d = decryption exponent
;c is the encrypted message
;e*d conguent 1(mod phi(n))
;m must be less than n


(define remainders 
  (lambda (a b xs)
    (if (= b 0) 
      (cons 0 xs)
      (remainders b (modulo a b) (cons (modulo a b) xs)))))

;to encrypt
;c conguent m^e(mod n)
(define encrypt
  (lambda (p q e m)
    (let ([n (* q p)])
      (modulo (expt m e) n)
      )))

;d the decryption exponent
;e*d conguent 1(mod phi(n))
;e needs to be chosen such that gcd(e,phi(n))=1
(define decryptionExponent
  (lambda (p q e)
    (let ([phi (* (- p 1) (- q 1))])
      (inversemod phi e 1))))

;This bruteforce method of getting the inverse mod should be replaced
;with one using the algorithm for solving linear diophantine equations
(define bruteinversemod
 (lambda (phi e y) 
  (if (= 0 [modulo (+ 1 (* phi y)) e] )
    [/ (+ 1 (* phi y)) e]
    (bruteinversemod phi e (+ y 1)))))

;efficient inverse mod
;solve aX+bY=1
;a = e
;b = phi(n)
;X=d will be the inverse mod
(define inversemod
  (lambda (phi e one) ;one should litterally be 1
    (let* (
            [xandy (solve e phi one)]
            [d xandy]
            ) d
      )
      ))

;to decrypt
;recovered message conguent c^d(mod n)
(define decrypt
  (lambda (p q e c)
    (let ([d (decryptionExponent p q e)])
      (modulo (expt c d) (* p q)))))
(require xrepl)
;linear diophantine equation solver.

(define solvable? 
  (lambda (a b c)
    (= 0 (modulo c (gcd a b)))
    ))

(define solve
  (lambda (a b c)
    (if (not (solvable? a b c))
      (print "not solvable")
      (let* ([d (gcd a b)]
            [a1 (/ a d)]
            [b1 (/ b d)]
            [soln (solve1 (max a1 b1) (min a1 b1))]
            [x (car soln)]
            [y (cdr soln)])
        (cond
          ((= (+ (* a1 x) (* b1 y)) 1) (cons (* d x) (* d y)))
          (else (cons (* d c y) (* d c x)) )
          )
          ;this gets the x and y the wrong way around
          ;if x or y is 1
          ;can't remember if the above comment is still a problem or not
        )
      )
    )
  )

;Solves for aX + bY = 1.
;returns '(y . x) in some cases and '(x . y) in others.
;It seems '(x . y) only occurs if (= r 1) is true in the first itteration.
;The flipping of the x and y should be handles in an outer method.
(define solve1
  (lambda (a b)
    (let* ([r (modulo a b)]
          [n (/ (- a r) b)])
      (if (= r 1)
        ;then r = a - n*b
        ;so that y* is 1 and x* is -n
        ;i.e. r = ay* + bx* 
        (cons (* -1 n) 1)
        (let* ([s (solve1 b r)]
               [y* (cdr s)]
               [x* (car s)]
               [y x*]
               [x (+ (* -1 x* n) y*)])
          (cons x y)
          )
        )
      )))


