(require xrepl)
;takes L={(a+b)* a b b (a+b)*}
(define fa
  (lambda (q ls)
    (let (
          [H (if (null? ls) '() (car ls))] 
          [T (if (null? ls) '()  (cdr ls))]
          )
      (cond
        [(= 0 q) (cond 
           [(equal? H 'b) (fa 0 T)]
           [(equal? H 'a) (or (fa 0 T) (fa 1 T))]
           [else #f]
           )]
        [(= 1 q) (if (equal? H 'b) (fa 2 T ) #f)]
        [(= 2 q) (if (equal? H 'b) (fa 3 T ) #f)]
        [(= 3 q) (cond
                   [(empty? T) #t]
                   [(equal? H 'a) (fa 3 T)]
                   [(equal? H 'b) (fa 3 T)]
                   [else #f]
                   )]
        [else #f]
      ))))
;(???  list_of_productions list)
;takes list of transition rules (N T M)
;where N is the current state 
;T is the terminal to match
;M is the state to transition to
;F is a boolean that indicates if the state is an accept state.
;


(define fsm
  (lambda (prodList ls F) 
    (define innerfsm
      (lambda (currentState lx)
        ;(print lx)
        (if (empty? lx) (not(boolean?(member currentState F)))
          (let ([prods (get_prods_state_N currentState prodList)]
                [head (car lx)]
                [tail (cdr lx)])
             (ormap (lambda (prd) 
                      (if (equal? head (car prd))
                        (innerfsm (cadr prd) tail)
                        #f
                        )
                      ) prods )
                    ))))
      (innerfsm 0 ls)
    ))
(define get_prods_state_N
  (lambda (x prodlst)
    (let ([filteredSet 
      (filter (lambda (prod) (= x (car prod))) prodlst)])
      ;(print filteredSet)
      (map cdr filteredSet)))) 


