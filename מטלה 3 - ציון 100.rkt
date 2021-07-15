#lang pl
#|
There was no clear division of work because we sat on
the hw together all the time
 03919788 בייננח הרש
 317862068 ןלזוג לאפר
|#
#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL>}
        |  { union <SOL> <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
                                                                                
<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  ;; We did this based on rules above It's pretty simple and understandable to us
    [Set SET]
    [Smult Number SOL]
    [Inter SOL SOL]
    [Union SOL SOL]
    [IdS    Symbol]
    [WithS  Symbol SOL SOL]) 


;; ----------------------------------------------------

;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable.

#|
this function recive a number and a set (a list) and return true if
 the list contains the number else false.
 if the list is empty return false basic recursion 
|#
(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond [(null? l) false]
        [( = n (first l)) true]
        [else (ismember? n (rest l))]))

(test (ismember? 0 '(5)) => false)     ;our  
(test (ismember? 8 '(2 6 8)) => true)     ;our
(test (ismember? 1 '(3 4 5)) => #f)     ;;eran
(test (ismember? 1 '()) => #f)         ;;eran
(test (ismember? 1 '(1)) => #t)         ;;eran

#|
 this function recive a set (a list) and delete a dupliced apprence of a number in the list , then return the new list.
 from the test we know we need to keep the last apprence of a number
 using the function 'ismember?' will check if a number is in the rest of the list- if so we move on
 if not we keep the number and move on 
|#
(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  (cond [(or (null? l) (null? (rest l))) l]
        [(ismember? (first l) (rest l)) (remove-duplicates (rest l))] 
        [else (cons (first l) (remove-duplicates (rest l)))]))


(test (remove-duplicates '(3 4 5)) => '(3 4 5))             ;;our
(test (remove-duplicates '(1 2 3 1 2 3)) => '(1 2 3))      ;;our
(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))  ;; eran
(test (remove-duplicates '(1)) => '(1))                   ;; eran
(test (remove-duplicates '()) => '())                      ;; eran


#|
 this function receive a set (list) and make 2 things delete duplicate
 (using previous function 'remove-duplicates') and then
 using racket sort function sort it from small to big
 https://www.youtube.com/watch?v=vT916wNbwZ4 - video about function sort

|#
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (sort (remove-duplicates l) <))



(test (create-sorted-set '(3 4 5)) => '(3 4 5))              ;; eran
(test (create-sorted-set '( 3 2 3 5 6)) => '(2 3 5 6))        ;; eran
(test (create-sorted-set '()) => '())                      ;; eran
(test (create-sorted-set '(6 8 2 4)) => '(2 4 6 8))             ;;our
(test (create-sorted-set '(1 2 3 1 3 2)) => '(1 2 3))  ;;our

#|
 this function receive 2 sets and need to combine them into one set,
 then sort and delete duplicates we will use the previous function
 create-sorted-set and will sent to is the combination of the sets
  https://stackoverflow.com/questions/28440442/how-to-merge-2-lists-into-one-in-racket
|#

(: set-union : SET SET -> SET)
(define (set-union A B)
  ( create-sorted-set (append A B)))

(test (set-union '(3 4 5) '(3 4 5)) => '(3 4 5))           ;; eran
(test (set-union '(3 4 5) '()) => '(3 4 5))           ;; eran
(test (set-union '(3 4 5) '(1)) => '(1 3 4 5))           ;; eran
(test (set-union '(1 2 3 1 3 2) '(1)) => '(1 2 3 ))      ;;our
(test (set-union '(1 2 3 1 3 2) '(5 4 5)) => '(1 2 3 4 5))   ;;our
(test (set-union '(3 2 9) '(1)) => '(1 2 3 9))           ;; our



#|
 based on function name and tests we had an idea but than
 we saw the part we need to fill in
 it took us half an hour to figure out  
 mem-filter check if a number is in set  A 
 usinug filter with operator mem-filter on a sorted set B we check if evry number in B is also in A.
|#

(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (filter mem-filter (create-sorted-set B)))

(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5)) ;;eran
(test (set-intersection '(3 4 5) '(3)) => '(3))     ;;eran
(test (set-intersection '(3 4 5) '(1)) => '())     ;;eran
(test (set-intersection '(1 2 3 1 3 2) '(1)) => '(1 ))      ;;our
(test (set-intersection '(1 2 3 1 3 2) '(3 1 2)) => '(1 2 3 ))   ;;our
(test (set-intersection '(1 2 3 1 3 2) '()) => '())   ;;our
(test (set-intersection '(3 2 9) '(1 9 3)) => '(3 9))           ;; our

#|
 this function receive a number and set and multiply every element of the set in
 the number we used the idea of previous function but 'filter' Wasn't good enough

|#


(: set-smult : Number (Listof Number) -> SET)
(define (set-smult n l)
  (: mul : Number -> Number)
  (define (mul x)
    (* x n))
  (map mul (create-sorted-set l)))


(test (set-smult 3 '(3 4 5)) => '(9 12 15))  ;;eran
(test (set-smult 2 '()) => '())            ;;eran
;;(test (set-smult 0 '(3 4 5)) => '(0 0 0))   ;;eran
(test (set-smult 3 '(4 5 3)) => '(9 12 15))   ;;our
(test (set-smult 2 '(6 8 2 4)) => '(4 8 12 16))   ;;our
(test (set-smult 5 '(1 9 3)) => '(5 15 45 ))           ;; our

;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable.

;; this function receive Sexpr and based on the constructors defined earlier transform them into SOL
(: parse-sexprS : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexprS sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set ns) ]  ;; In this line we received help from a friend
    [(symbol: name) (IdS name)]                            ;; IdS is the only construcor with symbol
    [(cons 'with more)                                      ;;with is copied from class 9 pdf
     (match sexpr                                            
        [(list 'with (list (symbol: name) named) body)(WithS name (parse-sexprS named) (parse-sexprS body))]
        [else (error 'parse-sexprS "bad `with' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexprS rhs))]   ;;based on constructor above
    [(list 'intersect lhs rhs)  (Inter (parse-sexprS lhs) (parse-sexprS rhs))] ;;based on constructor above
    [(list 'union lhs rhs) (Union (parse-sexprS lhs) (parse-sexprS rhs))] ;;based on constructor above
    [else (error 'parse-sexprS "bad syntax in ~s" sexpr)]))

#|
 the purpose of this function is to transform a string into SOL
 it does it in two stages
 1. function recive a string, make it into sexpr and send it to
 another function 2. 'parse-sexprS' that recive a sexpr and return a SOL
|#
(: parseS : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parseS str)
  (parse-sexprS (string->sexpr str)))


(test (parseS "{1 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 3 4 1 4 4 2 3 4 1 2 3)))   ;;eran
(test (parseS "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(4 2 3))))    ;;eran
(test (parseS "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(4 2 3))))    ;;eran
(test (parseS "{with S {intersect {1 2 3} {4 2 3}}
                 {union S S}}")
      =error> "parse-sexprS: bad `with' syntax in")                                      ;;eran
(test (parseS "{intersect {1 2 3} {1 2 3} {4 2 3}}") =error> "parse-sexprS: bad syntax in")    ;;our
(test (parseS "{4 6 8 2 10}") => (Set '(4 6 8 2 10)))                                       ;;our
(test (parseS "{with {a 15 }{union 5 a}}")=error> "parse-sexprS: bad syntax in")     ;;our
(test (parseS "{intersect {6 9} {6 9}}") => (Inter (Set '(6 9)) (Set '(6 9))))    ;;our




;##################################################3;mising our & eran test

;;-----------------------------------------------------
;; Substation 
#|
------------------------------------------------------
 Formal specs for `subst':
   (`Set' is a <NumList>, E, E1, E2 are <SOL>s, `x' is some <id>,
   `y' is a *different* <id>)
      Set[v/x]              = Set
      {smult n E}[v/x]      = {smult n E[v/x]}
      {inter E1 E2}[v/x]    = {inter E1[v/x] E2[v/x]}
      {union E1 E2}[v/x]    = {union E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#
 #|
The rules given above are very clear,
 we made a comparison to them and to the class file and
saw who E1 E2 is and therefore we completed
|#
(: substS : SOL Symbol SOL -> SOL)
(define (substS expr from to)
  (cases expr
    [(Set n) expr]
    [(Smult n s) (Smult n (substS s from to))]
    [(Inter l r) (Inter (substS l from to) (substS r from to))]
    [(Union l r) (Union (substS l from to) (substS r from to))]
    [(IdS name) (if (eq? name from) to expr)]      ;;IdS copied from class pdf
    [(WithS bound-id named-expr bound-body)        ;;WithS copied from class pdf  
     (if (eq? bound-id from)
       expr ; <-- don't go in!
       (WithS bound-id
             named-expr
             (substS bound-body from to)))]))

;; run will test substS as well - no need for additional tests 

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

 

Evaluation rules:
    eval({ N1 N2 ... Nl })  = sort( create-set({ N1 N2 ... Nl }))
                               ;; where create-set removes all duplications from
                                  the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E}) = { K*N1 K*N2 ... K*Nl }
                               ;; where eval(E)={ N1 N2 ... Nl }
    eval({intersect E1 E2}) = sort( create-set(set-intersection (eval(E1) , eval(E2)))     
    eval({union E1 E2})     = sort( create-set(set-union (eval(E1) , eval(E2)))
    eval({with {x E1} E2})  = eval(E2[eval(E1)/x])
    eval(id)                = error!


|#


;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable.

;; the code below is based on tirgol presentation 8,9. its very clear what to do if you look there
(: eval : SOL -> SET)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr)
  (cases expr
    [(Set S) (create-sorted-set S)]  
    [(Smult n set) (set-smult n (eval set))]
    [(Inter l r) (set-intersection (eval l)(eval r))]
    [(Union l r) (set-union (eval l)(eval r))]
    [(WithS name named body) (eval (substS body name (Set (eval named))))] 
    [(IdS name) (error 'eval "free identifier: ~s" name)]))


(: run : String -> SET)
;; evaluate a SOL program contained in a string
(define (run str)
  (eval (parseS str)))


;; eran tests
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {with {x {4 5 7 6 9 8 8 8}}
                    {union x S}}}")
      => '(2 3 4 5 6 7 8 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
              {union {scalar-mult 3 B}
                 {4 5 7 9 8 8 8}}}")
      =error> "eval: free identifier:")

;;our test
(test (run "{with {y { y }} {x y 5 6} {scalar-mult 5 x}}") =error> "parse-sexprS: bad `with' syntax in")
(test (run "{8 8 8 8 8 8 8 8 8 8 8 8}") => '(8))
(test (run "{with {x {union {8 4 6 2} {2 8 6 4}}}
            {scalar-mult 12 x}}") => '(24 48 72 96))
(test (run "{with {x  {intersect {3 2 5 1} {6 2 8 4}}}
            {union {1 2 3 } x}}") => '(1 2 3))



;##################################################3;mising our test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
all the functions except the function 'freeInstanceList' and
 function 'unique-l' are copied from class pdf
|#

#|
<WAE> ::= <num>
 | { + <WAE> <WAE> }
 | { - <WAE> <WAE> }
 | { * <WAE> <WAE> }
 | { / <WAE> <WAE> }
 | { with { <id> <WAE> } <WAE> }
 | <id>

|#

(define-type WAE
  [Num Number]
  [Add WAE WAE]
  [Sub WAE WAE]
  [Mul WAE WAE]
  [Div WAE WAE]
  [Id Symbol]
  [With Symbol WAE WAE])



(: parse-sexpr : Sexpr -> WAE)
 ;; to convert s-expressions into WAEs
 (define (parse-sexpr sexpr)
  (match sexpr
   [(number: n) (Num n)]
   [(symbol: name) (Id name)]
   [(cons 'with more)
 ;; go in here for all sexpr that begin with a 'with
    (match sexpr
      [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
      [else (error 'parse-sexpr "bad ‘with' syntax in ~s" sexpr)])]
   [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
   [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
   [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
   [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
   [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


(: parse : String -> WAE)
(define (parse str)
  (parse-sexpr (string->sexpr str)))



 (: subst : WAE Symbol WAE -> WAE)
 ;; substitutes the second argument with the third argument in the
 ;; first argument, as per the rules of substitution; the resulting
 ;; expression contains no free instances of the second argument
 (define (subst expr from to) ; returns expr[to/from]
   (cases expr
     [(Num n) expr]
     [(Add l r) (Add (subst l from to) (subst r from to))]
     [(Sub l r) (Sub (subst l from to) (subst r from to))]
     [(Mul l r) (Mul (subst l from to) (subst r from to))]
     [(Div l r) (Div (subst l from to) (subst r from to))]
     [(Id name) (if (eq? name from) to expr)]
     [(With bound-id named-expr bound-body)
      (if (eq? bound-id from)
        expr ; <-- don't go in!
        (With bound-id
              named-expr
             (subst bound-body from to)))]))




#|
this is a help function to remove the duplicates from the end of the list
 and keep the first performance of the symbol https://groups.google.com/g/racket-users/c/BuUzcJtd3Ig?pli=1
 we dont do test becuse 'freeInstanceList' Would not pass tests if
this function did not work
|#

(: remove-dup : (Listof Symbol) -> (Listof Symbol))
(define (remove-dup l)
 (cond
  [(null? l) '()]
  [else (cons (first l) (remove* (list (first l)) (remove-dup (rest l))))]))


;;HINT: use the current structure of eval (and subst) in the WAE interpreter we have seen
;;in class to do so. Your resulting program should never evaluate the code (WAE) it takes
;;as input. In fact, the function eval itself should not appear in your code.;

#|
 Our technique was to copy the function subst and start changing it according
to the hint and then dig deeper into what the function needs to return
 a number is an empty list
 no need to do the 'Add' 'Sub' 'mul' 'div' calculation and basically all
the elements of the 2 lists are free Instance so we will return one list containing the 2 lists
 id return an error of free identifier in eval function so in this function we need to return name
 with is unclear to us yet , we used evel structure and tried and tried until it worked

|#
(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList expr)
  (cases expr
    [(Num n) '()]
    [(Add l r) (remove-dup (append (freeInstanceList l) (freeInstanceList r)))]
    [(Sub l r) (remove-dup (append (freeInstanceList l) (freeInstanceList r)))]
    [(Mul l r) (remove-dup (append (freeInstanceList l) (freeInstanceList r)))]
    [(Div l r) (remove-dup (append (freeInstanceList l) (freeInstanceList r)))]
    [(Id name) (list name)]
    [(With bound-id named-expr bound-body)
      (remove-dup ( append (freeInstanceList named-expr) (freeInstanceList (subst bound-body bound-id named-expr))))])) 



;;eran tests

(test (freeInstanceList (parse "w")) => '(w))
(test (freeInstanceList (parse "{with {xxx 2} {with {yyy 3} {+{- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (With 'x (Num 2) (Add (Id 'x) (Num 3)))) => '())
(test (freeInstanceList (parse "{+ z {+ x z}}")) => '(z x))

;;our tests
(test (freeInstanceList (Id 'w)) => '(w))
(test (freeInstanceList (parse "{/ z {+ x z}}")) => '(z x))
(test (freeInstanceList (parse "{* z {+ x z}}")) => '(z x))
(test (freeInstanceList (parse "{with {x {+ 9 w}} {+ x {with {x 9} {/ x 6}}}}")) => '(w ))
(test (freeInstanceList (parse "{{x 8} {/ x {with {x 8} 2}}}")) =error> "bad syntax in")
(test (freeInstanceList (parse "{with x 5 {with {z {- x 3}} {+ z y}}}")) =error> "bad ‘with' syntax in")

