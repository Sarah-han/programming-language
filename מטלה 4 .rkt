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
        |  { with {<id> <SOL>  <id> <SOL>} <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
     edddccccccccccccccc4d4c4ccrc    |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current 0/environment

        |  True
        |  False                            ;;if we have true we need false
        | { if <SOL> then <SOL> else <SOL> }
        | { equal? <SOL> <SOL> }

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  [Set  SET]
  [Smult Number SOL]
  [Inter SOL SOL]
  [Union SOL SOL]
  [Id    Symbol]
  ;;    [With  Symbol Symbol SOL SOL SOL] -- not to be used, syntactic sugar for ...
  [Fun   Symbol Symbol SOL]
  [CallS SOL SOL SOL]
  [CallD SOL SOL SOL]
  [Bool Boolean]
  [If SOL SOL SOL]
  [Equal SOL SOL])          ;; based on { equal? <SOL> <SOL> } 


;; ----------------------------------------------------

;; noting to do here

;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond [(null? l) #f]
        [(= n (first l)) #t]
        [else (ismember? n (rest l))]))

(test (not (ismember? 1 '(3 4 5))))
(test (not (ismember? 1 '( 3 2 3 5 6))))
(test (ismember? 1 '(3 4 5 1 3 4)))
(test (ismember? 1 '(1)))

(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  (cond [(or (null? l) (null? (rest l))) l]
        [(ismember? (first l) (rest l)) (remove-duplicates (rest l))]
        [else (cons (first l) (remove-duplicates (rest l)))]))
  
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (remove-duplicates (sort l <)))
  
(: set-union : SET SET -> SET)
(define (set-union A B)
  (create-sorted-set (append A B)))

(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  (filter mem-filter (create-sorted-set B)))

  


;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable. 
(: parse-sexpr : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexpr sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; sort and remove-duplicates
    ['True (Bool true)] 
    ['False (Bool false)]  ;; same as true
    [(symbol: name) (Id name)]
    [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name1) named1 (symbol: name2) named2) body) ;;we Complete the line based on the order of the arguments in the constructor
          (CallS (Fun name1 name2 (parse-sexpr body)) (parse-sexpr named1) (parse-sexpr named2))] ;;; There is no With constructor. Replace it with existing constructors...;; 
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name1) (symbol: name2)) body)
          (if (equal? name1 name2)
              (error 'parse-sexpr "`fun' has a duplicate param name in ~s" sexpr) ;; cannot use the same param name twice ;;we complete line based on given test
              (Fun name1 name2 (parse-sexpr body)))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))] ;; same as 'call-static
    [(list 'if cond 'then true-cond 'else false-cond) (If (parse-sexpr cond) (parse-sexpr true-cond) (parse-sexpr false-cond))]  ;; we did what looks logic- after running test will know if we did a good job
    [(list 'equal? arg1 arg2) (Equal (parse-sexpr arg1) (parse-sexpr arg2))]  ;; we did what looks logic- after running test will know if we did a good job
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

;;all test pass- the logic was ok!
;;It was our logic according to the other lines of code that were full


    

    


(: parse : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

  

;;; Tests for parse
 
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}} c {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'c
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Set '())))


(test (parse "{with {S {intersect {1 2 3} {4 2 3}} S1 {union {1 2 3} {4 2 3}}}
                          {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters
(test (parse "True") => (Bool true))
(test (parse "{if {equal? {1 2 3} {1 2}} then {1 2 3} else {1 2}}") =>
      (If (Equal (Set '(1 2 3)) (Set '(1 2))) (Set '(1 2 3)) (Set '(1 2))))

(test (parse "{with {S {intersect {1 2 3} {4 2 3}} c {}}
                 {call-dynamic {fun {x y} {union x S}}
                               {if {equal? S {scalar-mult 3 S}}
                                   then S
                                   else {4 5 7 6 9 8 8 8}}
                               {}}}")
=> (CallS (Fun 'S 'c
               (CallD (Fun 'x 'y (Union (Id 'x) (Id 'S)))
                      (If (Equal (Id 'S) (Smult 3 (Id 'S)))
                          (Id 'S)
                          (Set '(4 5 6 7 8 9)))
                      (Set '())))
          (Inter (Set '(1 2 3)) (Set '(2 3 4)))
          (Set '())))

;; our tests
(test (parse "False") => (Bool false))


(test (parse "{with S 3 t 5} ") =error> "parse-sexpr: bad `with' syntax in (with S 3 t 5)") ;; missing body

(test (parse "{intersect {1 2 3}} ")  =error> "parse-sexpr: bad syntax in (intersect (1 2 3))")
;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below
    eval({ N1 N2 ... Nl })      = sort( create-set({ N1 N2 ... Nl })) ;; where create-set removes all duplications from
                                                                         the sequence (list) and sort is a sorting procedure

    eval({scalar-mult K E})     = { K*N1 K*N2 ... K*Nl }              ;; where eval(E)={ N1 N2 ... Nl }
    eval({intersect E1 E2})     = sort( create-set(set-intersection (eval(E1) , eval(E2)))     
    eval({union E1 E2})         = sort( create-set(set-union (eval(E1) , eval(E2)))
    eval({fun {x1 x2} E},env)   = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env)
                                = eval(Ef,extend(x2,eval(E2,env)(extend (x1, eval(E1,env),f-env) )  ;;So far this has been the hardest line to complete in the task
                                                      if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
                                = error!              otherwise
    eval({call-dynamic E-op E1 E2},env)
                                = eval(Ef,extend(x2,eval(E2,env)(extend (x1, eval(E1,env),env) )
                                                      if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
                                = error!              otherwise         ;; The only change is in the definition of the environment of the function definition time.
                                                                                This is what will make the function dynamic instead of static

    eval(True,env)              = true
    eval(False,env)             = false
    eval({if E1 then E2 else E3},env)
                                = eval(E3, env)       if eval(E1,env) = false
                                = eval(E2, env)     otherwise      ;; if eval(E1,env) = true                                   

    eval({equal? E1 E2},env)    = true                if eval(E1,env) is equal in content to eval(E2,env) else false
                                =  error!      otherwise   ;; only When we wrote tests we saw a mistake in this part and we changed





|#

;; Types for environments, values, and a lookup function

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [SetV SET]
  [FunV Symbol Symbol SOL ENV]
  [BoolV Boolean]) ;; constarctor boolean. replace Replaces the boolean of lp 

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: SetV->set : VAL -> SET)
(define (SetV->set v)
  (cases v
    [(SetV S) S]
    [else (error 'SetV->set "expects a set, got: ~s" v)]))


;;in the hw its said we need to use SetV->set - and this taks VAL and return a set.
;; map is helpful for set and we can use idea from hw 3
;; we return SetV -is from type VAL
(: smult-set : Number VAL -> VAL)
(define (smult-set n s)
  (: mult-op : Number -> Number)
  (define (mult-op k)
    (* k n))
 (SetV (map mult-op (SetV->set s))))


;; VAL VAL -> VAL  this is clear from the function body.
;; the first part is a decleration on operator on SET so we got ispired from decleration on operator on NUMBER
(: set-op :(SET SET -> SET) VAL VAL -> VAL )   
;; gets a binary SET operator, and uses it within a SetV
;; wrapper
(define (set-op op val1 val2)
  (SetV (op (SetV->set val1) (SetV->set val2))))

;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL ENV -> VAL)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr env)
  (cases expr
    [(Set S) (SetV (create-sorted-set S))]
    [(Smult n set) (smult-set n (eval set env))]       
    [(Inter l r) (set-op set-intersection(eval l env) (eval r env))]    
    [(Union l r) (set-op set-union (eval l env) (eval r env))]
    [(Id name) (lookup name env)]
    [(Fun bound-id1 bound-id2 bound-body)
     (FunV bound-id1 bound-id2 bound-body env)]
    [(CallS fun-expr arg-expr1 arg-expr2)                               
     (let ([fval (eval fun-expr env)])                                     
       (cases fval                            
         [(FunV bound-id1 bound-id2 bound-body f-env)
                 (eval bound-body                              ;;based on eval roles
                  (Extend bound-id2 (eval arg-expr2 env) 
                     (Extend bound-id1 (eval arg-expr1 env)
                        f-env)))]
         [else (error 'eval "`call-static' expects a function, got: ~s"
                      fval)]))]
    [(CallD fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)                  ;;based on eval roles && class code
            (eval bound-body
                  (Extend bound-id2 (eval arg-expr2 env) 
                     (Extend bound-id1 (eval arg-expr1 env) 
                        env)))]                                            ; not f-env , eran asked this in calss and sara answered so she rememberd 
         [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                      fval)]))]
    [(Bool b) (BoolV b)] ;; not sure at all, maybe its need to be eval(b ,env)
    [(If cond true-cond false-cond)
     (let ([cval (eval cond env)])
       (cases cval
         [(BoolV b) (if (equal? b false) (eval false-cond env) (eval true-cond env)) ] 
         [else (error 'eval "if expected boolean value in ~s" cval)]))] 
    [(Equal l r) (if (equal? (eval l env) (eval r env))
                     (BoolV #t)
                    (BoolV #f))]))


#|

 [FunV Symbol Symbol SOL ENV] SOL--> [CallD SOL SOL SOL]   
The bottom line is the first we define and it defines the pair. Put a pair in an empty environment
The middle row expands the environment, to an environment that also knows the couple's first.
Top row extends the environment to an environment that also knows the other of the couple.
We knew to use a static because the test hints at it.
The function was hard to implement.
We took screenshots of the codes Eran did in class and tried to incorporate them as he said.

s1- is the extra (useless) parameter you mention in the hw

At the end of all the fun should register an environment because it is one of the arguments.
We had an argument whether to send an empty environment or use the one defined but in the end we decided to send an empty environment each time
|#



(: createGlobalEnv : -> ENV)
  (define (createGlobalEnv)
   (Extend 'second
           (FunV 'p 's1 (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
            (Extend 'first
                    (FunV 'p 's1 (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
                    (Extend 'cons
                            (FunV 'fir 'sec (Fun 'sel 's1 (CallS (Id 'sel) (Id 'fir) (Id 'sec))) (EmptyEnv)) 
                                    (EmptyEnv)))))


(: run : String -> (U SET VAL Boolean))
;; evaluate a SOL program contained in a string
(define (run str)
  (let ([result (eval (parse str) (createGlobalEnv))])
    (cases result
      [(SetV S) S]
      [(BoolV b) b]
      [else result])))


(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))


(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
               {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}
                    S1 {}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}
                     S1 {}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))


(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}
                   S1 {}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}
                     S1 {}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))


(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")
(test (run "True") => #t)
(test (run "{if {equal? {1 2 3} {1 2}} then {1 2 3} else {1 2}}") => '(1 2))
(test (run "{equal? {union {1 2 3} {4 2 3}} {1 2 3 4}}") => #t)
(test (run "{union {equal? {4} {4}} {4 2 3}}") =error> "SetV->set: expects a set, got: #(struct:BoolV #t)")


;;our test
(test (run "{with {S {intersect {1 2 3} {4 2 3}}
                   S1 {}}
                 {call-static {fun {x y} {union Z S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      =error> "lookup: no binding for Z")

(test (run "{call-dynamic {1} {2 2} {}}")
      =error> "eval: `call-dynamic' expects a function, got: #(struct:SetV (1))")

(test (run "{if {equal? {1 2 3} {1 2 3}} then {1 2 3} else {1 2}}") => '(1 2 3))
(test (run "{if {1 2 3} then {1 2 3} else {3 1}}") =error> "eval: if expected boolean value in #(struct:SetV (1 2 3))" )


(test (run "{with { f {with { a {44} b {44 }} {fun {x y} {intersect a b}}}  y {}}
                           {with {a { 4} b {4}}
                                 {call-static f  {} {} }}}") => '(44))


(test (run "{with { f {with { a { 44} b {44}} {fun {x y} {intersect a b}}}  y {}}
                          {with {a {4} b {4}}
                                {call-dynamic f  {} {} }}}") => '(4))


#|

Open questions:

1.
setv
funv
bool v

sol Suitable for functions, sets and boolean

at the end , after all the procrs we can evaluet only 3 types based on this define-type

(define-type VAL
  [SetV SET]
  [FunV Symbol Symbol SOL ENV]
  [BoolV Boolean])


2.
We had no hesitation in what to use because we saw in the test that static is used.
But it makes sense that we want at definition of 'with' a static definition. After all, we define the name of a variable and value
and we want this value to be preserved when it has a use for the body .


3.We decided to put callS throughout the function global environment,
We tried all the options and saw that the second and first must be static. if not we have error "lookup: no binding for sec"
 But in the case of cons it did not change anything.



4.We ran this test many times,
 each time with a different combination of static and dynamic positions but
 always the test  pass because the initial definition of the pair does not change
In the test the pair is defined only once and there is no attempt to change the values of the organs in the pair.






|#