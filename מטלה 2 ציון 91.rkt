#lang pl 02

#|בייננח הרש
039197488
ex 2


 q1 i
It took me a whole day to write the solution to this question
each time I checked that the rules were correct
I encountered another problem and had to change
Im not sure my answer is correct,
I tried not to do ambiguous language and I have a lot of rules


<digit>::= 1|2|3|4|5|6|7|8|9   (1)
           |<digit> <digit>    (2)
           |(string-length <strings>)   (3)


<#\v>::=<#\v><#\v>              (4)
        |v stant for digit      (5) ; char of digit

"<D>"::=<D> sequens of digits   (6)

<srting>::= stand for string ; this is not a role, its only clarification

<strings>::=<strings> <strings>             (7)
          |null                           (8)
          |lambda ג " "                   (9)      ;empty string like said in the hw
          |(string <#\v>)                 (10)
          |"<D>"                          (11)
          |(string-append <strings>)      (12)
          |(numbr->string <digit>)        (13)
          |(string-insert <strings> <#\v> <digit>)      (14)


; I dont add here "<D>" role to prevent Ambiguity
<se>::=<#\v>                         (15) 
     |<digit>                        (16)
     |{string <#\v>}                 (17)          
     |{string-append <strings>}      (18)     
     |{string-insert <strings> <#\v> <digit> }           (19)   
     |{number->string <digit> }      (20)             



 q1 ii
;derivation process.
the three sentences are from examples of valid expressions.
It is not listed that it is forbidden so I hope its ok.

 
SE=( string-append "45" ( number->string ( string-length "034" ) ))

<SE>=>(12){string-append <strings>}
    =>(8){string-append strings  <strings> }
    =>(7){string-append strings <strings> <strings> }
    =>(11){string-append strings "<D>" <strings> }
    =>(6){string-append strings "45" <strings> }
    =>(13){string-append strings "45" (numbr->string <digit>) }
    =>(3){string-append strings "45" (numbr->string (string-length <strings>)) }
    =>(11){string-append strings "45" (numbr->string (string-length "<D>")) }
    =>(6){string-append strings "45" (numbr->string (string-length "034")) };;Done


SE=( string #\1 #\2 #\4 )

<SE>=>(19){string <#\v>}
    =>(4){string <#\v> <#\v>}
    =>(4){string <#\v> <#\v> <#\v>}
    =>(1){string <#\1> <#\V> <#\v>}
    =>(1){string <#\1> <#\2> <#\v>}
    =>(1){string <#\1> <#\2> <#\4>}  ;;Done



SE=( string-insert "57" #\4 66 )

<SE>=>(22){string-insert <strings> <#\v> <digit> }
    =>(11){string-insert "<D>" <#\v> <digit> }
    =>(6){string-insert "57" <#\v> <digit> }
    =>(5){string-insert "57" <#\4> <digit> }
    =>(2){string-insert "57" <#\4> <digit><digit> }
    =>(1){string-insert "57" <#\4> 6<digit> }
    =>(1){string-insert "57" <#\4> 66 } ;;Done

|#






#| q2 declerction of 2 functions as described in the homework
foldl was easy for me to understand.
Understanding map took me a bit of time
and then combain them to one line of code
|#

(: sum-of-squares : (Listof Number) -> Number)
(: square : Number -> Number)

#| (: sum-of-squares : (Listof Number) -> Number)
This function sends each member of the list to square function
and then accumulat the results
|#
(define (sum-of-squares lst)
  (foldl + 0 (map square lst)) 
)


#| (: square : Number -> Number)
 The function gets a number,
 multiplied it by itself and returns the result
 (i.e. makes ^2)
|#
(define (square num)
  (* num num)
)


;;  test for Q2
(test (sum-of-squares '(2 2 2)) => 12)
(test (sum-of-squares '(-2 -2 -2)) => 12)
(test (sum-of-squares '(8 9 10 11)) => 366)
(test (sum-of-squares '(42 9)) => 1845)




#| Q3 a
I copied and add to the  <-fill in-> parts 
I used a lot of scribbles in a notebook
in an attempt to understand what each variable should be
and how to put everything into this template.
also one closing brackets was missing it took me some time to fix it
|#
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
 (: poly : (Listof Number) Number Integer Number ->
Number)
 (define (poly argsL x power accum)
 (if (null? argsL)
     accum
     (poly (rest argsL) x (+ power 1) (+  accum (* (expt x power) (first argsL))))))
 (: polyX : Number -> Number)
 (define (polyX x)
   (poly coeffs x 0 0))
   polyX)

;;tests for createPolynomial

(define p-1-2-3 (createPolynomial '(-1 -2 -3)))
(test (p-1-2-3 6) =>(+ (* -1 (expt 6 0)) (* -2 (expt 6 1)) (* -3 (expt 6 2))))

(define p891011 (createPolynomial '(8 9 10 11)))
(test (p891011 3) => (+ (* 8 (expt 3 0)) (* 9 (expt 3 1)) (* 10 (expt 3 2)) (* 11 (expt 3 3))))

(define p222 (createPolynomial '(2 2 2)))
(test (p222 10) => (+ (* 2 (expt 10 0)) (* 2 (expt 10 1)) (* 2(expt 10 2))))

(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)









#| Q3 b i

language PLANG {{ 𝒑𝒐𝒍𝒚 𝑪𝟏 𝑪𝟐 … 𝑪𝒌} {𝑷𝟏 𝑷𝟐 … 𝑷𝓵}}
lets say you get only 𝑪𝟏 & 𝑷𝟏 in (<PLANG>) and there is a gremmer role for
creating more (<AEs>)

The grammar:
<PLANG>::= poly <AEs> (<AEs>)
<AEs>::=<AE>
        |<AE><AEs>  ;roles like what tom did with string in class 6 about ROL
 <AE>::=is the same to class
|#


#| Q3 b ii
I copied and add to the  <-fill in-> parts 
|#

(define-type PLANG
 [Poly (Listof AE) (Listof AE)]) ; poly  gets <AEs> (<AEs>), thats why  (Listof AE)
 (define-type AE
 [Num Number]
 [Add AE AE]
 [Sub AE AE]
 [Mul AE AE]
 [Div AE AE])



;; I dont do test to this  its form hw pdf Except for the one I made to make everything colorful
 (: parse-sexpr : Sexpr -> AE)
 ;; to convert s-expressions into AEs
 (define (parse-sexpr sexpr)
 (match sexpr
 [(number: n) (Num n)]
 [(list '+ lhs rhs) (Add (parse-sexpr lhs)(parse-sexpr rhs))]
 [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list '* lhs rhs) (Mul (parse-sexpr lhs)(parse-sexpr rhs))]
 [(list '/ lhs rhs) (Div (parse-sexpr lhs)(parse-sexpr rhs))]
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


 
(test (parse-sexpr null)=error> "parse-sexpr: bad syntax in ()")

 
 ;; parses a string containing a PLANG expressionto a PLANG AST
#|
According to the tests there are 3 situations.
Lack of coefficient, lack of point and normal condition
i did it using syntax  (match value
 [pattern result-expr]
 ...) from class pdf
I copied the errors for the hw pdf
 I dont know why but when i chang this conditions :
   [(list (cons 'poly llst) (list rlst ...)) (Poly (map parse-sexpr llst) (map parse-sexpr rlst))]
 location in the order of matchm the tests failes
very frustrating
llst- left list
rlst- rigth list
let change str to sexpr
and then map send evry element of the list to parse-sexpr
that chang it to AE and so we get a list of AE as nedeed for poly  
|#
(: parse : String -> PLANG)
 (define (parse str)
   (let ([code (string->sexpr str)])
 (match code 
   [(list (cons 'poly '()) (list rlst ...)) (error 'parse "at least one coefficient is required in ~s" code)]
   [(list (cons 'poly llst) '()) (error 'parse "at least one point is required in ~s" code)]
   [(list (cons 'poly llst) (list rlst ...)) (Poly (map parse-sexpr llst) (map parse-sexpr rlst))]
   [else (error 'parse "bad syntax in ~s" code)]))) ;;include empty list check so no need for empty list condition




;; test for parse
(test (parse "{ }")=error> "parse: bad syntax in ()")  ;empty
(test (parse "{{poly 8 9 10 11} {1 2 3}}")=> (Poly (list (Num 8) (Num 9) (Num 10) (Num 11)) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly } {8 9 10 11} }")=error> "parse: at least one coefficient is required in ((poly) (8 9 10 11))")
(test (parse "{{poly 8 9 10 11} {} {} }")=error> "parse: bad syntax in ((poly 8 9 10 11) () ())")
(test (parse "{{poly 8 9 10 11} {} }")=error> "parse: at least one point is required in ((poly 8 9 10 11) ())")



#| Q3 iii
I copied and add to the  <-fill in-> parts
we must use cases on p-expr
llst- left list
rlst- rigth list
|#
;; evaluates AE expressions to numbers

(: eval : AE -> Number) ;; if i dont add this line the "evel" word is error
 (define (eval expr)
 (cases expr
 [(Num n) n]
 [(Add l r) (+ (eval l) (eval r))]
 [(Sub l r) (- (eval l) (eval r))]
 [(Mul l r) (* (eval l) (eval r))]
 [(Div l r) (/ (eval l) (eval r))]))


#| this function take a PLANG called p-expr,
only one constructor for poly exist so we have only one case [Poly (Listof AE) (Listof AE)]
using map 3 times - on the left list for AE to number , on the right list from AE to number and
 eventually on createPolynomial the left list (coeficient) and the rigth list the list of values for x (points)
|#


 (: eval-poly : PLANG -> (Listof Number) )
 (define (eval-poly p-expr)
 (cases  p-expr
    [(Poly llst rlst) (map (createPolynomial (map eval llst)) (map eval rlst))]))


 (: run : String -> (Listof Number))
 ;; evaluate a FLANG program contained in a string
#| this function take a stirng,
transform it to PLANG using parse function
 and return the Listof Number that eve-poly create
|#
 (define (run str)
 (eval-poly (parse str)))

(test (run "{{poly 1/2 2 } {3 1/2 3}}") => '(13/2 3/2 13/2))
(test (run "{{poly 2 2 2} {2 10}}")=> '(14 222))
(test (run "{{poly 4} {1 4 9}}")=> '(4 4 4))
(test (run "{{poly {* 4 2} {- 4 1}} {{- 8 4}}}")=> '(20))
(test (run "{{poly 8 9 10 11} {1 2 3}}")=> '(38 154 422))
(test (run "{{poly 1 0} {-1 6 3}}")=> '(1 1 1))
(test (run "{{poly {+ 0 1} 1 {* 2 3}} {{/ 4 8} 3 {+ 15 9}}}")=> '(3 58 3481))







