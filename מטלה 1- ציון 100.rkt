#lang pl

#|
sarah hananayev בייננח הרש
039197488
|#

;; declaration of the functions implemented for Q1 & Q2 in this file
(: append5 : Char Char Char Char Char -> String )
(: permute3 : Char Char Char -> (Listof String) )
(: count-3lists : (Listof (Listof Any)) -> Natural )
(: count-3lists-tail : (Listof (Listof Any)) -> Natural )
(: helper-3lists-tail : (Listof (Listof Any)) Natural -> Natural )
(: count-3listsRec : (Listof (Listof Any)) -> Natural )




#|(: append5 : Char Char Char Char Char -> String )
Q1.1 the function Change 5 chars to one string.
for the function body I used a function I found in this link
https://docs.racket-lang.org/reference/strings.html#%28def._%28%28quote._~23~25kernel%29._string%29%29
|#

(define (append5 a b c d e)
  (string a b c d e)
)


;; test for append5
(test (append5 #\p #\i #\a #\n #\o) => "piano")
(test (append5 #\w #\o #\m #\a #\n) => "woman")
(test (append5 #\s #\u #\g #\a #\r) => "sugar")
(test (append5 #\d #\r #\e #\a #\m) => "dream")







#|(: permute3 : Char Char Char -> (Listof String) )
Q1.2 the function Change 3 chars and return a list of 6 strings.
based on the idea of Q1.1 Im using the same fnction I found in docs.racket-lang
|#

(define (permute3 a b c )
 (list
  (string a b c )
  (string a c b)
  (string b a c)
  (string b c a)
  (string c a b)
  (string c b a)))
 


;; test for permute3
(test (permute3 #\c #\a #\t) =>'("cat" "cta" "act" "atc" "tca" "tac"))
(test (permute3 #\a #\b #\c) =>'("abc" "acb" "bac" "bca" "cab" "cba"))
(test (permute3 #\d #\o #\g) =>'("dog" "dgo" "odg" "ogd" "gdo" "god"))
(test (permute3 #\z #\i #\p) =>'("zip" "zpi" "izp" "ipz" "pzi" "piz"))








#|(: count-3lists : (Listof (Listof Any)) -> Natural )
Q2.a Recursion- This function goes through a list that contains lists
and returns the amount of lists that contain 3 elements
My thinking was to check if the first list is 3 length
if true return 1 and run the function again on the rest of the list
and false return 0 and run the function again on the rest of the list
|#

(define (count-3lists lst)
  (if (null? lst)
   0
  (if (= (length(first lst)) 3)
      (+ 1 (count-3lists (rest lst)))
      (+ 0 (count-3lists (rest lst))))))


;; test for count-3lists
(test (count-3lists '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3lists '((1 3 4) (() (1 2 3) ()) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test (count-3lists '((() (1) ))) => 0)
(test (count-3lists '(( ( 1 1 1) ( 2 2 #\c ) ( "I" "eat" "apple" )) ( () () () ) ( 1 2 ))) => 2)







#|(: helper-3lists-tail : (Listof (Listof Any)) Natural -> Natural )
(: count-3lists-tail : (Listof (Listof Any)) -> Natural )
Q2.b this function is similar to Q2.a only here we use tail recursion.
the solution has the same idea but need to add a help function called "helper-3lists-tail".
the help function will remember the count up to the level we are in the recursion
as Tom explained in class
|#
;; the help function
(define (helper-3lists-tail lst counter)
  (if (null? lst)
   counter
  (if (= (length(first lst)) 3)
      (helper-3lists-tail (rest lst) (+ 1 counter))
      (helper-3lists-tail (rest lst) (+ 0 counter)))))


;; the main function  
(define (count-3lists-tail lst)
  (helper-3lists-tail lst 0))




;;test for count-3lists-tail
(test (count-3lists-tail '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3lists-tail '((1 3 4) (() (1 2 3) ()) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test (count-3lists-tail '((() (1) ))) => 0)
(test (count-3lists-tail '(( ( 1 1 1) ( 2 2 #\c ) ( "I" "eat" "apple" )) ( () () () ) ( 1 2 ))) => 2)



#|(: count-3listsRec : (Listof (Listof Any)) -> Natural )
Q2.c simiar to Q2a,b but the inner list of langth 3 needs to be considerd in the count.
for that Im using a help function copied form stackoverflow .
the function Checks whether its list of lists
https://stackoverflow.com/questions/53363087/how-can-i-check-if-a-list-is-a-list-of-lists
|#
;; the help function (dose not have declaration)
(define (list-of-lists? v)
  (and (list? v) (andmap list? v)))

;; the main function
(define (count-3listsRec lst)
  (if (null? lst)
   0
  ;; if list of list and in length 3 add 1 to the result and go into daeper level (witout this condition the function failes tests)
  (if (and (list-of-lists? (first lst)) (= (length(first lst)) 3)) 
      (+ 1 (count-3listsRec (first lst)) (count-3listsRec (rest lst)))
       ;; If list of list but not in length 3 no need to add the count and go into daeper level 
      (if (list-of-lists? (first lst)) 
          (+ (count-3listsRec (first lst)) (count-3listsRec (rest lst)))
          ;; must be a list, check the length of it 
          (if (= (length(first lst)) 3) 
              (+ 1 (count-3listsRec (rest lst)))
              (+ 0 (count-3lists (rest lst))))))))



;;test for count-3listsRec 
(test (count-3listsRec '((1 3 4) (() (1 2 3)) ("tt""Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test (count-3listsRec '((1 3 4) (1 3 4) (1 2 7) )) => 3) ;I took the test from the clas s forum 
(test (count-3listsRec '((() (1 2 3) (1 2 3) (1 2 3) ()))) => 3)
(test (count-3listsRec '((() (1) ))) => 0)
(test (count-3listsRec '(( ( 1 1 1) ( 2 2 #\c ) ( "I" "eat" "apple" )) ( () () () ) ( 1 2 ))) => 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; declaration of the functions implemented for Q3 in this file
(: search-stack : Symbol KeyStack -> (U String #false) )
(: pop-stack : KeyStack -> (U KeyStack #false) )

#|
Q3.a,b
From what I understand the solution to the first two questions
should be within the function that defines the new type KeyStack
empty constructor & constructor.
|#

(define-type KeyStack
[ EmptyKS ]
[ Push Symbol String KeyStack ])


;; test for EmptyKS & Push
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>(Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))=> (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))

#|(: search-stack : Symbol KeyStack -> (U String #false))
Q3.c
According to the structure of the test I used recursion.
after every attempt to build this function with if, and, cond or match failed,
I use a switch case on the constructor of the stack
help from pg 26 in lecture 1 pdf
if the symbol that is being pushed similsr to the symbol we look for
return the string, if we reach empty return false
|#
 

(define (search-stack sy ks)
  (cases ks
    [(EmptyKS) #false]
    [(Push symbol string keystack) (if (eq? symbol sy) 
                                       string
                                       (search-stack sy keystack))]))


;; test for search-stack
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a
"A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a
"A" (EmptyKS))))) => #f)


#|
Q3.d
After drawing conclusions from func Q3.c in here I did not do recursion,
I only used push on the first and returnd the rest as requested,
here I also tried to use match and even send email to tom but only switch case works 
|#


(define (pop-stack ks)
  (cases ks
    [(EmptyKS) #false]
    [(Push symbol string keystack) keystack ]))

(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A"(EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Q4
(: is-odd? : Natural -> Boolean)
#| this function recive a non negative number and return
true if its an odd number
false if its not odd number
The function works recursively using another function called "is even?".
Stop condition is whether X is equal to zero. If so return false
If not activate the function "is even?" on X-1,
if "is even?" return true then "is-odd?" return true and that matches the correct answer
|#
(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))

(: is-even? : Natural -> Boolean)
#|
this function recive a non negative number and return
true if its an even number
false if its not even number
The function works recursively using another function called "is odd?"
Stop condition is whether X is equal to zero. If so return true
If not activate the function "is-odd?" on X-1,
if "is odd?" return false then "is-even?" return false and that matches the correct answer
|#
(define (is-even? x)
 (if (zero? x)
 true
 (is-odd? (- x 1))))


;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))




(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
#|
Type A can be anything, it makes the function generic
this function recives 2 elements
1.a function that recive type A and return boolean
2.a function that list of type A and return boolean 
"every?" goes through the list and checks that all the elements in it are of type A
if so return true else false
|#
(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst)))))




;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
#|
this function recive a list of type Natural numbers and return boolean
it uses the func "every?" with "is even?" and the list of type Natural
to checks if all the numbers in the list are even-if so return true
else return false
|#

(define (all-even? lst)
 (every? is-even? lst))
;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))



(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
Boolean))
#|
this function recive 4 elements
1. a function that recive type A and return boolean
2. a function that recive type B and return boolean
3. a list of A
4. a list of B
and returns boolean
recursiv func that does what the function "every?" does but simultaneously on 2 lists
goes through the list A and checks that all the elements in it are of type A
and goes through the list B and checks that all the elements in it are of type B
|#
(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists assumed to be of same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))







