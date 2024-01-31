#lang pl

#|
Question 1
__________
Function consumes list of numbers and a number n and splits the list in lists
with size of n. Function returns list of lists
Recursion:
Stop step is empty list or list that smaller/equal n. Or error (negative n)
Recursion step is to combine list (first n elements from given list using take func)
with next recursion (that will get what left after n elements using drop func) using cons

Problems:
1) There is no drop and take functions in pl.
2) Function in question is limited to Number values... why?
Solution:
1) Created take and drop functions bymyself using given first and rest functions
2) Changed the type of every list to Any. It will work with the numbers and also may be
very helpful in the future
__________
|#

(: create-fixed-length-lists : (Listof Any) Number -> (Listof (Listof Any)))
(define (create-fixed-length-lists num-list n)
  (cond
    [(<= n 0) (error 'create-fixed-length-lists "n cannot be negative or zero")] ;; negaive n error
    [(null? num-list) '()] ;; empty list
    [(<= (length num-list) n) (list num-list)] ;; return the num-list if it empty or smaller/equal n
    [else (cons (take num-list n)(create-fixed-length-lists (drop num-list n) n))]
    )
  )


;; take function takes list and number and returns list of first n elements in list or less
( : take : (Listof Any) Number -> (Listof Any))
(define (take lst n)
  (cond
    [(< n 0) (error 'take "n cannot be negative")] ;; negaive n error
    [(or (null? lst) (= n 0)) '()] ;; base case list is empty or n is 0 return empty list
    [else (cons (first lst) (take (rest lst) (- n 1)))]  ;; add new intem to returned list in every iteration
   )
  )

;; drop function takes list and number and returns list of objects after n objects
( : drop : (Listof Any) Number -> (Listof Any))
(define (drop lst n)
  (cond
    [(< n 0) (error 'take "n cannot be negative")] ;; negaive n error
    [(null? lst) '()] ;; 1 base case list is empty return empty list
    [(= n 0) lst] ;; 2 base case n is zero return whats left
    [else (drop (rest lst) (- n 1))] ;; remove one item in every iteration
    )
  )

;; first question tests
;; take test
(test (take '(1 2 3 4 5) 3) => '(1 2 3)) ;; regular 
(test (take '(1 2 3 4 5) 0) => '()) ;; zero n
(test (take '() 3) => '()) ;; empty list 
(test (take '(1 2 3) 5) => '(1 2 3)) ;; n bigger than list
(test (take '(1 2 3 4 5) 1) => '(1))
(test (take '(1 2 3 4 5) -1) =error> "negative") ;; negative n
;; drop test
(test (drop '(1 2 3 4 5) 3) => '(4 5)) ;; regular
(test (drop '(1 2 3 4 5) 0) => '(1 2 3 4 5)) ;; zero n
(test (drop '() 3) => '()) ;; empty list
(test (drop '(1 2 3) 5) => '()) ;; n bigger than list size
(test (drop '(1 2 3 4 5) 1) => '(2 3 4 5))
(test (drop '(1 2 3 4 5) -1) =error> "negative") ;; negative n
;; create-fixed-length-lists test
;; from question
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 3) => '((1 2 3) (4 5 6) (7 8 9)))
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 9) => '((1 2 3 4 5 6 7 8 9)))
;; my test
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 1) => '((1) (2) (3) (4) (5) (6) (7) (8) (9))) ;; n = 1
(test (create-fixed-length-lists '(1 2 3 4 5 6 7 8 9) 10) => '((1 2 3 4 5 6 7 8 9))) ;; n bigger than list size
(test (create-fixed-length-lists '() 3) => '()) ;; empty list
(test (create-fixed-length-lists '(1 2 3) 4) => '((1 2 3)))
(test (create-fixed-length-lists '(1 2 3) -4) =error> "negative") ;; negative n
(test (create-fixed-length-lists '(1 2 3) 0) =error> "zero") ;; zero n


#|
Question 2a
___________
Function recives list of any and return max depth of it.
Recursion:
Stop empty list return 0
Step take max between first item depth and other items depth

Problem:
1) Cant use recursion with first command because first may return Item and not List
Solution:
1) Help function that will recieve any item
|#

(: nested-list-depth : (Listof Any) -> Number)
(define (nested-list-depth lst)
  (if (null? lst) 0  ;; list is empty return 0
  (max
   (item-depth (first lst)) ;; help function because item not always list
   (nested-list-depth (rest lst)) ;; rest always returns list so can use recursion
   )
  )
  )
;; help function to check specified items depth
(: item-depth : Any -> Number)
(define (item-depth item)
(cond
  [(null? item) 0] ;; if null no depth
  [(list? item) (+ 1 (nested-list-depth item))] ;; if its list use main function
  [else 1] ;; its not list so depth is 1
 )
  )

;; Question 2a tests
;; from question
(test (nested-list-depth '(1 (2 3) ((4)) (5 (6)))) => 3) (test (nested-list-depth '(1 2 3)) => 1)
(test (nested-list-depth '()) => 0)
;; mine
(test (nested-list-depth '()) => 0)  ; empty list
(test (nested-list-depth '(1 2 3)) => 1)  ; flat list
(test (nested-list-depth '(1 (2 3) 4)) => 2)  ; one level of nesting
(test (nested-list-depth '(1 (2 (3)) 4)) => 3)  ; two levels of nesting
(test (nested-list-depth '((1) (2) (3 (4 (5))))) => 4)  ; mixed levels of nesting
(test (nested-list-depth '(1 2 (3 (4 (5 (6)))))) => 5)  ; increasing depth

#|
Question 2b
___________
Function consumes list of lists (of any) and for every list it returns min and max value
if it exist and nothing if not

Will use helper functions to extract number list and find max and min in it

Problems:
1) Hard to work with different types of data and existed functions.
In example can use (max firs(lst) a) if lst is not defined as Numbers
Solutiuon:
1) Use helper functions to work with specific values
|#

;; function will extract list of numbers from list (only first level)
(: extract-nums : (Listof Any) -> (Listof Number))
(define (extract-nums lst)
  (cond
    [(null? lst) '()] ;; return empty if list is empty
    [(number? (first lst)) (cons (first lst) (extract-nums (rest lst)))] ;; if first item is number add it to return list
    [else (extract-nums (rest lst))] ;; look for other items without first
    )
  )

;; test
(test (extract-nums '(1 "Bob" 2 "Alice" '4 '(5))) => '(1 2))
(test (extract-nums '()) => '())

;; find max/min in list of numbers
(: max-in-list : (Listof Number) -> Number)
(define (max-in-list lst)
  (if (= 1 (length lst))
      (first lst)  ;; return value from list if its the only
      (max (first lst) (max-in-list (rest lst))) ;; compare value to other values in list
  )
  )

(: min-in-list : (Listof Number) -> Number)
(define (min-in-list lst)
  (if (= 1 (length lst))
      (first lst) ;; return value from list if its the only
      (min (first lst) (min-in-list (rest lst)))  ;; compare value to other values in list
  )
  )

;;test
(test (max-in-list '(1 2 3)) => 3)
(test (max-in-list '(-1 -2 -3)) => -1)
(test (max-in-list '(-1 0 1)) => 1)
(test (min-in-list '(1 2 3)) => 1)
(test (min-in-list '(-1 -2 -3)) => -3)
(test (min-in-list '(-1 0 1)) => -1)

;; main function
(: min&max-lists : (Listof (Listof Any)) -> (Listof (Listof Number)))
(define (min&max-lists lst)
(cond
  [(null? lst) '()] ;; return empty if lst is empty
  [(null? (extract-nums (first lst)))(cons '() (min&max-lists (rest lst)))] ;; add empty list to result if first list dont have numbers
  [else
   (cons ;; add new list to result
   (list ;; create list with min and max value
    (min-in-list (extract-nums (first lst)))
    (max-in-list (extract-nums (first lst))))
   (min&max-lists (rest lst))) ;; continue to other lists
   ]
  )
  )

;; main tests
;; from question
(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ()))
        => '((1 5) (1 7) ()))
;; mine
(test (min&max-lists '()) => '()) ;; null
(test (min&max-lists '((1 2 3 4 5))) => '((1 5))) ;; only numbers
(test (min&max-lists '((1 "A" (2 3)) (4 (5 6) "B" 7))) => '((1 1) (4 7))) ;; mixed
(test (min&max-lists '(() (1 2 3) () (() () ()))) => '(() (1 3) () ())) ;; nested lists
(test (min&max-lists '((A B C) (D E F))) => '(() ())) ;; nested no numbers

#|
Question 3
__________
Queue data structure with enqueue dequeue and search.
|#


(define-type TaggedQueue
  [EmptyTQ] ; empty value
  [Enqueue Symbol Any TaggedQueue] ; id value nextNode
  )


(: search-queue : Symbol TaggedQueue -> Any)
(define (search-queue sym queue)
  (cases queue
    [(EmptyTQ) #f] ;; if queue empty or nothing found return false
    [(Enqueue s v n) (if (eq? s sym) v (search-queue sym n))] ;; if found return value else keep seeking
    )
  )

(: dequeue-queue : TaggedQueue -> Any)
(define (dequeue-queue queue)
  (cases queue
    [(EmptyTQ) #f] ;; if nothing to remove return false
    [(Enqueue s v n) n] ;; return next node
  )
  )

;; tests
;; from questions
(test (EmptyTQ) => (EmptyTQ))
(test (Enqueue 'x 42 (EmptyTQ)) => (Enqueue 'x 42 (EmptyTQ)))
(test (search-queue 'x (Enqueue 'x 42 (EmptyTQ))) => 42)
(test (dequeue-queue (Enqueue 'x 42 (EmptyTQ))) => (EmptyTQ))
(test (dequeue-queue (EmptyTQ)) => #f)
;; mine
;; Search for Elements in Different Positions
(test (search-queue 'a (Enqueue 'c 30 (Enqueue 'b 20 (Enqueue 'a 10 (EmptyTQ))))) => 10) ; front
(test (search-queue 'b (Enqueue 'c 30 (Enqueue 'b 20 (Enqueue 'a 10 (EmptyTQ))))) => 20) ; middle
(test (search-queue 'c (Enqueue 'c 30 (Enqueue 'b 20 (Enqueue 'a 10 (EmptyTQ))))) => 30) ; back

;; Dequeueing from a Queue with Multiple Elements
(test (dequeue-queue (Enqueue 'c 30 (Enqueue 'b 20 (Enqueue 'a 10 (EmptyTQ))))) => (Enqueue 'b 20 (Enqueue 'a 10 (EmptyTQ))))

;; Enqueueing Elements with the Same Symbol
(test (Enqueue 'a 100 (Enqueue 'a 50 (EmptyTQ))) => (Enqueue 'a 100 (Enqueue 'a 50 (EmptyTQ))))


;; Searching in an Empty Queue
(test (search-queue 'x (EmptyTQ)) => #f)

;; Dequeueing from an Already Empty Queue
(test (dequeue-queue (EmptyTQ)) => #f)





#|
Question 4
__________
<Q4> :=
      | <var> = <String>
      | <var> = <concat>
<concat> :=       
      | <var> ++ <var>
      | <var> ++ <String>
      | <String> ++ <var>
      | <String> ++ <String>
<var> := <varchar>
      | <varchar><var>
<String> := "<chars>"
<char> := "a" | ... | "z"
         | "A" | ... | "Z"
         | "0" | ... | "9"
         | (other characters)
<varchar> := "a" | ... | "z"
           | "A" | ... | "Z"
<chars> := <char>
         | <chars><char>
      
|#

























