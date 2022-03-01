#lang racket

(define nat-num-stream ;;; stream is a thunk
  (letrec
      ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (add-pointwise list1 list2)
    (if (and (list? list1) (list? list2)) 
        (letrec ( 
            ;;; limit number of calls to null? on list1
            [l1 (null? list1)] 
            ;;; limit number of calls to null? on list2
            [l2 (null? list2)])
        (cond 
            ;;; If list 1 and 2 both null then add null to list
            [(and l1 l2) null] 
            ;;; If list1 null, but list2 non empty number list. Pointwise add list2 head to list and recurse with empty list1 and tail of list2.
            [(and l1 (number? (car list2))) (cons (car list2) (add-pointwise list1 (cdr list2)))]
            ;;; If list2 null, but list1 non empty number list. Pointwise add list1 head to list and recurse with tail of list1 and empty list2.
            [(and l2 (number? (car list1))) (cons (car list1) (add-pointwise (cdr list1) list2))] 
            ;;; If list 1 and 2 both non empty with number lists. Pointwise add list and 2 heads to list together and recurse with tail 1 and 2.
            [(and (number? (car list2)) (number? (car list1))) (cons (+ (car list1) (car list2)) (add-pointwise (cdr list1) (cdr list2)))]
            ;;; condition to catch non number parameters passed into function
            [#t (error "illegal parameter")]))
        ;;; condition to catch non list parameters passed into function
        (error "illegal parameter")))

(define (add-pointwise-lists list_of_lists)
    (if (list? list_of_lists) 
        (cond 
            ;;; If list of lists empty then add null to list
            [(null? list_of_lists) null]
            ;;; If head of list a lists. Call add-pointwise with head of list of lists and recurse with tail of list_of_lists.
            [(list? (car list_of_lists)) (add-pointwise (car list_of_lists) (add-pointwise-lists (cdr list_of_lists)))]
            ;;; condition to catch non list parameters passed into function
            [#t (error "illegal parameter")])
        ;;; condition to catch non list parameters passed into function
        (error "illegal parameter")))

(define (add-pointwise-lists-2 list_of_lists)
    ;;; lambda function using add_pointwise 
    (define f (lambda (acc list) (add-pointwise acc list)))
    (if (list? list_of_lists)
        ;;; if list if lists is a list then fold f to piecewise add lists into accumulator
        (foldl f null list_of_lists)
        ;;; condition to catch non list parameters passed into function
        (error "illegal parameter")))

(define (stream-for-n-steps s n)
    (if (and (number? n) (> n 0))
        ;;; if n is a value greater then 0 build list from stream s
        (letrec ([s_pair (s)]) 
            ;;; add head of stream output to list and recurse with tail of stream and (n-1)
            (cons (car s_pair) (stream-for-n-steps (cdr s_pair) (- n 1))))
        ;;; null terminate list if n is 0 or less
        null))

;;; function form based on nat-num-stream
(define fibo-stream ;;; stream is a thunk
    (letrec 
        ([fib (lambda (x1 x2) 
            (cond 
                ;;; F0 = 0
                [(= x1 -1) (cons 0 (lambda () (fib 0 -1)))]
                ;;; F1 = 1
                [(= x2 -1) (cons 1 (lambda () (fib 0 1)))]
                ;;; Fn = F(n-1) + F(n-2)
                [#t (cons (+ x1 x2) (lambda () (fib x2 (+ x1 x2))))]))]) 
    ;;; start fib
    (lambda () (fib -1 -1))))

(define (filter-stream f s)
    ;;; save computation of having to get head and tail of stream more than once
    (letrec ([s_pair (s)][s_val (car s_pair)][next_s_thunk (cdr s_pair)])
    ;;; boolean result of filter on stream head
    (if (f s_val) 
        ;;; cons head of stream and recuvively filter stream
        (lambda () (cons s_val (filter-stream f next_s_thunk)))
        ;;; recursively filter stream 
        (filter-stream f next_s_thunk))))

;;; Use filter nat-num-stream on to convert the number to a string, then the string to a list of characters. You can compare two lists using equal?
;;; filtered results will be palindromes 
(define palyndromic-numbers
    (filter-stream [lambda (num) (letrec ([str (string->list (number->string num))]) (equal? str (reverse str)))] nat-num-stream))

;;; used macro function template and stream function template from nat-num-stream
(define-syntax create-stream  ; macro name
    (syntax-rules (using starting at with increment) ; other keywords
        [(create-stream name using csf starting at iO with increment delta) ; how to use macro
            (define name ; form of expansion 
                ; nat-num-stream function as base
                (letrec
                    ([f (lambda (x) (cons (csf x) (lambda () (f (+ x delta)))))])
                (lambda () (f iO))))])) 

(define (vector-assoc v vec)
    (letrec ;;; local function call to check for vector pair match
        ([fVec (lambda (i vLength) 
            (if (< i vLength) ;;; if i less then length of vec check vector pair else return #f
                (letrec ([vec_pair (vector-ref vec i)]) ;;; get vector pair at index i
                (if (and (pair? vec_pair) (equal? v (car vec_pair))) ;;; if vec_pair is a pair and head of pair equals v then return vec_pair
                    vec_pair ;;; return matched vector pair
                    (fVec (+ i 1) vLength))) ;;; else recurse vector function
                #f))]);;; return #f on no match found
    ;;; if vec is a vector then initiat fVec with index 0 and size of vector
    ;;; else return #f if improper arguments are given with function call
    (if (vector? vec) (fVec 0 (vector-length vec)) #f)))

(define (cached-assoc xs n)
    (letrec ([cache (make-vector n #f)] ;;; mutable vector of size n with #f
             [pos 0] ;;; mutatable variable to track round robin cache position 
             [ret-func (lambda (v) ;;; function returned which receives takes arg v when cached-assoc is called
                            (let ([cache_pair (vector-assoc v cache)]) ;;; local variable to limit executions of vector-assoc 
                                (if (equal? #f cache_pair) ;;; checks result of vector-assoc
                                    (let ([assoc_pair (assoc v xs)]) ;;; local variable to limit executions of assoc 
                                        (if (equal? #f assoc_pair) ;;; checks result of assoc
                                            assoc_pair ;;; return #f
                                            (begin  ;;; expressions to be called in order to mutate cache properlly
                                                (vector-set! cache pos assoc_pair) ;;; mutate index pos of cache
                                                (set! pos (remainder (+ pos 1) n)) ;;; increment and mod pos using mutation
                                                assoc_pair)));;; match found return assoc_pair
                                    (cache_pair))))] ;;; return matche cache_pair value
             [ret-false (lambda (v) #f)]) ;;; return false is improper arguments are given with cached-assoc call
    ;;; checks function ws called with proper argument types
    (if (and (list? xs) (number? n)) ret-func ret-false)))

(add-pointwise '(2 2 1) '(4 2))
(add-pointwise-lists '((1 1) (2 1 2 6) (3) (-5)))
(add-pointwise-lists-2 '((1 2) (2 4 3 2) (-9) (-2)))
(stream-for-n-steps nat-num-stream 13)
(stream-for-n-steps fibo-stream 6)
(stream-for-n-steps (filter-stream (lambda (i) (> i 2)) fibo-stream) 5)
(stream-for-n-steps palyndromic-numbers 13)

(create-stream double using (lambda (x) (+ x x)) 
         starting at  (begin (print "starting 2") 1)
         with increment (begin (print "inc 1") 1))
(double)
(stream-for-n-steps double 10)

(create-stream squares using (lambda (x) (* x x))
          starting at  (begin (print "starting 5") 5)
          with increment (begin (print "inc 2") 2))
(squares)
(stream-for-n-steps squares 5)

(stream-for-n-steps (filter-stream (lambda (i) (> i 2)) squares) 5)

(vector-assoc 5 (vector (cons "R" 1) (cons 3 1) (cons 4 1) (cons 5 1)))
(let
    [(cache (cached-assoc (list (cons "R" 2) (cons "G" 4)) 3) )]
            (cache "G"))

;;; (raise (add-pointwise '(w 2 1) '(4 2)))