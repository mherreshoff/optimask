#lang scheme
(begin
  ; Map and then concatenate:
  (define (concat-map f L) (apply append (map f L)))
  
  ; Take the cartesian product of two lists.  For example:
  ; (cartesian-product f '(a b) '(1 2)) = (list (f 'a 1) (f 'a 2) (f 'b 1) (f 'b 2))
  (define (cartesian-product f L1 L2) (concat-map (lambda (a1) (map (lambda (a2) (f a1 a2)) L2)) L1))
  
  ; Make a version of the function f which returns out when the input was in and defaults to f otherwise.
  (define (subst in out f) (lambda (x) (if (eq? x in) out (f x))))
  
  ; Return the list of all mappings from domain to range.
  (define (all-mappings domain range)
    (if (null? domain)
        (list (lambda (x) '?))
        (cartesian-product (lambda (output f) (subst (car domain) output f))
                           range
                           (all-mappings (cdr domain) range))))
  
  ; Uncurry uncurries a function:
  ; For example: ((uncurry f) '(1 2 3)) = (((f 1) 2) 3)
  (define (uncurry f)
    (define (iter f L)
      (if (null? L) f (iter (f (car L)) (cdr L))))
    (lambda (L) (iter f L)))
  
  ; Returns a list of all the maps from the product of the domains to the range.
  ; Each item in the list takes a list of values in the domains as an argument and returns an element from the range.
  (define (all-multi-mappings domains range)
    (map uncurry (foldr all-mappings range domains)))
  
  ; Make a list of mask functions of level n.
  ; The masks of level zero are just constant actions.
  ; The masks of level n+1 are the agents with each possible response strategy depending on the opponent's response to the masks of level n.
  (define (masks actions n)
    (if (= n 0)
        (map const actions)
        (let ((previous-strategies (masks actions (- n 1))))
          (map (lambda (strategy) (lambda (opponent) (strategy (map opponent previous-strategies))))
               (all-multi-mappings (map (const actions) previous-strategies) actions)))))
  
  ; A game is a list containing the action set and a utility function which turns two actions into a number.
  ; Here's the prisoner's dilema:
  (define game (list (list 'C 'D)
                     (lambda (my-action opp-action)
                       (define (action-case x c d)
                         (cond ((eq? x 'C) c)
                               ((eq? x 'D) d)
                               (else '?)))
                       (action-case my-action
                                    (action-case opp-action 2 0)
                                    (action-case opp-action 3 1)))))
  
  ; Here's the prisoner's dilema with nukes:
  (define nuke-game (list (list 'C 'D 'N)
                          (lambda (my-action opp-action)
                            (define (action-case x c d n)
                              (cond ((eq? x 'C) c)
                                    ((eq? x 'D) d)
                                    ((eq? x 'N) n)
                                    (else '?)))
                            (action-case my-action
                                         (action-case opp-action 2 0 -10)
                                         (action-case opp-action 3 1 -10)
                                         (action-case opp-action -10 -10 -10)))))
  
  ; Optimask(n) is an agent which tries its opponent against all the masks of level (n-1) and then behaves like the mask which obtained maximal utility against its opponent.
  (define (optimask game n)
    (let* ((actions (first game))
           (my-utility (second game))
           (my-masks (masks actions (- n 1))))
      (lambda (opponent)
        ((argmax (lambda (mask) (my-utility (mask opponent) (opponent mask))) my-masks) opponent))))
  
  
  ; Here are some functions to help build the opponents:
  (define (coop? x) (eq? x 'C))
  (define (defect? x) (eq? x 'D))
  (define (coop-if x) (if x 'C 'D)) 
  (define (guardian x) (lambda (opponent) (opponent x)))
  
  ; Some opponents for testing:
  (define coop-bot (const 'C))
  (define defect-bot (const 'D))
  (define justice-bot (guardian coop-bot))
  (define troll-bot (guardian defect-bot))
  
  (define prudent-troll-bot (lambda (x) (coop-if (and (defect? (x coop-bot)) (coop? (x defect-bot))))))
  
  (define (play f g) (list (f g) (g f)))
  
  )

