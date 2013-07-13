#lang scheme
;;; This is an interpreter for Eliezer's eval heirarchy.

;;; An agent is a scheme expression which evaluates to a two place function.  It takes as arguments a play function and an opponent.
;;; Agents are not allowed to call eval or the global play function, they may only use the play function to see the result of one agent playing against another.

; Here are some helper functions we can call inside of agents to help build them.
(define (coop? x) (eq? x 'C))
(define (defect? x) (eq? x 'D))
(define (coop-if x) (if x 'C 'D))
(define (make-mask play agent mask) (lambda (p a1 a2) (if (equal? a1 agent) (play p mask a2) (play p a1 a2))))


; By default, play just evaluates the first agent with the second agent's code as an argument.  The play-fn passed in is the only code agents should call to evaluate other agents.
(define (play play-fn agent1 agent2)
  ((eval agent1) play-fn agent2))

; behaves-like is one example argument for play*.  It makes 
(define (mask-play agent mask) `(lambda (play a1 a2) (play (if (equals? a1 ',agent) ',mask a1) a2)))

;;; Convenience for humans:
; Match is a convenience for humans to look at the result of a tournament.
(define (match agent1 agent2) (list (play play agent1 agent2) (play play agent2 agent1)))

;;; Some example agents

; make-quine-bot takes a piece of code that contains an unbound variable named "source" and makes it into an agent in which the variable source is bound to 
(define (make-quine-bot code)
  `((lambda (quine) (let ((source (list quine (list (quote quote) quine)))) ,code))
    '(lambda (quine) (let ((source (list quine (list (quote quote) quine)))) ,code))))

; Mask-bot does whatever its opponent does would do against it 


(define (const-agent x) `(lambda (play opponent) ',x))
(define coop-bot (const-agent 'C))
(define defect-bot (const-agent 'D))

(define (guardian mask) `(lambda (play opponent) (play play opponent ',mask)))

(define (mask-guardian-bot mask)
  (make-quine-bot `(lambda (play opponent) (play (make-mask play source ',mask) opponent source))))

(define justice-bot (mask-guardian-bot coop-bot))
(define troll-bot (mask-guardian-bot defect-bot))

(define prudent-troll-bot
  (make-quine-bot
                           `(lambda (play opponent) (coop-if (and (defect? (play (make-mask play source ',coop-bot) opponent source))
                                                                 (coop? (play (make-mask play source ',defect-bot) opponent source)))))))

;;; Now here's optimask implemented using the eval heirarchy:

; Map and then concatenate:
(define (concat-map f L) (apply append (map f L)))

; Take the cartesian product of two lists.  For example:
; (cartesian-product f '(a b) '(1 2)) = (list (f 'a 1) (f 'a 2) (f 'b 1) (f 'b 2))
(define (cartesian-product f L1 L2) (concat-map (lambda (a1) (map (lambda (a2) (f a1 a2)) L2)) L1))

(define (multi-cartesian-product f LL)
  (map (lambda (L) (apply f L))
       (foldr (lambda (L combinations) (cartesian-product cons L combinations))
              '(())
              LL)))

; Return the list of all mappings from domain to range.
(define (all-mappings domain range)
  (foldr (lambda (x L)
           (cartesian-product (lambda (output mapping) (cons (list x output) mapping)) range L))
         '(())
         domain))

; Returns a list of all the maps from the product of the domains to the range.
; Each item in the list takes a list of values in the domains as an argument and returns an element from the range.
(define (all-multi-mappings domains range) (all-mappings (multi-cartesian-product list domains) range))

; Take a mapping and turn it into the code for a scheme function.
(define (mapping->code mapping)
  (define (cond-clause L) `((equal? input ',(first L)) ',(second L)))
  `(lambda (input) (cond ,@(map cond-clause mapping))))


; Make a list of mask functions of level n.
; The masks of level zero are just constant actions.
; The masks of level n+1 are the agents with each possible response strategy depending on the opponent's response to the masks of level n.
(define (masks actions n)
  (if (= n 0)
      (map const-agent actions)
      (let ((previous-strategies (masks actions (- n 1))))
        (map (lambda (strategy)
               (make-quine-bot `(lambda (play opponent) (,(mapping->code strategy) (map (lambda (s) (play (make-mask play source s) opponent source)) ',previous-strategies)))))
             (all-multi-mappings (map (const actions) previous-strategies) actions)))))

; A game is a list containing the action set and a utility function which turns two actions into a number.
; Here's the prisoner's dilema:
(define game (list '(C D)
                   '(lambda (my-action opp-action)
                      (define (action-case x c d)
                        (cond ((eq? x 'C) c)
                              ((eq? x 'D) d)
                              (else '?)))
                      (action-case my-action
                                   (action-case opp-action 2 0)
                                   (action-case opp-action 3 1)))))

; Here's the prisoner's dilema with nukes:
(define nuke-game (list '(C D N)
                        '(lambda (my-action opp-action)
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
    (make-quine-bot
     `(lambda (play opponent)
        (play
         play
         (argmax
          (lambda (mask)
            (let ((play* (make-mask play source mask)))
              (,my-utility (play play* mask opponent) (play play* opponent mask))))
          ',my-masks)
         opponent)))))


