;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2013-2013, Georgia Tech Research Corporation
;;;; All rights reserved.
;;;;
;;;; Author(s): Arash Rouhani <rarash@gatech.edu>
;;;; Georgia Tech Humanoid Robotics Lab
;;;;
;;;; This file is provided under the following "BSD-style" License:
;;;;
;;;;   Redistribution and use in source and binary forms, with or
;;;;   without modification, are permitted provided that the following
;;;;   conditions are met:
;;;;   * Redistributions of source code must retain the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer.
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;;   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;;   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;;   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;;;   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;;;;   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :motion-grammar-kit)

;;;;;;;;;;;;;;;
;; Subset construction (ATM to one DFA for each nonterminal)
;;;;;;;;;;;;;;;

(defstruct (dd) ;; dd - A soon to be state in the predictive automaton (not an ATN-state!)
  configurations  ;; Set of configurations contained within the dd
  busy-set ;; Set of configurations that closure already have visited
  recursive-alternatives ;; Set of productions that have ever recursed when closuring
  overflown ;; We set this if we have overflown
  non-regular ;; We set this if we suspect that the grammar is non-regular
  name ;; An optional pretty name for the node
  )

(defstruct (atnconf (:type list))
  state ;; String name for atn-state
  alternative ;; Fixnum, the index of the production
  stack ;; Represented with a list here
  )

(defun dd-equivalent (a b)
  ;; TODO Make it O(n) or O(nlog(n)) or at least smarter than O(n^2)!
  ;(finite-set-equal (dd-configurations a) (dd-configurations b))
  (let* ((csa (dd-configurations a))
         (csb (dd-configurations b))
         ;(andor (lambda (matrix) (fold (lambda (x y) (and x y)) t (fold (lambda (x y) (or x y)) nil matrix))))
         (andor (lambda (matrix) (funcall #'every (lambda (row) (funcall #'some #'identity row)) matrix)))
         )
    (funcall andor (map-finite-set 'list (lambda (ca)
                                           (map-finite-set 'list (lambda (cb)
                                                                   (atnconf-equivalent ca cb)) csb)) csa))))

(defun dd-unique-atn-states (ds)
  (remove-duplicates (map-finite-set 'list #'atnconf-state (dd-configurations ds)))
  )

(defun fill-atnconf (state alternative stack)
  (let ((a (make-atnconf)))
    (setf (atnconf-state a) state
          (atnconf-alternative a) alternative
          (atnconf-stack a) stack
          )
    a
    )
  )

(defun empty-atnconf-set ()
  (make-finite-set :compare #'atnconf-compare)
  )

(defun singleton-atnconf-set (elem)
  (finite-set-add (empty-atnconf-set) elem)
  )

(defun make-empty-dd (&optional (name "default"))
  (let ((d (make-dd)))
    (setf (dd-configurations d) (empty-atnconf-set)
          (dd-busy-set d) (empty-atnconf-set)
          (dd-recursive-alternatives d) (make-finite-set :compare #'fixnum-compare)
          (dd-overflown d) nil
          (dd-non-regular d) nil
          (dd-name d) name
          )
    d
    )
  )

(defun atnconf-compare (a b)
  (compare-nest
    (atn-state-compare (atnconf-state a) (atnconf-state b))
    (fixnum-compare (atnconf-alternative a) (atnconf-alternative b))
    (stack-compare (atnconf-stack a) (atnconf-stack b))))

(defun stack-compare (a b)
  (gsymbol-compare (mapcar #'atn-state-name a) (mapcar #'atn-state-name b))
  )

(defun atnconf-equivalent (a b)
  (and (atn-state-equal (atnconf-state a) (atnconf-state b))
       (= (atnconf-alternative a) (atnconf-alternative b))
       (stack-equivalent (atnconf-stack a) (atnconf-stack b)))
  )

(defun stack-equivalent (a b)
  (stack-equivalent-helper (reverse a) (reverse b)))

(defun stack-equivalent-helper (a b)
  (cond
    ((null a) t)
    ((null b) t)
    ((atn-state-equal (car a) (car b)) (stack-equivalent (cdr a) (cdr b)))
    (t nil)
    )
  )

(defmacro replicate (num formula)
  "If formula is a function, the function will be called multiple times"
  `(vector ,@(make-list num :initial-element formula))
  )

(defmacro dd-configurations-union (state rhs-set)
  `(setf (dd-configurations ,state) (finite-set-union (dd-configurations ,state)
                                                      ,rhs-set))

  )

(defmacro += (lhs rhs)
  "Take union of lhs and rhs and store in lhs"
  `(setf ,lhs (finite-set-union ,lhs ,rhs))
  )

(defun nonterminalp (sym)
  (string-upcase-p (symbol-name sym)))

(defun terminalp (sym)
  (string-downcase-p (symbol-name sym)))

(defun atn-closure (atn ds atn-config)
  (destructuring-bind (p i gamma) atn-config
    (if (finite-set-inp atn-config (dd-busy-set ds))
      (return-from atn-closure (empty-atnconf-set))
      (setf (dd-busy-set ds) (finite-set-add (dd-busy-set ds) atn-config ))
      )
    (let ((res (singleton-atnconf-set atn-config))
          )
      (labels ((recurse (new-p new-gamma) (atn-closure atn ds (list new-p i new-gamma))))
        (when (atn-state-final-p p)
          ;(print "wohoo-final")
          (if (car gamma)
            (+= res (recurse (car gamma) (cdr gamma)))
            (fa-map-edges nil (pattern-lambda (p1 (equal (atn-state-nonterminal p)) p2)
                                ;(declare (ignore p1))
                                (+= res (recurse p2 nil))) atn)
            )
          )
        (fa-map-edge-lists nil (lambda (edge)
                                 ;(print edge)
                                 (pattern-case edge
                                   ((:pattern (:predicate atn-state-equal p) :epsilon q)
                                    ;(print "wohoo-eps")
                                    (+= res (recurse q gamma)))
                                   ((:pattern (:predicate atn-state-equal p) sym q) ;; TODO: replace sym with (and) pattern
                                    (when (nonterminalp sym)
                                      ;(print "wohoo-nonterminal-gonna-recurse")
                                      (let ((depth (count q gamma)))
                                        (when (>= depth 1)
                                          (finite-set-nadd (dd-recursive-alternatives ds) i)
                                          (when (> (finite-set-length (dd-recursive-alternatives ds)) 1)
                                            (setf (dd-non-regular ds) t)
                                            (error "Likely not a LL regular grammar, at configuration ~A, these alternatives have visited ~A"
                                                   atn-config
                                                   (dd-recursive-alternatives ds)))
                                          (when (> depth 10) ; Some limit
                                            (setf (dd-overflown ds) t)
                                            (error (format nil "Recursion limit overflow in atnconfig ~A" atn-config))
                                            (return-from atn-closure res) ;; I don't see why the paper does this!
                                            )

                                          )
                                        (+= res (recurse (ATN-START-NAME sym) (cons q gamma)))
                                        )
                                      )
                                    )
                                   )
                                 ) atn)
        res
        ))
    )
  )

(defmacro while (test &rest body)
  `(do ()
     ((not ,test))
     ,@body))

(defun find-predicted-alternatives (ds)
  ;(apply #'finite-set (finite-set-map 'list #'atnconf-alternative configs))
  (apply #'finite-set (finite-set-map 'list #'atnconf-alternative (dd-configurations ds)))
  )

(defun resolve (ds)
  nil ;; TODO understand why/if this function is necessary and what it exactly does
  )

(defun atn-move (atn ds terminal)
  "Which are the reachable configurations from ds if we would consume terminal"
  ;; We assume any terminal is satisfiable
  ;; Note: In an atn graph, there can never be more than one terminal edge from any node
  ;; This means that given that the configurations are unique, so will our
  ;; output be even if we don't put it in a set ever
  ;;
  ;; TODO: good god clean this up!
  (apply #'finite-set-custom (empty-atnconf-set)
         (remove nil (map-finite-set 'list
                                     (lambda (p) (car (remove nil (fa-map-edges 'list (pattern-lambda ((:predicate atn-state-equal (atnconf-state p)) (equal terminal) q)
                                                                                        (fill-atnconf q (atnconf-alternative p) (atnconf-stack p))) atn))))
                                     ;; TODO Super-inefficient
                                     (dd-configurations ds))))
  )

(defun atn-get-terminals (atn ds)
  ;; TODO: This function is subject to change once we change the meaning of being terminal
  (remove nil (remove-duplicates (map-finite-set 'list (lambda (p1)
                                             (car (remove nil (fa-map-edges 'list (pattern-lambda ((:predicate atn-state-equal p1) terminal _p2) (and (terminalp terminal) terminal)) atn))) ;; TODO: Super-inefficient
                                             ) (dd-unique-atn-states ds))))
  )

(defun atnconf-conflicts (a b)
  "Yes if a and b are in conflict"
  (and (/= (atnconf-alternative a) (atnconf-alternative b))
       (atn-state-equal (atnconf-state a) (atnconf-state b))
       (stack-equivalent (atnconf-stack a) (atnconf-stack b))))

(defun dd-calc-conflicts (ds)
  ;(index-finite-set (configurations ds) (compose #'atn-state-name #'atnconf-state) #'identity )
  (remove-if (compose (curry #'= 1) #'length) (partition-finite-set (dd-configurations ds) #'atnconf-conflicts)))

(defun create-dfa (grammar nonterminal)
  ;; TODO Pass the atn as argument so you don't need to create a new one each time
  (let* ((atn (grammar->ATN grammar))
         (atn-state (ATN-start-name nonterminal))
         (ds0 (make-empty-dd "s0"))
         (states nil) ;; TODO use tree-set instead or maybe not because dunno how find will work then
         (queue nil)
         ;(num-prods (fold-fa-edges (lambda (c from _ _2) (+ c (if (equal atn-state from) 1 0))) 0 atn)) ;; TODO: This is stupid ...
         ;(final-states (make-sequence 'vector num-prods :initial-element (make-empty-dd))) ;; TODO: make real copies
         ;(final-states (replicate num-prods (make-empty-dd))) ;; DONE
         ;(final-states (make-sequence 'vector (length grammar) :initial-element nil))
         ;(final-states (map 'vector #'identity (loop for i from 1 to (length grammar) collect (make-empty-dd (format nil "final => ~A" i)))))
         (final-states (map 'vector #'identity (loop for i from 1 to (length grammar) collect (make-empty-dd "remove"))))
         (reachable-alternatives (make-finite-set))
         (edges nil)
         (counter 0) ;; For name generation
         )
    (fa-map-edges nil (pattern-lambda ((:predicate atn-state-equal atn-state) :epsilon other)
                        ;(setf (dd-configurations ds0) (finite-set-union (dd-configurations ds0)
                        ;                                                     (closure ds0 (list other 123 nil))))
                        ;(dd-configurations-union ds0 (atn-closure atn ds0 (list other 456 nil)))
                        (+= (dd-configurations ds0) (atn-closure atn ds0 (list other (atn-state-prod-id other) nil)))
                        ) atn)
    (setf (dd-busy-set ds0) "Forgotten") ;; To make prints more readable
    (when (finite-set-empty-p (dd-configurations ds0))
      (return-from create-dfa "Doh!")
      )
    (push ds0 queue)
    (push ds0 states)
    ;(print ds0)
    (while (car queue)
           (let* ((ds (pop queue))
                  (terminals (atn-get-terminals atn ds)))
             ;; TODO: Here is the place to do an very extensive analysis of the terminals we have
             ;; Do mutators conflict? Do predicates overlap? Do predicates cover everything, hence no syntax errors possible
             ;;
             ;; To begin with however I just do treat them as tokens in the traditional sense
             ;(print terminals)
             (finite-set-map 'nil
                             (lambda (terminal)
                               ;(print terminal)
                               (let* ((moveset (atn-move atn ds terminal))
                                      (ds-new (make-empty-dd (format nil "s~A" (incf counter))))
                                      (clos (curry #'atn-closure atn ds-new))
                                     )
                                 nil
                                 (+= (dd-configurations ds-new) (fold-finite-set (lambda (acc atnconf) (finite-set-union acc (funcall clos atnconf)))
                                                                                 (empty-atnconf-set)
                                                                                 moveset))
                                 (setf (dd-busy-set ds-new) "Forgotten") ;; To make prints more readable
                                 ;(print "start search in states")
                                 ;(print ds-new)
                                 (let ((search-result (find ds-new states :test #'dd-equivalent)))
                                   ;(if search-result (print "Found my guy!!") (print "This is truly a new ds"))
                                   (if search-result
                                     (setf ds-new search-result)
                                     (progn ;; Here we start to do some checking for ambiguities and overflows etc
                                            (cond
                                              ((dd-overflown ds-new) (error "Recursion limit overflow, couldn't synthesize"))
                                              ((dd-non-regular ds-new) (error "I can't show that the grammar is LL-regular, sorry I can't make a dfa"))
                                              ((car (dd-calc-conflicts ds-new)) (error (format nil "Ambigious grammar, conlict set: ~A" (dd-calc-conflicts ds-new)))))
                                            (push ds-new states)
                                            )
                                     )
                                   (if-pattern (:pattern j) (find-predicted-alternatives ds-new)
                                               (setf ds-new (aref final-states j)
                                                     (dd-name ds-new) (format nil "final=>~A" j)
                                                     reachable-alternatives (finite-set-add reachable-alternatives j)
                                                     ;(dd-name ds-new) (format nil "~A=>~A" (dd-name ds-new) j)
                                                     )
                                               ;
                                               ;(when (not search-result) (push ds-new queue))
                                               (when (not search-result) (setf queue `(,@queue ,ds-new))) ;; TODO: This should be stack not queue! Order doesn't matter I just did this for easier debugging
                                               )


                                   )
                                 (push (list (dd-name ds) terminal (dd-name ds-new)) edges) ;; Probably not necessary
                                 ;(push (list ds terminal ds-new) edges)
                                 )
                               ) terminals)
             )
           )
    ;(print "Ok guys I'm gonna do a finite automata")
    ;(print edges)
    ;(print ds0)
    ;(print final-states)
    ;(make-fa edges ds0 (remove nil (map 'list #'identity final-states)))

    (unless (finite-set-equal (find-predicted-alternatives ds0) reachable-alternatives)
      (error "Some productions in ~A are unexpandable, namely these ~A" nonterminal (finite-set-difference (find-predicted-alternatives ds0) reachable-alternatives)))
    ;(make-fa edges (dd-name ds0) (map 'list #'dd-name (remove nil final-states)))
    (make-fa edges (dd-name ds0) (remove-if (curry #'string= "remove") (map 'list #'dd-name final-states)))


    ;(let ((counter 0))
    ;  (make-fa edges (dd-name ds0) (map 'list (lambda (ds) (format nil "~A ==> ~A" (dd-name ds) (incf counter))) (remove nil final-states)))
    ;  )

    )
  )
