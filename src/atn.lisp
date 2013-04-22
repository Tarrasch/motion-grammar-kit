;;;; -*- Lisp -*-
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
;; ATN states
;;;;;;;;;;;;;;;

(defstruct (atn)
  name  ;; String description AND unique identifier
  nonterminal ;; The nonterminal the state is within
  type ;; (or 'start 'mid 'final)
  prod-id ;; (or fixnum nil), When type is 'mid, then a fixnum for prodution id, nil otherwise
          ;; TODO Define exactly what a prod-id refers to
  ) 

(defstruct (atn-state (:type list))
  name  ;; String description AND unique identifier
  nonterminal ;; The nonterminal the state is within
  type ;; (or 'start 'mid 'final)
  prod-id ;; (or fixnum nil), When type is 'mid, then a fixnum for prodution id, nil otherwise
          ;; TODO Define exactly what a prod-id refers to
  ) 

(defun init-atn-state (name nonterminal type pid)
  (let ((as (make-atn-state)))
    (setf (atn-state-name as) name
          (atn-state-nonterminal as) nonterminal
          (atn-state-type as) type
          (atn-state-prod-id as) pid 
          )
    as
    ) 
  )

(defun atn-start-name (head)
  (init-atn-state (format nil "p_~A" head) head 'start nil))

(defun atn-final-name (head)
  (init-atn-state (format nil "p_~A'" head) head 'final nil))

(defun atn-numeric-name (int head prod-id)
  (init-atn-state (format nil "p_~A" int) head 'mid prod-id))

(defun atn-state-compare (a b)
  "Compare two atn states"
  (string-compare (atn-state-name a) (atn-state-name b))) ;; TODO: This is real version
  ;;;(string-compare (or (atn-state-name a) a) (or (atn-state-name b) b)))  ;; TODO for debugging

(defun atn-state-equal (a b)
  (= 0 (atn-state-compare a b)))

(defun atn-state-final-p (p)
  (equal 'final (atn-state-type p)))

; TODO, Logical bug!! Fix case with empty productions
(defun grammar->ATN (grammar)
  "Return the ATN of the grammar in fa format. The ATN strictly speaking isn't
   a finite automata, but it's a natural way to encode it"
  (let ((state-counter 0)
        (prod-counter -1)
        )
    ;TODO fix the code duplication in ATN-numeric-name calls
    ;TODO prettify by having something like "grammar-map-grouped (lambda (head bodys))"
      (make-fa (apply #'append (grammar-map 'list (lambda (head body)
                                    (incf prod-counter)
                                    (labels ((chain (xs)
                                               (if xs
                                                 (cons (list (numeric state-counter) (car xs) (numeric (incf state-counter))) (chain (cdr xs)))
                                                 nil))
                                             (numeric (id) (atn-numeric-name id head prod-counter))
                                             )
                                    (let* ((beg (list (ATN-START-NAME head) :epsilon (numeric (incf state-counter))))
                                           (mids (chain body))
                                           (end (list (numeric state-counter) :epsilon (ATN-FINAL-NAME head)))
                                           )
                                      `(,beg ,@mids ,end)
                                      ))
                                    ) grammar)) nil nil)) )
