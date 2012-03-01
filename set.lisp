;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2012, Georgia Tech Research Corporation
;;;; All rights reserved.
;;;;
;;;; Author(s): Neil T. Dantam <ntd@gatech.edu>
;;;; Georgia Tech Humanoid Robotics Lab
;;;; Under Direction of Prof. Mike Stilman
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



;;;; FILE: set.lisp
;;;; BRIEF: basic operations for finite sets
;;;; AUTHOR: Neil T. Dantam
;;;;
;;;; Common Lisp includes set operations for finite sets represented
;;;; as lists, however these may become slow as sets grow very large.
;;;; Thus, we abstract the set operations to allow for easy
;;;; replacement of underlying representation with something like hash
;;;; tables, search trees, or bit vectors later.


(defun make-finite-set ()
  nil)

(defun finite-set-map (result-type function set)
  "Apply FUNCTION to all members of SET."
  (etypecase set
    (list (map result-type function set))))

(defmacro do-finite-set ((var set &optional result-form) &body body)
  "Iterate over members of the set."
  (alexandria:with-gensyms (set-var)
    `(let ((,set-var ,set))
       (etypecase ,set-var
         (list (dolist (,var ,set-var ,result-form)
                 ,@body))))))

(defun finite-set-fold (function initial-value set)
  (etypecase set
    (sequence (reduce function set :initial-value initial-value))))

(defun finite-set-equal (a b)
  "Are sets A and B equal?"
  (cond
    ((and (listp a) (listp b))
     (and (null (set-difference a b :test #'equal))
          (null (set-difference b a :test #'equal))))
    (t
     (error "Can't operate on ~A and ~B" a b))))

(defun finite-set-member (set item)
  "Is ITEM a member of SET?"
  (etypecase set
      (sequence
       (find item set))))

(defun finite-set-add (set item)
  "Return a new set containing ITEM and all members of SET."
  (etypecase set
    (list (if (finite-set-member set item)
              set
              (cons item set)))))

