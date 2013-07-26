;; guile-bdd - A behavior driven development framework for GNU Guile
;; Copyright (C) 2013  Cyrill Schenkel
;;
;; This file is part of guile-bdd.
;;
;; guile-bdd is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-bdd is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see {http://www.gnu.org/licenses/}.

(use-modules (ice-9 format)
	     (test bdd))

(load "test.scm")

;; test data

(define test-purpose "test the bdd framework")
(define stakeholder "developer")
(define intention "create a fictional story")
(define scenario-format "test format ~a")

(define-story story-with-one-scenario "Story with one scenario"
  (in-order-to test-purpose)
  (as-a stakeholder)
  (i-want-to intention)
  (scenario 1 (format #f scenario-format 1)
	    (given ((x 5))
	      (do-some-initialization)
	      (and-some-more))
	    (when ()
	      (some-action))
	    (then
		(assert-this-and-that))))

(define-story story-with-three-scenarios "Story with three scenarios"
  (in-order-to test-purpose)
  (as-a stakeholder)
  (i-want-to intention)
  (scenario 1 (format #f scenario-format 1)
	    (given ((x 5))
	      (do-some-initialization)
	      (and-some-more))
	    (when ()
	      (some-action))
	    (then
		(assert-this-and-that)))
  (scenario 2 (format #f scenario-format 2)
	    (given ()
	      (do-some-initialization)
	      (and-some-more))
	    (when ((y 6))
	      (some-action))
	    (then
		(assert-this-and-that)))
  (scenario 4 (format #f scenario-format 4)
	    (given ())
	    (when ()
	      (some-action))
	    (then
		(assert-this-and-that))))

(define-story story-without-scenarios "Story without scenarios"
  (in-order-to test-purpose)
  (as-a stakeholder)
  (i-want-to intention))

;; test cases

(define story-test-suite
  `("story test suite" .
    (,(lambda ()
	(define context "Should return (1) if story has only one scenario with
the number 1.")
	
	;; Given
	;; story-with-one-scenario

	;; When
	(define actual-result (story-scenario-numbers story-with-one-scenario))

	;; Then
	(assert-equal context '(1) actual-result))
     ,(lambda ()
	(define context "Should return (1 2 4) if story has 3 scenarios with
the numbers 1, 2 and 4.")
	
	;; Given
	;; story-with-three-scenarios

	;; When
	(define actual-result (story-scenario-numbers
			       story-with-three-scenarios))

	;; Then
	(assert-equal context '(1 2 4) actual-result))
     ,(lambda ()
	(define context "Should return () if story don't have a scenario.")
	
	;; Given
	;; story-without-scenarios

	;; When
	(define actual-result (story-scenario-numbers
			       story-without-scenarios))

	;; Then
	(assert-equal context '() actual-result)))))
