#!/usr/bin/guile \
-L ../src/
!#
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


;;; test helper functions

(define (assertion-error msg)
  (display msg)
  (newline) #f)

(define (assert-equal context expected actual)
  (if (not (equal? expected actual))
      (assertion-error
       (format #f "~a~%  The actual value is not equal to the expected value.
    [Actual  ] ~s => ~a
    [Expected] ~s => ~a" context actual actual expected expected)) #t))

(define (assert-not-equal context unexpected actual)
  (if (equal? unexpected actual)
      (assertion-error
       (format #f "~a~%  The actual value is equal to the unexpected value.
    [Actual  ] ~s => ~a
    [Expected] ~s => ~a" context actual actual
    `(not ,unexpected) `(not ,unexpected))) #t))

(define (ignore test-case)
  (lambda () 0))

(define (run-suite suite)
  (format #t "RUN SUITE: ~a~%" (car suite))
  (map (lambda (test-case)
	 (test-case)) (cdr suite)))

(define (run-suites . suites)
  (evaluate-results (map count-results (map run-suite suites))))

(define (count-results results)
  (let count-next-item ((remaining-results results) (result '(0 0 0)))
    (if (null? remaining-results)
	result
	(count-next-item
	 (cdr remaining-results)
	 (case (car remaining-results)
	   ((#t) (cons (1+ (car result)) (cdr result)))
	   ((#f) (list (car result) (1+ (cadr result)) (caddr result)))
	   ((0) (list (car result) (cadr result) (1+ (caddr result)))))))))

(define (evaluate-results results)
  (let eval-next ((remaining-results results) (result '(0 0 0)))
    (if (null? remaining-results)
	result
	(eval-next (cdr remaining-results)
		   (map + (car remaining-results) result)))))

;;; bdd.scm tests

;; test data

(define test-purpose "test the bdd framework")
(define stakeholder "developer")
(define intention "create a fictional story")

(define-story story-with-one-scenario "Story with one scenario"
  (in-order-to test-purpose)
  (as-a stakeholder)
  (i-want-to intention)
  (scenario 1 (format #f "test scenario ~a" 1)
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
  (scenario 1 (format #f "test scenario ~a" 1)
	    (given ((x 5))
	      (do-some-initialization)
	      (and-some-more))
	    (when ()
	      (some-action))
	    (then
		(assert-this-and-that)))
  (scenario 2 (format #f "test scenario ~a" 2)
	    (given ()
	      (do-some-initialization)
	      (and-some-more))
	    (when ((y 6))
	      (some-action))
	    (then
		(assert-this-and-that)))
  (scenario 4 (format #f "test scenario ~a" 4)
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

(define story-scenario-numbers-test
  `("story-scenario-numbers test" .
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

(define story-description-test
  `("story-description test" .
    (,(lambda ()
	(define context "should return the story description")
	
	;; Given
	;; story-with-one-scenario

	;; When
	(define actual-returned-description
	  (story-description story-with-one-scenario))

	;; Then
	(assert-equal context "Story with one scenario"
		      actual-returned-description)))))

(define story-intention-test
  `("story-intention test" .
    (,(lambda ()
	(define context "should return the story intention")
	
	;; Given
	;; story-with-one-scenario

	;; When
	(define actual-returned-intention
	  (story-intention story-with-one-scenario))

	;; Then
	(assert-equal context intention
		      actual-returned-intention)))))

(define story-who-test
  `("story-who test" .
    (,(lambda ()
	(define context "should return the stakeholder of the story")
	
	;; Given
	;; story-without-scenarios

	;; When
	(define actual-returned-stakeholder
	  (story-who story-without-scenarios))

	;; Then
	(assert-equal context stakeholder
		      actual-returned-stakeholder)))))

(define story-purpose-test
  `("story-purpose test" .
    (,(lambda ()
	(define context "should return the story purpose")
	
	;; Given
	;; story-with-one-scenario

	;; When
	(define actual-returned-purpose
	  (story-purpose story-with-one-scenario))

	;; Then
	(assert-equal context test-purpose
		      actual-returned-purpose)))))

(define story-scenario-test
  `("story-scenario test" .
    (,(lambda ()
	(define context
	  "should return the story scenario with the given number 4")
	
	;; Given
	;; story-with-three-scenarios

	;; When
	(define actual-returned-scenario
	  (story-scenario story-with-three-scenarios
			  4))

	;; Then
	(assert-equal
	 context 
	 (scenario 4 (format #f "test scenario ~a" 4)
		   (given ()) (when () (some-action))
		   (then (assert-this-and-that)))
	 actual-returned-scenario))
     ,(lambda ()
	(define context
	  "should return the story scenario with the given number 1")
	
	;; Given
	;; story-with-three-scenarios

	;; When
	(define actual-returned-scenario
	  (story-scenario story-with-three-scenarios
			  1))

	;; Then
	(assert-equal
	 context 
	 (scenario 1 (format #f "test scenario ~a" 1)
		   (given ((x 5))
		     (do-some-initialization)
		     (and-some-more))
		   (when () (some-action))
		   (then (assert-this-and-that)))
	 actual-returned-scenario))
     ,(lambda ()
	(define context "should return '() because the story doesn't have a
scenario with the number 3")
	
	;; Given
	;; story-with-three-scenarios

	;; When
	(define actual-returned-scenario
	  (story-scenario story-with-three-scenarios
			  3))

	;; Then
	(assert-equal context '() actual-returned-scenario)))))

(define scenario-actions-test
  `("scenario-actions test" .
    (,(lambda ()
	(define context "should return the scenario actions")
	
	;; Given
	(define given-scenario
	  (scenario 1 "scenario-description"
		    (given ())
		    (when () (test-action))
		    (then)))

	;; When
	(define actual-returned-actions
	  (scenario-actions given-scenario))

	;; Then
	(assert-equal context '((test-action))
		      actual-returned-actions)))))

(define scenario-actual-outputs-test
  `("scenario-actual-outputs test" .
    (,(lambda ()
	(define context "should return the actual outputs of the scenario")
	
	;; Given
	(define given-scenario
	  (scenario 1 "scenario-description"
		    (given ())
		    (when ((out 1)))
		    (then)))

	;; When
	(define actual-returned-actual-outputs
	  (scenario-actual-outputs given-scenario))

	;; Then
	(assert-equal context '((out 1))
		      actual-returned-actual-outputs)))))

(define scenario-assertions-test
  `("scenario-assertions test" .
    (,(lambda ()
	(define context "should return the assertions of the scenario")
	
	;; Given
	(define given-scenario
	  (scenario 1 "scenario-description"
		    (given ())
		    (when ())
		    (then (assertion1) (assertion2))))

	;; When
	(define actual-returned-assertions
	  (scenario-assertions given-scenario))

	;; Then
	(assert-equal context '((assertion1) (assertion2))
		      actual-returned-assertions)))))

(define scenario-description-test
  `("scenario-description test" .
    (,(lambda ()
	(define context "should return the description of the scenario")
	
	;; Given
	(define given-description "scenario-description")
	(define given-scenario
	  (scenario 1 given-description
		    (given ())
		    (when ())
		    (then (assertion1) (assertion2))))

	;; When
	(define actual-returned-description
	  (scenario-description given-scenario))

	;; Then
	(assert-equal context given-description
		      actual-returned-description)))))

(define scenario-given-inputs-test
  `("scenario-given-inputs test" .
    (,(lambda ()
	(define context "should return the given inputs of the scenario")
	
	;; Given
	(define given-scenario
	  (scenario 1 "scenario-description"
		    (given ((in 5)))
		    (when ())
		    (then)))

	;; When
	(define actual-returned-given-inputs
	  (scenario-given-inputs given-scenario))

	;; Then
	(assert-equal context '((in 5))
		      actual-returned-given-inputs)))))

(define scenario-initialization-logic-test
  `("scenario-initialization-logic test" .
    (,(lambda ()
	(define context
	  "should return the initialization logic of the scenario")
	
	;; Given
	(define given-scenario
	  (scenario 1 "scenario-description"
		    (given ()
		      (init))
		    (when ())
		    (then)))

	;; When
	(define actual-returned-initialization-logic
	  (scenario-initialization-logic given-scenario))

	;; Then
	(assert-equal context '((init))
		      actual-returned-initialization-logic)))))

(define scenario-number-test
  `("scenario-number test" .
    (,(lambda ()
	(define context
	  "should return the number of the scenario")
	
	;; Given
	(define given-scenario
	  (scenario 90 "scenario-description"
		    (given ())
		    (when ())
		    (then)))

	;; When
	(define actual-returned-number
	  (scenario-number given-scenario))

	;; Then
	(assert-equal context 90 actual-returned-number)))))

(define assert-that-test
  `("assert-that test" .
    (,(lambda ()
	(define context
	  "should throw an assertion-exception if the values aren't
equal")
	;; Given
	(define given-actual-value 1)
	(define given-expected-value "test")
	
	;; When
	(define actual-result
	  (catch 'assertion-exception
	    (lambda () (assert-that given-actual-value is given-expected-value)
		    #t)
	    (lambda (key assertion-error) assertion-error)))

	;; Then
	(assert-not-equal context #t actual-result)
	(assert-equal context #t (assertion-error? actual-result)))
     ,(lambda ()
	(define context
	  "should throw an assertion-exception if the values are
equal")
	;; Given
	(define given-actual-value 1)
	(define given-expected-value 1)
	
	;; When
	(define actual-result
	  (catch 'assertion-exception
	    (lambda ()
	      (assert-that given-actual-value is not given-expected-value)
	      #t)
	    (lambda (key assertion-error) assertion-error)))

	;; Then
	(assert-not-equal context actual-result #t)
	(assert-equal context (assertion-error? actual-result) #t))
     ,(lambda ()
	(define context
	  "should throw an assertion-exception if the value is '()")
	;; Given
	(define given-actual-value '())
	
	;; When
	(define actual-result
	  (catch 'assertion-exception
	    (lambda () (assert-that given-actual-value is not null)
		    #t)
	    (lambda (key assertion-error) assertion-error)))

	;; Then
	(assert-not-equal context actual-result #t)
	(assert-equal context (assertion-error? actual-result) #t))
     ,(lambda ()
	(define context
	  "should throw an assertion-exception if the value is not '()")
	;; Given
	(define given-actual-value 3)
	
	;; When
	(define actual-result
	  (catch 'assertion-exception
	    (lambda () (assert-that given-actual-value is null)
		    #t)
	    (lambda (key assertion-error) assertion-error)))

	;; Then
	(assert-not-equal context actual-result #t)
	(assert-equal context (assertion-error? actual-result) #t))
     ,(lambda ()
	(define context
	  "should throw an assertion-exception if the comparator
function returns `#f'")
	;; Given
	(define given-actual-value 3)
	(define given-expected-value 4)
	
	;; When
	(define actual-result
	  (catch 'assertion-exception
	    (lambda () (assert-that given-actual-value > given-expected-value)
		    #t)
	    (lambda (key assertion-error) assertion-error)))

	;; Then
	(assert-not-equal context #t actual-result)
	(assert-equal context #t (assertion-error? actual-result)))
     ,(lambda ()
	(define context
	  "shouldn't throw an assertion-exception if the values are
equal")
	;; Given
	(define given-actual-value "test")
	(define given-expected-value "test")
	
	;; When
	(define actual-result
	  (catch 'assertion-exception
	    (lambda () (assert-that given-actual-value is given-expected-value)
		    #t)
	    (lambda (key assertion-error) assertion-error)))

	;; Then
	(assert-equal context actual-result #t))
     ,(lambda ()
	(define context
	  "shouldn't throw an assertion-exception if the values aren't
equal")
	;; Given
	(define given-actual-value 1)
	(define given-expected-value 3)
	
	;; When
	(define actual-result
	  (catch 'assertion-exception
	    (lambda ()
	      (assert-that given-actual-value is not given-expected-value)
	      #t)
	    (lambda (key assertion-error) assertion-error)))

	;; Then
	(assert-equal context actual-result #t))
     ,(lambda ()
	(define context
	  "shouldn't throw an assertion-exception if the value isn't '()")
	;; Given
	(define given-actual-value 3)
	
	;; When
	(define actual-result
	  (catch 'assertion-exception
	    (lambda () (assert-that given-actual-value is not null)
		    #t)
	    (lambda (key assertion-error) assertion-error)))

	;; Then
	(assert-equal context actual-result #t))
     ,(lambda ()
	(define context
	  "shouldn't throw an assertion-exception if the value is '()")
	;; Given
	(define given-actual-value '())
	
	;; When
	(define actual-result
	  (catch 'assertion-exception
	    (lambda () (assert-that given-actual-value is null)
		    #t)
	    (lambda (key assertion-error) assertion-error)))

	;; Then
	(assert-equal context actual-result #t))
     ,(lambda ()
	(define context
	  "shouldn't throw an assertion-exception if the comparator
function returns `#t'")
	;; Given
	(define given-actual-value 4)
	(define given-expected-value 3)
	
	;; When
	(define actual-result
	  (catch 'assertion-exception
	    (lambda () (assert-that given-actual-value > given-expected-value)
		    #t)
	    (lambda (key assertion-error) assertion-error)))

	;; Then
	(assert-equal context #t actual-result)))))

(define assertion-error-type-test
  `("assertion-error-type test" .
    (,(lambda ()
	(define context
	  "should return the type of the assertion-error")
	
	;; Given
	(define given-assertion-error
	  (make-assertion-error 'test "test-message" '()))

	;; When
	(define actual-returned-type
	  (assertion-error-type given-assertion-error))

	;; Then
	(assert-equal context 'test actual-returned-type)))))

(define assertion-error-message-test
  `("assertion-error-message test" .
    (,(lambda ()
	(define context
	  "should return the error message of the assertion-error")
	
	;; Given
	(define given-assertion-error
	  (make-assertion-error 'test "test-message" '()))

	;; When
	(define actual-returned-message
	  (assertion-error-message given-assertion-error))

	;; Then
	(assert-equal context "test-message" actual-returned-message)))))

(define assertion-error-context-test
  `("assertion-error-context test" .
    (,(lambda ()
	(define context
	  "should return the context of the assertion-error")
	
	;; Given
	(define given-assertion-error
	  (make-assertion-error 'test "test-message" '()))

	;; When
	(define actual-returned-context
	  (assertion-error-context given-assertion-error))

	;; Then
	(assert-equal context '() actual-returned-context)))))

(define assertion-error?-test
  `("assertion-error? test" .
    (,(lambda ()
	(define context "should return true because the input value is
of type assertion-error")

	;; Given
	(define given-assertion-error
	  (make-assertion-error 'test "test-message" '()))

	;; When
	(define actual-return-value
	  (assertion-error? given-assertion-error))

	;; Then
	(assert-equal context #t actual-return-value))
     ,(lambda ()
	(define context "should return false because the input value
isn't of type assertion-error")

	;; Given
	(define given-input
	  '(1 2 3))

	;; When
	(define actual-return-value
	  (assertion-error? given-input))

	;; Then
	(assert-equal context #f actual-return-value)))))


;;; execute tests

(let ((test-results
       (run-suites story-scenario-numbers-test
		   story-description-test
		   story-intention-test
		   story-who-test
		   story-purpose-test
		   story-scenario-test
		   scenario-actions-test
		   scenario-actual-outputs-test
		   scenario-assertions-test
		   scenario-description-test
		   scenario-given-inputs-test
		   scenario-initialization-logic-test
		   scenario-number-test
		   assert-that-test
		   assertion-error-type-test
		   assertion-error-message-test
		   assertion-error-context-test
		   assertion-error?-test
		   ;; verify-behavior-test
		   ;; behavior-run-hook-test
		   )))
  (format #t "  [OK     ] => ~a~%  [FAILED ] => ~a~%  [IGNORED] => ~a~%"
	  (car test-results) (cadr test-results) (caddr test-results))
  (and (> (cadr test-results) 0)
       (display '(exit 1))))
