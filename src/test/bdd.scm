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

(define-module (test bdd)
  :export (
	   ;; story
	   define-story
	   story-description
	   story-purpose
	   story-who
	   story-intention
	   story-scenario
	   story-scenario-numbers

	   ;; scenario
	   scenario
	   scenario-number
	   scenario-description
	   scenario-given-inputs
	   scenario-initialization-logic
	   scenario-actual-outputs
	   scenario-actions
	   scenario-assertions
	   
	   ;; assertions
	   assert-that
	   make-assertion-error
	   assertion-error?
	   assertion-error-type
	   assertion-error-message
	   assertion-error-context

	   ;; test execution
	   verify-behavior

	   ;; hooks
	   behavior-add-hook
	   behavior-run-hook
	   ))

(define hooks (make-hash-table))

;; Following hooks are used by this module:
;;   `scenario-given-exception-thrown-hook'
;;   `scenario-when-exception-thrown-hook'
;;   `scenario-then-exception-thrown-hook'
;;   `scenario-before-hook'
;;   `scenario-after-hook'
;;   `scenario-given-before-hook'
;;   `scenario-when-before-hook'
;;   `scenario-then-before-hook'
;;   `scenario-given-after-hook'
;;   `scenario-when-after-hook'
;;   `scenario-then-after-hook'
;;   `scenario-assertion-failed-hook'
;;   `story-before-hook'
;;   `story-after-hook'

(define-syntax define-story
  (syntax-rules (in-order-to as-a i-want-to)
    "Defines a new user story. According to the definition from
Wikipedia a user story consists of \"one or more sentences in the
everyday or business language of the end user or user of a system that
captures what a user does or needs to do\" to get his work done. This
sentences are called scenarios here. For defining scenarios for a
story please use the `scenario' macro."
    ((_ name description
	(in-order-to purpose)
	(as-a who)
	(i-want-to intention)
	scenarios ...)
     (define name 
       `(story ,description ,purpose ,who ,intention
	       (,scenarios ...))))))

(define-syntax scenario
  (syntax-rules (given when then)
    "Defines a scenario with its preconditions (given), the
action (when) and the expected output (then) based on the action and
its preconditions. A scenario is part of a story (See: `define-story')."
    ((_ number description
	(given (input-values ...)
	  preconditions ...)
	(when (output-values ...)
	  actions ...)
	(then assertions ...))
     `(,number ,description
	       (given (input-values ...)
		 preconditions ...)
	       (when (output-values ...)
		 actions ...)
	       (then assertions ...)))))

(define (story-description story)
  "Accessor function for the description of a story."
  (cadr story))

(define (story-purpose story)
  "Accessor function for the purpose of a story."
  (caddr story))

(define (story-who story)
  "Accessor function for the stakeholder of a story."
  (cadddr story))

(define (story-intention story)
  "Accessor function for the intention of the stakeholder of a story."
  (cadddr (cdr story)))

(define (story-scenarios story)
  "Accessor functio for all scenarios defined in a story."
  (cadddr (cddr story)))

(define (story-scenario story scenario-num)
  "Searches a scenario with a specific number in a story. If it can't
be found '() is returned."
  (let scenario-by-number ((scenarios (story-scenarios story)))
    (cond
     ((null? scenarios) '())
     ((= (scenario-number (car scenarios)) scenario-num)
      (car scenarios))
     (else (scenario-by-number (cdr scenarios))))))

(define (story-scenario-numbers story)
  "Builds a list of the numbers of all scenarios in a story"
  (reverse
   (let next-number ((scenarios (story-scenarios story))
		     (scenario-numbers '()))
     (if (null? scenarios) scenario-numbers
	 (next-number (cdr scenarios)
		      (cons (scenario-number (car scenarios))
			    scenario-numbers))))))

(define (scenario-number scenario)
  "Accessor function for the number of a scenario."
  (car scenario))

(define (scenario-description scenario)
  "Accessor function for the description of a scenario."
  (cadr scenario))

(define (scenario-given-inputs scenario)
  "Accessor function for the given inputs of a scenario."
  (cadr (caddr scenario)))

(define (scenario-initialization-logic scenario)
  "Accessor function for the initialization logic of a scenario."
  (cddr (caddr scenario)))

(define (scenario-actual-outputs scenario)
  "Accessor function for the actual outputs of a scenario."
  (cadr (cadddr scenario)))

(define (scenario-actions scenario)
  "Accessor function for the actions performed in a scenario."
  (cddr (cadddr scenario)))

(define (scenario-assertions scenario)
  "Accessor function for the assertions made for a scenario."
  (cdr (cadr (cdddr scenario))))

(define-syntax assert-that
  (syntax-rules (is not null)
      "Verifies that a condition is met. `assert-that' should be used
to implement the assertions part of a `scenario'."
      ((_ actual is null)
       (assert-that* (not (null? actual))
       		     'invalid-assertion
       		     "~s should be null"
       		     actual))
      ((_ actual is not null)
       (assert-that* (null? actual)
       		     'invalid-assertion
       		     "~s shouldn't be null"
       		     actual))
      ((_ actual is expected)
       (assert-that* (not (equal? actual expected))
       		     'invalid-assertion
       		     "~s should be equal to ~s"
       		     actual expected))
      ((_ actual is not unexpected)
       (assert-that* (equal? actual unexpected)
       		     'invalid-assertion
       		     "~s shouldn't be equal to ~s"
       		     actual unexpected))
      ((_ actual comparator expected)
       (assert-that* (not (comparator actual expected))
		     'invalid-assertion
       		     "~s ~a ~s"
       		     actual comparator expected))))

(define-syntax assert-that*
  (syntax-rules ()
    ((_ condition type format-arguments format-arguments* ...)
     (if condition
	 (throw 'assertion-exception
		(make-assertion-error
		 type
		 (format #f format-arguments format-arguments* ...)
		 '()))))))

(define (make-assertion-error type message context)
  "Create a new assertion error."
  `(,type ,message ,context))

(define (assertion-error-type assertion-error)
  "Accessor function for the type of an `assertion-error'. The helps
to clarify the cause of the error."
  (car assertion-error))

(define (assertion-error-message assertion-error)
  "Accessor function for the message of an `assertion-error'. Each
`assertion-error' should come with a human readable error message."
  (cadr assertion-error))

(define (assertion-error-context assertion-error)
  "Acessor function for the context the `assertion-error' occurred
in."
  (caddr assertion-error))

(define (assertion-error? data)
  "Returns `#t' if `data' is an `assertion-error' or `#f' otherwise."
  (and (list? data)
       (= (length data) 3)
       (symbol? (car data))
       (string? (cadr data))))

(define (verify-behavior . stories)
  "Verifies the behavior of the implementation of the stories under
test. Returns either `#t' if the behavior of the implementation
complies with the stories or `#f' otherwise."
  #f)

(define (behavior-add-hook hook function)
  "Adds a function to `hook'. The behavior hooks could be used to do,
for example, initialization of the environment in which a behavior
should be verified." 
  #f)

(define (behavior-run-hook hook . arguments)
  "Runs all functions of a `hook' with the given `arguments'."
  #f)
