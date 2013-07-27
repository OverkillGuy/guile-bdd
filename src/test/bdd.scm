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
	   ))

(define-syntax define-story
  (syntax-rules (in-order-to as-a i-want-to)
    "Defines a new user story. According to the definition from
Wikipedia a user story consists of \"one or more sentences in the
everyday or business language of the end user or user of a system that
captures what a user does or needs to do\" to get his work done. For
defining scenarios for a story please use the `scenario' macro."
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
