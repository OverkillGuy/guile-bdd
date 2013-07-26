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

(use-modules (ice-9 format))

(define (assertion-error msg)
  (display msg)
  (newline) #f)

(define (assert-equal context expected actual)
  (if (not (equal? expected actual))
      (assertion-error
       (format #f "~a~%  The actual value is not equal to the expected value.
    [Actual  ] ~s => ~a
    [Expected] ~s => ~a" context actual actual expected expected)) #t))

(define (ignore test-case)
  (lambda () 0))

(define (run-suite suite)
  (format #t "RUN SUITE: ~a~%" (car suite))
  (map (lambda (test-case)
	 (test-case)) (cdr suite)))

(define (run-suites . suites)
  (let ((result (map count-results (map run-suite suites))))
    (display result) (newline) result))

(define (count-results results)
  (let count-next-item ((remaining-results results) (result '(0 0 0)))
    (if (null? remaining-results)
	result
	(count-next-item
	 (cdr remaining-results)
	 (case (car remaining-results)
	   ((#t) (cons (1+ (car result)) (cdr result)))
	   ((#f) (list (car result) (1+ (cadr result)) (cddr result)))
	   ((0) (list (car result) (cadr result) (1+ (caddr result)))))))))
