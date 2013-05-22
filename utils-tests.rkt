#lang racket

(require rackunit
         rackunit/text-ui
         "utils.rkt")

(define update-alist-tests
  (test-suite
    "Tests for the update-alist function"

    (test-case
      "Test that we can replace the specified assoc pair"
      (check-equal?
        (update-alist '((car ford) (fruit apple) (dog spike) (cat trouble))
                      'fruit 'grape)
        '((car ford) (fruit grape) (dog spike) (cat trouble))))))

(define get-computation-component-tests
  (test-suite
    "Tests for the get-computation-component function"

    (test-case
      "Tests that we can get a component when it is there"
      (check-equal?
        (get-computation-component 'primary-segment '((primary-segment (DUP))))
        '(DUP)
        "Cannot get specified component"))

    (test-case
      "Tests that we get an exception when the component is not there"
      (with-handlers ([exn:fail? (lambda (exn) #t)])
        (get-computation-component 'termination-type '((primary-segment (DUP))))
        (check-true #f "get-computation-component should have thrown an exception when accessing a missing component")))))

(run-tests update-alist-tests 'verbose)
(run-tests get-computation-component-tests 'verbose)
