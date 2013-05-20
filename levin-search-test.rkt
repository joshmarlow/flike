#lang racket

(require rackunit
         rackunit/text-ui
         "utils.rkt"
         "flike.rkt"
         "levin-search.rkt")

(define levin-search-component-tests
  (test-suite
    "Test building an components of levin search"

    (test-case
      "Test creating an initial pool of programs"
      (let ((dictionary (build-initial-dictionary)))
        (check-equal? (apply set (build-initial-pool dictionary))
                      (apply set '(((primary-segment (INVERT)))
                                   ((primary-segment (OR)))
                                   ((primary-segment (AND)))
                                   ((primary-segment (=0)))
                                   ((primary-segment (<0)))
                                   ((primary-segment (>0)))
                                   ((primary-segment (<)))
                                   ((primary-segment (>)))
                                   ((primary-segment (DUP)))
                                   ((primary-segment(SWAP)))
                                   ((primary-segment (DROP)))
                                   ((primary-segment (ROT)))
                                   ((primary-segment (JUMPIF)))
                                   ((primary-segment (+)))
                                   ((primary-segment (-)))
                                   ((primary-segment (*)))
                                   ((primary-segment (/))))))))

    (test-case
      "Test expanding an existing pool of programs"
      (let ((pool '(((primary-segment (5)))
                    ((primary-segment (DUP)))))
            (operator-set '(SWAP INVERT)))
        (check-equal? (apply set (expand-pool pool operator-set))
                      (apply set '(((primary-segment (5 SWAP)))
                                   ((primary-segment (5 INVERT)))
                                   ((primary-segment (DUP SWAP)))
                                   ((primary-segment (DUP INVERT)))))))

      (test-case
        "Test contracting an existing pool of programs"
        (let ((pool '(((primary-segment (5 +))
                       (termination-type exception))
                      ((primary-segment (5 DUP))
                       (termination-type halt))
                      ((primary-segment (1 2 3 4 + + +))
                       (termination-type time-exceeded)))))
          (check-equal? (apply set (contract-pool pool))
                        (apply set '(((primary-segment (5 DUP))
                                      (termination-type halt))))
                        "Pool not contracted as expected"))))))

(define levin-search-tests
  (test-suite
    "Test levin search with various scenarios"

    (test-case
      "Basic test"
      (define dictionary (build-initial-dictionary))
      (define initial-stack '(1))
      (define (criteria-predicate computation)
        (let* ((output-stack (second (assoc 'param-stack computation))))
          (if (> (length output-stack) 0)
            (equal? (first output-stack) 3)
            #f)))

      (let* ((max-clock-time (* 60 20))
             (computation (levin-search dictionary initial-stack max-clock-time criteria-predicate))
             (primary-segment (get-computation-component 'primary-segment computation)))
        (check-equal? (get-computation-component 'primary-segment computation)
                      '(DUP DUP + +))))))

(run-tests levin-search-component-tests 'verbose)
(run-tests levin-search-tests 'verbose)
