#lang racket

(require "flike.rkt")
(require "utils.rkt")

(provide build-initial-pool)
(provide expand-pool)
(provide contract-pool)
(provide levin-search)

;;(define rseed 6130)
;;(display (string-append "Random seed: " (number->string rseed) "\n"))
;;(random-seed rseed)

(define (build-initial-pool dictionary)
  (for/list ((word-name (hash-keys dictionary)))
            (list (list 'primary-segment (list word-name)))))

(define (expand-pool pool operator-set) 
  (for*/list ((computation pool) (op operator-set))
             (update-alist computation 'primary-segment
                           (append 
                             (get-computation-component 'primary-segment computation)
                             (list op)))))

(define (contract-pool pool)
  ;; Weed out any computation from the pool that had an exception
  (filter (lambda (computation)
            (equal? (get-computation-component 'termination-type computation)
                         'halt))
          pool))

;;(define (levin-search dictionary max-clock-time criteria-predicate) '((primary-segment (DUP DUP DUP DUP + + + +))))
(define (levin-search dictionary initial-stack max-clock-time criteria-predicate)
  (define initial-pool (build-initial-pool dictionary))
  (define start-time (current-seconds))
  (define stop-time (+ start-time max-clock-time))

  (define (levin-search-helper pool max-steps)
    (let* ((computations (map (lambda (computation)
                                (flike-eval (get-computation-component 'primary-segment computation)
                                            initial-stack
                                            max-steps))
                              pool))
           (matches (filter criteria-predicate computations)))
      (if (> (length matches) 0)
        (first matches)
        (if (> (current-seconds) stop-time)
          'nil
          (levin-search-helper (expand-pool (contract-pool computations) (hash-keys dictionary))
                               (+ max-steps 1))))))

  (levin-search-helper initial-pool 10))
