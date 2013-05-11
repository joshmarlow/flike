#lang racket

(provide forth-eval)

(define (forth-true? val)
  (not (equal? val 0)))
(define (false? val)
  (equal? val 0))
(define (bool->forth-bool val)
  ;; convert a racket boolean to a forth boolean
  (if val -1 0))

(define (build-initial-dictionary)
  (let ((dictionary (make-hash)))
    (define (define-linear-operator name thunk)
      ;; Most of the initial operators are 'linear'; ie, they don't affect
      ;; the way the next instruction index is calculated.  Thus, they
      ;; fit into this little framework here.
      (hash-set! dictionary name
                 (lambda (instruction-idx initial-stack)
                   (list (+ instruction-idx 1) (thunk initial-stack)))))
    (define-linear-operator 'INVERT (lambda (stack)
                                      (cond
                                        ((false? (first stack)) (cons -1 (rest stack)))
                                        (#t (cons 0 (rest stack))))))
    (define-linear-operator 'OR (lambda (stack)
                                  (cons
                                    (if (or (forth-true? (first stack))
                                            (forth-true? (second stack)))
                                      -1
                                      0)
                                    (rest (rest stack)))))
    (define-linear-operator 'AND (lambda (stack)
                                   (cons
                                     (if (and (forth-true? (first stack))
                                              (forth-true? (second stack)))
                                       -1
                                       0)
                                     (rest (rest stack)))))
    (define-linear-operator '=0 (lambda (stack)
                                  (cons (bool->forth-bool (equal? (first stack) 0))
                                        (rest stack))))
    (define-linear-operator '<0 (lambda (stack)
                                  (cons (bool->forth-bool (< (first stack) 0))
                                        (rest stack))))
    (define-linear-operator '>0 (lambda (stack)
                                  (cons (bool->forth-bool (> (first stack) 0))
                                        (rest stack))))
    (define-linear-operator '< (lambda (stack)
                                 (cons (bool->forth-bool (< (second stack) (first stack)))
                                       (rest (rest stack)))))
    (define-linear-operator '> (lambda (stack)
                                 (cons (bool->forth-bool (> (second stack) (first stack)))
                                       (rest (rest stack)))))
    (define-linear-operator 'DUP (lambda (stack)
                                   (cons (first stack) stack)))
    (define-linear-operator 'SWAP (lambda (stack)
                                    (append (list (second stack))
                                            (list (first stack))
                                            (rest (rest stack)))))
    (define-linear-operator 'DROP (lambda (stack)
                                    (rest stack)))
    (define-linear-operator '+ (lambda (stack)
                                 (append
                                   (list (+ (second stack) (first stack)))
                                   (rest (rest stack)))))
    (define-linear-operator '- (lambda (stack)
                                 (append
                                   (list (- (second stack) (first stack)))
                                   (rest (rest stack)))))
    (define-linear-operator '* (lambda (stack)
                                 (append
                                   (list (* (second stack) (first stack)))
                                   (rest (rest stack)))))
    (define-linear-operator '/ (lambda (stack)
                                 (append
                                   (list (/ (second stack) (first stack)))
                                   (rest (rest stack)))))
    (define-linear-operator 'ROT (lambda (stack)
                                   (append (list (third stack))
                                           (list (first stack))
                                           (list (second stack))
                                           (rest (rest (rest stack))))))
    dictionary))

(define (forth-eval program initial-stack)

  (define dictionary (build-initial-dictionary))

  (define (forth-eval-helper program instruction-idx stack)
    ;; Recursively evaluate the program.  Each word sets
    ;; the next instruction, which opens the possibility to
    ;; loops and branches.
    (define (exec-word)
      ;; Helper function for executing a word
      (let ((input (list-ref program instruction-idx)))
        (cond
          ((hash-has-key? dictionary input)
           (let ((word (hash-ref dictionary input)))
             (word instruction-idx stack)))
          ((integer? input) (list (+ instruction-idx 1) (append (list input) stack)))
          (error (string-append "Unknown word " (symbol->string input))))))

    (if (>= instruction-idx (length program))
      stack
      (let* ((exec-output (exec-word))
             (new-instruction-idx (first exec-output))
             (new-stack (second exec-output)))
        (forth-eval-helper program new-instruction-idx new-stack))))

  ;; NOTE: we reverse the stack so that we can use the traditional
  ;; FORTH notation of the right-most element being the 'TOP' of the
  ;; stack.
  (reverse (forth-eval-helper program 0 (reverse initial-stack))))
