#lang racket

(provide forth-eval)

(define (forth-eval program initial-stack)
  (define (exec-word word stack)
    (define (forth-true? val)
      (not (equal? val 0)))
    (define (false? val)
      (equal? val 0))
    (define (bool->forth-bool val)
      ;; convert a racket boolean to a forth boolean
      (if val -1 0))
    (cond
      ((integer? word) (cons word stack))
      ((equal? word 'INVERT)
       (cond
         ((false? (first stack)) (cons -1 (rest stack)))
         (#t (cons 0 (rest stack)))))
      ((equal? word 'OR)
       (cons
         (if (or (forth-true? (first stack))
                 (forth-true? (second stack)))
           -1
           0)
         (rest (rest stack))))
      ((equal? word 'AND)
       (cons
         (if (and (forth-true? (first stack))
                 (forth-true? (second stack)))
           -1
           0)
         (rest (rest stack))))
      ((equal? word 'JUMP)
       (initial-stack))
      ((equal? word '=0)
       (cons (bool->forth-bool (equal? (first stack) 0))
             (rest stack)))
      ((equal? word '<0)
       (cons (bool->forth-bool (< (first stack) 0))
             (rest stack)))
      ((equal? word '>0)
       (cons (bool->forth-bool (> (first stack) 0))
             (rest stack)))
      ((equal? word '<)
       (cons (bool->forth-bool (< (second stack) (first stack)))
             (rest (rest stack))))
      ((equal? word '>)
       (cons (bool->forth-bool (> (second stack) (first stack)))
             (rest (rest stack))))
      ((equal? word 'DUP) (cons (first stack) stack))
      ((equal? word 'SWAP)
       (append (list (second stack))
               (list (first stack))
               (rest (rest stack))))
      ((equal? word 'DROP) (rest stack))
      ((equal? word '+) (append
                          (list (+ (second stack) (first stack)))
                          (rest (rest stack))))
      ((equal? word '-) (append
                          (list (- (second stack) (first stack)))
                          (rest (rest stack))))
      ((equal? word '*) (append
                          (list (* (second stack) (first stack)))
                          (rest (rest stack))))
      ((equal? word '/) (append
                          (list (/ (second stack) (first stack)))
                          (rest (rest stack))))
      ((equal? word 'ROT)
       (append (list (third stack))
               (list (first stack))
               (list (second stack))
               (rest (rest (rest stack)))))
      (error (string-append "Unknown word " (symbol->string word)))))
  (define (forth-eval-helper  program instruction-idx initial-stack)
    (if (>= instruction-idx (length program))
      initial-stack
      (forth-eval-helper program (+ 1 instruction-idx)
                         (exec-word (list-ref program instruction-idx)
                                    initial-stack))))
  ;; NOTE: we reverse the stack so that we can use the traditional
  ;; FORTH notation of the the last element being the 'TOP' of the
  ;; stack.
  (reverse (forth-eval-helper program 0 (reverse initial-stack))))
