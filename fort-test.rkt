#lang racket

(require rackunit
         rackunit/text-ui
         "forth.rkt")

(define stack-manipulations
  (test-suite
    "Tests basic stack manipulation functionality"

    (test-case
      "Test inserting numbers on the stack"
      (let ((initial-stack '())
            (program '(5 6)))
        (check-equal?
          (forth-eval program initial-stack)
          '(5 6))))
    (test-case
      "Test DUP"
      (let ((initial-stack '())
            (program '(1 DUP)))
        (check-equal?
          (forth-eval program initial-stack)
          '(1 1))))
    (test-case
      "Test SWAP"
      (let ((initial-stack '())
            (program '(1 2 SWAP)))
        (check-equal?
          (forth-eval program initial-stack)
          '(2 1))))
    (test-case
      "Test DROP"
      (let ((initial-stack '())
            (program '(1 2 DROP)))
        (check-equal?
          (forth-eval program initial-stack)
          '(1))))
    (test-case
      "Test ROT"
      (let ((initial-stack '())
            (program '(0 1 2 3 ROT)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0 2 3 1))))))

(define integer-math-operations
  (test-suite
    "Test basic math operations"

    (test-case
      "Test +"
      (let ((initial-stack '())
            (program '(0 7 3 +)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0 10))))
    (test-case
      "Test -"
      (let ((initial-stack '())
            (program '(0 7 3 -)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0 4))))
    (test-case
      "Test *"
      (let ((initial-stack '())
            (program '(0 7 3 *)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0 21))))
    (test-case
      "Test /"
      (let ((initial-stack '())
            (program '(0 10 2 /)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0 5))))))

(define flow-control
  (test-suite
    "Test flow control operations"

    (test-case
      "Test JUMP"
      ;; NOTE: this deviates quite a bit from standard forth and
      ;; is much more assembly like.  The program should
      ;; consult the top of the stack, and if true, jump to
      ;; instruction IDX where IDX is the second value on the stack.
      (let ((initial-stack '(2 -1))
            (program '(JUMP DUP 5)))
        (check-equal?
          (forth-eval program initial-stack)
          '(5)
          "Testing JUMP program")))))

(define boolean-tests
  (test-suite
    "Test boolean operations"

    (test-case
      "Test =0 is true if the top of the stack is 0"
      (let ((initial-stack '(0))
            (program '(=0)))
        (check-equal?
          (forth-eval program initial-stack)
          '(-1)
          "0 =0"))
      (let ((initial-stack '(1))
            (program '(=0)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0)
          "1 =0")))

    (test-case
      "Test >0 is true if the top of the stack is greater than 0"
      (let ((initial-stack '(1))
            (program '(>0)))
        (check-equal?
          (forth-eval program initial-stack)
          '(-1)
          "1 >0"))
      (let ((initial-stack '(-1))
            (program '(>0)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0)
          "-1 >0")))

    (test-case
      "Test <0 is true if the top of the stack is less than 0"
      (let ((initial-stack '(1))
            (program '(<0)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0)
          "Testing 1 <0"))
      (let ((initial-stack '(-1))
            (program '(<0)))
        (check-equal?
          (forth-eval program initial-stack)
          '(-1)
          "Testing -1 <0")))

    (test-case
      "Test > is true if the first stack value is less than the second stack value"
      (let ((initial-stack '(0 1))
            (program '(>)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0)
          "Testing 0 1 >"))
      (let ((initial-stack '(1 0))
            (program '(>)))
        (check-equal?
          (forth-eval program initial-stack)
          '(-1)
          "Testing 1 0 >")))

    (test-case
      "Test < is true if the first stack value is greater than the second stack value"
      (let ((initial-stack '(0 1))
            (program '(<)))
        (check-equal?
          (forth-eval program initial-stack)
          '(-1)
          "Testing 0 1 <"))
      (let ((initial-stack '(1 0))
            (program '(<)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0)
          "Testing 1 0 <")))

    (test-case
      "Test AND"
      ;; Test 0 0 AND
      (let ((initial-stack '(0 0))
            (program '(AND)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0)
          "Testing 0 0 AND"))
      ;; Test 0 1 AND
      (let ((initial-stack '(0 -1))
            (program '(AND)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0)
          "Testing 0 -1 AND"))
      ;; Test 1 0 AND
      (let ((initial-stack '(-1 0))
            (program '(AND)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0)
          "Testing -1 0 AND"))
      ;; Test 1 1 AND
      (let ((initial-stack '(-1 -1))
            (program '(AND)))
        (check-equal?
          (forth-eval program initial-stack)
          '(-1)
          "Testing -1 -1 AND")))

    (test-case
      "Test OR"
      ;; Test 0 0 OR
      (let ((initial-stack '(0 0))
            (program '(OR)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0)
          "Testing 0 0 OR"))
      ;; Test 0 1 OR
      (let ((initial-stack '(0 -1))
            (program '(OR)))
        (check-equal?
          (forth-eval program initial-stack)
          '(-1)
          "Testing 0 1 OR"))
      ;; Test 1 0 OR
      (let ((initial-stack '(-1 0))
            (program '(OR)))
        (check-equal?
          (forth-eval program initial-stack)
          '(-1)
          "Testing 1 0 OR"))
      ;; Test 1 1 OR
      (let ((initial-stack '(-1 -1))
            (program '(OR)))
        (check-equal?
          (forth-eval program initial-stack)
          '(-1)
          "Testing 1 1 OR")))

    (test-case
      "Test INVERT."
      ;; Test INVERT is true (-1) if the first stack value is false(0).
      (let ((initial-stack '(0))
            (program '(INVERT)))
        (check-equal?
          (forth-eval program initial-stack)
          '(-1)))
      ;; Test INVERT is false (0) if the first stack value is true (-1).
      (let ((initial-stack '(-1))
            (program '(INVERT)))
        (check-equal?
          (forth-eval program initial-stack)
          '(0))))))

(run-tests stack-manipulations 'verbose)
(run-tests integer-math-operations 'verbose)
(run-tests flow-control 'verbose)
(run-tests boolean-tests 'verbose)
