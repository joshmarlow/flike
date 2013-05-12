#lang racket

(require rackunit
         rackunit/text-ui
         "flike.rkt")

(define stack-manipulations
  (test-suite
    "Tests basic stack manipulation functionality"

    (test-case
      "Test inserting numbers on the stack"
      (let ((initial-stack '())
            (program '(5 6)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(5 6))))
    (test-case
      "Test DUP"
      (let ((initial-stack '())
            (program '(1 DUP)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(1 1))))
    (test-case
      "Test SWAP"
      (let ((initial-stack '())
            (program '(1 2 SWAP)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(2 1))))
    (test-case
      "Test DROP"
      (let ((initial-stack '())
            (program '(1 2 DROP)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(1))))
    (test-case
      "Test ROT"
      (let ((initial-stack '())
            (program '(0 1 2 3 ROT)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0 2 3 1))))))

(define integer-math-operations
  (test-suite
    "Test basic math operations"

    (test-case
      "Test +"
      (let ((initial-stack '())
            (program '(0 7 3 +)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0 10))))
    (test-case
      "Test -"
      (let ((initial-stack '())
            (program '(0 7 3 -)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0 4))))
    (test-case
      "Test *"
      (let ((initial-stack '())
            (program '(0 7 3 *)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0 21))))
    (test-case
      "Test /"
      (let ((initial-stack '())
            (program '(0 10 2 /)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0 5))))))

(define flow-control
  (test-suite
    "Test flow control operations"

    (test-case
      "Test JUMPIF"
      ;; NOTE: this deviates quite a bit from standard flike and
      ;; is much more assembly like.  The program should
      ;; consult the top of the stack, and if true, jump to
      ;; instruction IDX where IDX is the second value on the stack.
      (let ((initial-stack '(3 2 -1))
            (program '(JUMPIF DUP 5)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(3 5)
          "Testing JUMPIF program"))
      (let ((initial-stack '(3 2 0))
            (program '(JUMPIF DUP 5)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(3 3 5))))))

(define boolean-tests
  (test-suite
    "Test boolean operations"

    (test-case
      "Test =0 is true if the top of the stack is 0"
      (let ((initial-stack '(0))
            (program '(=0)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(-1)
          "0 =0"))
      (let ((initial-stack '(1))
            (program '(=0)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0)
          "1 =0")))

    (test-case
      "Test >0 is true if the top of the stack is greater than 0"
      (let ((initial-stack '(1))
            (program '(>0)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(-1)
          "1 >0"))
      (let ((initial-stack '(-1))
            (program '(>0)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0)
          "-1 >0")))

    (test-case
      "Test <0 is true if the top of the stack is less than 0"
      (let ((initial-stack '(1))
            (program '(<0)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0)
          "Testing 1 <0"))
      (let ((initial-stack '(-1))
            (program '(<0)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(-1)
          "Testing -1 <0")))

    (test-case
      "Test > is true if the first stack value is less than the second stack value"
      (let ((initial-stack '(0 1))
            (program '(>)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0)
          "Testing 0 1 >"))
      (let ((initial-stack '(1 0))
            (program '(>)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(-1)
          "Testing 1 0 >")))

    (test-case
      "Test < is true if the first stack value is greater than the second stack value"
      (let ((initial-stack '(0 1))
            (program '(<)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(-1)
          "Testing 0 1 <"))
      (let ((initial-stack '(1 0))
            (program '(<)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0)
          "Testing 1 0 <")))

    (test-case
      "Test AND"
      ;; Test 0 0 AND
      (let ((initial-stack '(0 0))
            (program '(AND)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0)
          "Testing 0 0 AND"))
      ;; Test 0 1 AND
      (let ((initial-stack '(0 -1))
            (program '(AND)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0)
          "Testing 0 -1 AND"))
      ;; Test 1 0 AND
      (let ((initial-stack '(-1 0))
            (program '(AND)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0)
          "Testing -1 0 AND"))
      ;; Test 1 1 AND
      (let ((initial-stack '(-1 -1))
            (program '(AND)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(-1)
          "Testing -1 -1 AND")))

    (test-case
      "Test OR"
      ;; Test 0 0 OR
      (let ((initial-stack '(0 0))
            (program '(OR)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0)
          "Testing 0 0 OR"))
      ;; Test 0 1 OR
      (let ((initial-stack '(0 -1))
            (program '(OR)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(-1)
          "Testing 0 1 OR"))
      ;; Test 1 0 OR
      (let ((initial-stack '(-1 0))
            (program '(OR)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(-1)
          "Testing 1 0 OR"))
      ;; Test 1 1 OR
      (let ((initial-stack '(-1 -1))
            (program '(OR)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(-1)
          "Testing 1 1 OR")))

    (test-case
      "Test INVERT."
      ;; Test INVERT is true (-1) if the first stack value is false(0).
      (let ((initial-stack '(0))
            (program '(INVERT)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(-1)))
      ;; Test INVERT is false (0) if the first stack value is true (-1).
      (let ((initial-stack '(-1))
            (program '(INVERT)))
        (check-equal?
          (flike-eval program initial-stack 10)
          '(0))))))

(define misc-tests
  (test-suite
    "Miscellaneous tests"

    (test-case
      "Test the program execution stops after the specified number of instructions"
      (let ((max-steps 4)
            (initial-stack '())
            (program '(1 2 3 4 DUP DUP DUP)))
        (check-equal?
          (flike-eval program initial-stack max-steps)
          '(1 2 3 4)
          "Max-steps not honored as expected")))))

(run-tests stack-manipulations 'verbose)
(run-tests integer-math-operations 'verbose)
(run-tests flow-control 'verbose)
(run-tests boolean-tests 'verbose)
(run-tests misc-tests 'verbose)
