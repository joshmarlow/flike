#lang racket

(require rackunit
         rackunit/text-ui
         "flike.rkt")

(define stack-manipulations
  (test-suite
    "Tests basic stack manipulation functionality"

    (test-case
      "Test inserting numbers on the stack"
      (let* ((initial-stack '())
            (program '(5 6))
            (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(5 6))))
    (test-case
      "Test DUP"
      (let* ((initial-stack '())
            (program '(1 DUP))
            (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(1 1))))
    (test-case
      "Test SWAP"
      (let* ((initial-stack '())
            (program '(1 2 SWAP))
            (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(2 1))))
    (test-case
      "Test DROP"
      (let* ((initial-stack '())
            (program '(1 2 DROP))
            (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(1))))
    (test-case
      "Test ROT"
      (let* ((initial-stack '())
            (program '(0 1 2 3 ROT))
            (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0 2 3 1))))))

(define integer-math-operations
  (test-suite
    "Test basic math operations"

    (test-case
      "Test +"
      (let* ((initial-stack '())
            (program '(0 7 3 +))
            (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0 10))))
    (test-case
      "Test -"
      (let* ((initial-stack '())
            (program '(0 7 3 -))
            (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0 4))))
    (test-case
      "Test *"
      (let* ((initial-stack '())
            (program '(0 7 3 *))
            (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0 21))))
    (test-case
      "Test /"
      (let* ((initial-stack '())
            (program '(0 10 2 /))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
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
      (let* ((initial-stack '(3 2 -1))
            (program '(JUMPIF DUP 5))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(3 5)
          "Testing JUMPIF program"))
      (let* ((initial-stack '(3 2 0))
            (program '(JUMPIF DUP 5))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(3 3 5))))))

(define boolean-tests
  (test-suite
    "Test boolean operations"

    (test-case
      "Test =0 is true if the top of the stack is 0"
      (let* ((initial-stack '(0))
             (program '(=0))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(-1)
          "0 =0"))
      (let* ((initial-stack '(1))
             (program '(=0))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0)
          "1 =0")))

    (test-case
      "Test >0 is true if the top of the stack is greater than 0"
      (let* ((initial-stack '(1))
             (program '(>0))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(-1)
          "1 >0"))
      (let* ((initial-stack '(-1))
             (program '(>0))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0)
          "-1 >0")))

    (test-case
      "Test <0 is true if the top of the stack is less than 0"
      (let* ((initial-stack '(1))
             (program '(<0))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0)
          "Testing 1 <0"))
      (let ((initial-stack '(-1))
            (program '(<0)))
        (check-equal?
          (second (assoc 'param-stack (flike-eval program initial-stack 10)))
          '(-1)
          "Testing -1 <0")))

    (test-case
      "Test > is true if the first stack value is less than the second stack value"
      (let* ((initial-stack '(0 1))
             (program '(>))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0)
          "Testing 0 1 >"))
      (let* ((initial-stack '(1 0))
             (program '(>))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(-1)
          "Testing 1 0 >")))

    (test-case
      "Test < is true if the first stack value is greater than the second stack value"
      (let* ((initial-stack '(0 1))
             (program '(<))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(-1)
          "Testing 0 1 <"))
      (let* ((initial-stack '(1 0))
             (program '(<))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0)
          "Testing 1 0 <")))

    (test-case
      "Test AND"
      ;; Test 0 0 AND
      (let* ((initial-stack '(0 0))
             (program '(AND))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0)
          "Testing 0 0 AND"))
      ;; Test 0 1 AND
      (let* ((initial-stack '(0 -1))
             (program '(AND))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0)
          "Testing 0 -1 AND"))
      ;; Test 1 0 AND
      (let* ((initial-stack '(-1 0))
             (program '(AND))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0)
          "Testing -1 0 AND"))
      ;; Test 1 1 AND
      (let* ((initial-stack '(-1 -1))
             (program '(AND))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(-1)
          "Testing -1 -1 AND")))

    (test-case
      "Test OR"
      ;; Test 0 0 OR
      (let* ((initial-stack '(0 0))
             (program '(OR))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0)
          "Testing 0 0 OR"))
      ;; Test 0 1 OR
      (let* ((initial-stack '(0 -1))
             (program '(OR))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(-1)
          "Testing 0 1 OR"))
      ;; Test 1 0 OR
      (let* ((initial-stack '(-1 0))
             (program '(OR))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(-1)
          "Testing 1 0 OR"))
      ;; Test 1 1 OR
      (let* ((initial-stack '(-1 -1))
             (program '(OR))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(-1)
          "Testing 1 1 OR")))

    (test-case
      "Test INVERT."
      ;; Test INVERT is true (-1) if the first stack value is false(0).
      (let* ((initial-stack '(0))
             (program '(INVERT))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(-1)))
      ;; Test INVERT is false (0) if the first stack value is true (-1).
      (let* ((initial-stack '(-1))
             (program '(INVERT))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (second (assoc 'param-stack result))
          '(0))))))

(define computation-state
  (test-suite
    "Tests various bits of the computation state returned by flike-eval"

    (test-case
      "Test the program execution stops after the specified number of instructions"
      (let* ((max-steps 4)
             (initial-stack '())
             (program '(1 2 3 4 DUP DUP DUP))
             (result (flike-eval program initial-stack 4)))
        (check-equal?
          (assoc 'step-count result)
          '(step-count 4)
          "Max-steps not honored as expected")
        (check-equal?
          (assoc 'param-stack result)
          '(param-stack (1 2 3 4))
          "Param stack not constructed as expected")
        (check-equal?
          (assoc 'termination-type result)
          '(termination-type time-exceeded)
          "Termination type should be 'time-exceeded'")))

    (test-case
      "Test that a program that halts in the provided timeframe returns the 'halt' as the termination type"
      (let* ((max-steps 4)
             (initial-stack '())
             (program '(1 2 3 + *))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (assoc 'termination-type result)
          '(termination-type halt)
          "Termination type should be 'halt'")))

    (test-case
      "Test that a program that generates a divide by zero error halts with 'exception' termination type"
      (let* ((max-steps 4)
             (initial-stack '())
             (program '(3 0 /))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (assoc 'termination-type result)
          '(termination-type exception)
          "Termination type should be 'exception")))

    (test-case
      "Test that a program that generates a stack underflow error halts with 'exception' termination type"
      (let* ((max-steps 4)
             (initial-stack '())
             (program '(3 +))
             (result (flike-eval program initial-stack 10)))
        (check-equal?
          (assoc 'termination-type result)
          '(termination-type exception)
          "Termination type should be 'exception")))))


(run-tests stack-manipulations 'verbose)
(run-tests integer-math-operations 'verbose)
(run-tests flow-control 'verbose)
(run-tests boolean-tests 'verbose)
(run-tests computation-state 'verbose)
