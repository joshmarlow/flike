#lang racket

(provide update-alist)
(provide get-computation-component)

(define (update-alist alist key value)
  (map
    (lambda (pair)
      (if (equal? (first pair) key)
        (list key value)
        pair))
    alist))

(define (get-computation-component component computation)
  (let ((component-pair (assoc component computation)))
    (if (eq? component-pair #f)
      (error (string-append "Cannot get missing component "
                            (symbol->string component)
                            " from compoutation"))
      (second component-pair))))
