(define file->lines
  (lambda (file)
    (call-with-input-file file
      (lambda (p)
        (let loop ([n (get-line p)])
          (cond
           [(eof-object? n) '()]
           [else
            (cons n (loop (get-line p)))]))))))

(define calc
  (lambda (op ms)
    (cond
     [(null? (cdr ms)) 0]
     [(op (car ms) (cadr ms))
      (+ 1 (calc op (cdr ms)))]
     [else
      (calc op (cdr ms))])))

;;; How many measurements are larger than the previous measurement?
(let ([measurements (map string->number (file->lines "input"))])
  (printf "answer: ~a~n" (calc < measurements)))
