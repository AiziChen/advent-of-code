(define file->lines
  (lambda (file)
    (call-with-input-file file
      (lambda (p)
        (let loop ([n (get-line p)])
          (cond
           [(eof-object? n) '()]
           [else
            (cons n (loop (get-line p)))]))))))

(define part1
  (lambda (op ms)
    (cond
     [(null? (cdr ms)) 0]
     [(op (car ms) (cadr ms))
      (+ 1 (part1 op (cdr ms)))]
     [else
      (part1 op (cdr ms))])))

(define part2
  (lambda (op ms)
    (cond
     [(= (length ms) 3) 0]
     [(op (+ (car ms) (cadr ms) (list-ref ms 2))
         (+ (cadr ms) (list-ref ms 2) (list-ref ms 3)))
      (+ 1 (part2 op (cdr ms)))]
     [else
      (part2 op (cdr ms))])))


(let ([measurements (map string->number (file->lines "input"))])
  ;;; How many measurements are larger than the previous measurement?
  (printf "part1 answer: ~a~n" (part1 < measurements))
  ;; How many sums are larger than the previus sum?
  (printf "part2 answer: ~a~n" (part2 < measurements)))
