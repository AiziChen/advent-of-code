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
  (lambda (is)
    (let loop ([is is]
               [position 0]
               [depth 0])
      (cond
       [(null? is) (* position depth)]
       [(equal? "forward" (caar is))
        (loop (cdr is) (+ position (cdar is)) depth)]
       [(equal? "down" (caar is))
        (loop (cdr is) position (+ depth (cdar is)))]
       [(equal? "up" (caar is))
        (loop (cdr is) position (- depth (cdar is)))]
       [else
        (loop (cdr is) position depth)]))))

(define part2
  (lambda (is)
    (let loop ([is is]
               [position 0]
               [aim 0]
               [depth 0])
      (cond
       [(null? is) (* position depth)]
       [(equal? "forward" (caar is))
        (loop (cdr is) (+ (cdar is) position) aim (+ depth (* (cdar is) aim)))]
       [(equal? "down" (caar is))
        (loop (cdr is) position (+ aim (cdar is)) depth)]
       [(equal? "up" (caar is))
        (loop (cdr is) position (- aim (cdar is)) depth)]
       [else
        (loop (cdr is) depth aim)]))))

(let ([instructions
       (map (lambda (line)
              (let ([len (string-length line)])
                (cons (substring line 0 (- len 2))
                      (string->number (substring line (- len 1) len)))))
            (file->lines "input"))])
  (printf "part1 answer: ~a~n" (part1 instructions))
  (printf "part2 answer: ~a~n" (part2 instructions)))
