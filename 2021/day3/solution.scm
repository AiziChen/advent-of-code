(define file->lines
  (lambda (file)
    (call-with-input-file file
      (lambda (p)
        (let loop ([n (get-line p)])
          (cond
           [(eof-object? n) '()]
           [else
            (cons n (loop (get-line p)))]))))))

(define process
  (lambda (op ls)
    (let loop ([ls ls]
               [0n 0]
               [1n 0])
      (cond
       [(null? ls)
        (if (= 0n 1n)
            (if (equal? op >) "1" "0")
            (if (op 0n 1n) "0" "1"))]
       [(equal? (car ls) #\1)
        (loop (cdr ls) 0n (+ 1n 1))]
       [(equal? (car ls) #\0)
        (loop (cdr ls) (+ 0n 1) 1n)]))))

(define part1
  (lambda (op bs)
    (let loop ([index 0])
      (cond
       [(= (string-length (car bs)) index) ""]
       [else
        (let ([ls (map
                   (lambda (bin)
                     (string-ref bin index))
                   bs)])
          (string-append (process op ls) (loop (+ index 1))))]))))

(define part2
  (lambda (op bs)
    (let loop ([bs bs] [index 0])
      (cond
       [(= 1 (length bs)) (car bs)]
       [(and (= 2 (length bs))
             (not (equal? (string-ref (car bs) index)
                          (string-ref (cadr bs) index))))
        (if (and (equal? op >) (equal? (string-ref (car bs) index) #\1))
            (car bs)
            (cadr bs))]
       [(and (= 2 (length bs))
             (equal? (car bs) (cadr bs)))
        (car bs)]
       [else
        (let* ([ls (map
                    (lambda (bin)
                      (string-ref bin index))
                    bs)]
               [sn (process op ls)])
          (loop (filter
                 (lambda (bin)
                   (equal? (string (string-ref bin index)) sn))
                 bs)
                (+ index 1)))]))))


(let ([binary-numbers (file->lines "input")])
  (let ([gamma-rate (string->number (part1 > binary-numbers) 2)]
        [epsilon-rate (string->number (part1 < binary-numbers) 2)]
        [oxygen-rate (string->number (part2 > binary-numbers) 2)]
        [CO2-rate (string->number (part2 < binary-numbers) 2)])
    (printf "part1 answer: ~a~n" (* gamma-rate epsilon-rate))
    (printf "part2 answer: ~a ~n" (* oxygen-rate CO2-rate))))


