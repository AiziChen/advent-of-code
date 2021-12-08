(define read-line
  (lambda (file)
    (call-with-input-file file
      (lambda (p)
        (get-line p)))))

(define line->list
  (lambda (line)
    (let loop ([index 0])
      (cond
       [(>= index (string-length line)) '()]
       [else
        (cons (string->number (string (string-ref line index)))
              (loop (+ index 2)))]))))

(define grow
  (lambda (fishes n)
    (cond
     [(null? fishes) '()]
     [(= (car fishes) 0)
      (cons 6 (cons 8 (grow (cdr fishes) n)))]
     [else
      (cons (- (car fishes) n)
            (grow (cdr fishes) n))])))

(define part1
  (lambda (fishes days)
    (cond
     [(= days 0) (length fishes)]
     [else
      (let ([new-fishes (grow fishes 1)])
        (part1 new-fishes (- days 1)))])))

(let ([fishes (line->list (read-line "input"))])
  (printf "part1 answer: ~a~n" (part1 fishes 80)))
