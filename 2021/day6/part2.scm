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

(define grow!
  (lambda (pfishes days)
    (cond
     [(= days 0) '()]
     [else
      (let loop ([index 0])
        (cond
         [(= index (length pfishes)) '()]
         [(= (car (list-ref pfishes index)) 0)
          (let ([pfish (list-ref pfishes index)])
            ;; add new fishes
            (let ([fpfish (list-ref pfishes (- (length pfishes) 1))])
              (set-cdr! fpfish (+ (cdr fpfish) (cdr pfish))))
            (let ([ppfish (list-ref pfishes (- (length pfishes) 3))])
              (set-cdr! ppfish (+ (cdr ppfish) (cdr pfish)))
              (set-cdr! pfish 0)))
          (loop (+ index 1))]
         [else
          (let ([pfish (list-ref pfishes index)]
                [ppfish (list-ref pfishes (- index 1))])
            (set-cdr! ppfish (+ (cdr pfish) (cdr ppfish)))
            (set-cdr! pfish 0))
          (loop (+ index 1))]))
      (grow! pfishes (- days 1))])))

(define total-fish
  (lambda (pfishes)
    (cond
     [(null? pfishes) 0]
     [else
      (+ (cdar pfishes)
         (total-fish (cdr pfishes)))])))


(let ([period-fishes '((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0) (7 . 0) (8 . 0) (-1 . 0))]
      [fishes (line->list (read-line "input"))])
  ;; period fishes init
  (for-each
   (lambda (fish)
     (let ([pfish (list-ref period-fishes fish)])
       (set-cdr! pfish (+ (cdr pfish) 1))))
   fishes)
  ;; fishes growing
  (grow! period-fishes 256)
  (printf "part2 answer: ~a~n" (total-fish period-fishes)))
