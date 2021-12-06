(load "input.scm")
(define boards-init
  (lambda (boards)
    (cond
     [(null? boards) '()]
     [else
      (cons
       (let loop ([board (car boards)])
         (cond
          [(null? board) '()]
          [else
           (cons
            (let loop2 ([ele (car board)])
              (cond
               [(null? ele) '()]
               [else
                (cons (cons (string->number (car ele)) #f)
                      (loop2 (cdr ele)))]))
                 (loop (cdr board)))]))
       (boards-init (cdr boards)))])))

(define iswin?
  (lambda (a-board)
    (or
     (let row-win? ([a-board a-board])
       (cond
        [(null? a-board) #f]
        [else
         (or (= (length (car a-board))
                (length (filter (lambda (row) (cdr row)) (car a-board))))
             (row-win? (cdr a-board)))]))
     (let colum-win? ([index 0])
       (cond
        [(= index (length a-board)) #f]
        [else
         (or (= (length a-board)
                (length (filter (lambda (col) (cdr (list-ref col index))) a-board)))
             (colum-win? (+ index 1)))])))))

(define find-and-mark!
  (lambda (boards n)
    (cond
     [(null? boards) '()]
     [else
      (for-each
       (lambda (row)
         (when (= n (car row))
           (set-cdr! row #t)))
       (car boards))
      (find-and-mark! (cdr boards) n)])))

(define sum-of-unmarked-numbers
  (lambda (board)
    (apply + (map
              (lambda (row)
                (apply + (map (lambda (ele) (if (cdr ele) 0 (car ele))) row)))
              board))))

(define rember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(equal? a (car lat)) (cdr lat)]
     [else
      (cons (car lat)
            (rember a (cdr lat)))])))

(let loop ([boards (boards-init *boards*)]
           [ns *random-numbers*])
  (cond
   [(or (null? ns) (null? boards)) '()]
   [else
    (for-each (lambda (board) (find-and-mark! board (car ns))) boards)
    (for-each
     (lambda (board)
       (when (iswin? board)
         (set! boards (rember board boards))
         (printf "result: ~a~n" (* (car ns) (sum-of-unmarked-numbers board)))))
     boards)
    (loop boards (cdr ns))]))
