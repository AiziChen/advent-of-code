(load "input.scm")
(define make-diagram
  (lambda (w h)
    (cond
     [(= h 0) '()]
     [else
      (cons (make-list w 0)
            (make-diagram w (- h 1)))])))

(define 2more-point-count
  (lambda (dg)
    (cond
     [(null? dg) 0]
     [else
      (+ (apply + (map (lambda (n) (if (>= n 2) 1 0)) (car dg)))
         (2more-point-count (cdr dg)))])))

(define add1-in-pos!
  (lambda (dg pos)
    (let ([t (list-tail (list-ref dg (car pos)) (cdr pos))])
      (set-car! t (+ (car t) 1)))))

(define generate-y-positions
  (lambda (x s d)
    (let loop ([r (- s d)])
      (cond
       [(= r 0)
        (list (cons x (+ d r)))]
       [else
        (cons (cons x (+ d r))
              (loop (if (> r 0) (- r 1) (+ r 1))))]))))
(define generate-x-positions
  (lambda (y s d)
    (let loop ([r (- s d)])
      (cond
       [(= r 0)
        (list (cons (+ d r) y))]
       [else
        (cons (cons (+ d r) y)
              (loop (if (> r 0) (- r 1) (+ r 1))))]))))

(define generate-diagonal-positions
  (lambda (sx sy dx dy)
    (let loop ([rx (- sx dx)]
               [ry (- sy dy)])
      (cond
       [(or (= rx 0) (= ry 0))
        (list (cons dx dy))]
       [(and (> rx 0) (> ry 0))
        (cons (cons (+ dx rx) (+ dy ry))
              (loop (- rx 1) (- ry 1)))]
       [(and (> rx 0) (< ry 0))
        (cons (cons (+ dx rx) (+ dy ry))
              (loop (- rx 1) (+ ry 1)))]
       [(and (< rx 0) (> ry 0))
        (cons (cons (+ dx rx) (+ dy ry))
              (loop (+ rx 1) (- ry 1)))]
       [else
        (cons (cons (+ dx rx) (+ dy ry))
              (loop (+ rx 1) (+ ry 1)))]))))

(define part1-list-of-positions
  (lambda (pos)
    (let ([start (car pos)]
          [dest (cadr pos)])
      (cond
       [(and (= (car start) (car dest))
             (not (= (cdr start) (cdr dest))))
        (generate-y-positions (car start) (cdr start) (cdr dest))]
       [(and (not (= (car start) (car dest)))
             (= (cdr start) (cdr dest)))
        (generate-x-positions (cdr start) (car start) (car dest))]
       [else '()]))))

(define part2-list-of-positions
  (lambda (pos)
    (let ([start (car pos)]
          [dest (cadr pos)])
      (cond
       [(and (not (= (car start) (car dest)))
             (not (= (cdr start) (cdr dest))))
        (generate-diagonal-positions (car start) (cdr start) (car dest) (cdr dest))]
       [else '()]))))

(define generate
  (lambda (dg positions)
    (for-each
     (lambda (pos)
       (if dg (add1-in-pos! dg pos)))
     positions)))


(let ([vents *lines-of-vents*]
      [diagram (make-diagram 1000 1000)])
  ;; part1
  (for-each
   (lambda (vent)
     (generate diagram (part1-list-of-positions vent)))
   vents)
  (printf "part1 answer: ~a~n" (2more-point-count diagram))
  ;; part2
  (for-each
   (lambda (vent)
     (generate diagram (part2-list-of-positions vent)))
   vents)
  (printf "part2 answer: ~a~n" (2more-point-count diagram)))
