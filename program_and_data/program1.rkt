#lang racket

(require racket/list)

; Function to read integers from a file and convert to a list
(define (read-integers filename)
  (call-with-input-file filename
    (lambda (in)
      (let ([line (read-line in 'any)])
        (map string->number (string-split line))))))

; Function to consolidate the list into value-frequency pairs using procedural hash
(define (consolidate lst)
  (define freq-hash (make-hash))
  (for-each (lambda (num)
              (hash-update! freq-hash num add1 0))
            lst)
  (hash->list freq-hash))

; Function to find the minimum and maximum values in a list
(define (min-max lst)
  (foldl (lambda (x acc)
           (let ([current-min (car acc)]
                 [current-max (cdr acc)])
             (cons (min x current-min) (max x current-max))))
         (cons (car lst) (car lst))
         lst))

; Function to perform counting sort without library sort function
(define (counting-sort lst)
  (let* ([min-max-pair (min-max lst)]
         [min-val (car min-max-pair)]
         [max-val (cdr min-max-pair)])
    (define range (+ 1 (- max-val min-val)))
    (define counts (make-vector range 0))
    
    ; Populate the counts vector
    (for-each (lambda (num)
                (vector-set! counts (- num min-val) (add1 (vector-ref counts (- num min-val)))))
              lst)
    
    ; Generate the sorted list
    (let loop ([i 0] [sorted '()])
      (if (= i range)
          (reverse sorted)
          (loop (add1 i)
                (append (make-list (vector-ref counts i) (+ i min-val)) sorted))))))

; Function to check if a list is sorted in ascending order
(define (is-sorted? lst)
  (or (null? lst)
      (null? (cdr lst))
      (and (<= (car lst) (cadr lst)) (is-sorted? (cdr lst)))))

; Function to test with multiple files
(define (test-multiple-files filenames)
  (for-each
   (lambda (filename)
     (let* ([integers (read-integers filename)]
            [sorted-list (counting-sort integers)]
            [sorted (is-sorted? sorted-list)])
       (unless (string=? filename "Data-7.txt")
         (displayln (string-append "File: " filename))
         (displayln sorted-list))
       (displayln (string-append "Sorted: " (if sorted "Yes" "No")))
       (newline)))
   filenames))

; Main function to test given text files
(define (main)
  (test-multiple-files '("Data-1.txt" "Data-2.txt" "Data-3.txt" "Data-4.txt" "Data-5.txt" "Data-6.txt" "Data-7.txt"))
  (is-sorted? '(1 3 2 4 5)))

; Call the main function
(main)
