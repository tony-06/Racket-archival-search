#lang racket

(define files ;creates list of files names from 001 to 025 (too lazy to write list myself)
  (map (lambda (x) (string-append (format "~a" (~r x #:min-width 3 #:pad-string "0")) ".txt"))
       (range 1 26)))

(define (hash-maker keys data) ;somebody once said "don't repeat yourself"
  (make-immutable-hash (map (lambda (x y) (cons x y)) keys data)))

(define (master-hash-maker filenames)
  (define (clean-file file) ;remove special characters and stop words
    (define (replace-chars string) ;helper function to replace non alphanumeric numbers
      (regexp-replace* #px"[^[:alpha:][:digit:][:space:]]" string "")) ;Stole this from chatgpt
    (define (remove-stop-words string-lst) ;helper function to remove stop words
      (remove* (string-split (port->string (open-input-file "stop_words_english.txt"))) string-lst))
    (remove-stop-words ; returns list of clean substrings
     (string-split (string-downcase(replace-chars (port->string (open-input-file file)))))))
  (define (make-string-hash string-lst) ;this makes my assignment 2 function look bloated
    (define count-words (map (lambda (x) (cons x (count (curry equal? x) string-lst))) string-lst))
    (define total-words (apply + (map cdr count-words)))
    (define frequencies
      (map (lambda (p) (cons (car p) (- 10(* -1 (log (/ (cdr p) total-words) 10))))) count-words))
    (make-immutable-hash frequencies))
  (define frequency-hash ;creates a list of word-frequency pairs
    (map (lambda (file) (make-string-hash (clean-file file))) filenames))
  (hash-maker files frequency-hash))

(define (search-master keywords master) ;search function for hash of hashes with a single search key
  (define (search-single hash keyword) ;single hashmap, single keyword
    (if (hash-has-key? hash keyword) (hash-ref hash keyword) #f))
  (define (search-multiple-keywords hash keywords) ;multiple keywords, single hashmap, returns score
    (define result (map (lambda (k) (search-single hash k)) keywords))
    (define (found-keywords count r)
      (foldr (lambda (r count) (if (not r) (- count 1) count)) count result))
    (* (found-keywords (length keywords) result) (apply +(filter number? result))))
  (define (search-all master keywords) ;multiple keywords, multiple hashmaps
    (hash-map master (lambda (k v) (cons k (search-multiple-keywords v keywords)))))
  (define (filter-sort-pairs pairs-lst) ;filter out any non-results and sort based on search score
    (filter (lambda (pair) (> (cdr pair) 0))(sort pairs-lst (lambda (p1 p2) (> (cdr p1) (cdr p2))))))
  (filter-sort-pairs (search-all master keywords)))

(define (search)
  (define (load-from-file filename) (with-input-from-file filename (lambda () (read))))
  (display "Enter search keywords: ")
  (define results (search-master (string-split (read-line)) (load-from-file "master-hash-file.txt")))
  (if (empty? results)
      (displayln "No results found for your search")
      (for-each
       (lambda (r) (and
                    (display (car r)) (display ": ")
                    (displayln (hash-ref (load-from-file "first-line-file.txt") (car r))))) results))
  (search))

(define (main) ;creating the master-hash-file is very slow, don't do it unless you have to
  (define (first-line-hash) ;hashed the first line of each file to make it return faster
    (define first-lines
      (map (lambda (file) (with-input-from-file file (lambda () (read-line)))) files))
    (hash-maker files first-lines))
  (define (save-to-file hash name) (with-output-to-file name (lambda () (write hash))))
  (cond
    [(and (file-exists? "master-hash-file.txt") (file-exists? "first-line-file.txt")) (search)]
    [(file-exists? "master-hash-file.txt")
     (and (save-to-file (first-line-hash) "first-line-file.txt")(search))]
    [(file-exists? "first-line-file.txt")
     (and (save-to-file (master-hash-maker files) "master-hash-file.txt") (search))]
    [else (and (save-to-file (first-line-hash) "first-line-file.txt")
               (save-to-file (master-hash-maker files) "master-hash-file.txt") (search))]))

(main) ;I think there is a lambda function in every definition. Thats lambda calculus, right?
