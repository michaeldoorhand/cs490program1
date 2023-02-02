#lang racket
(require racket/port) 
; standard Racket. We're going to be doing some I/O other than reading from 
; keyboard or printing to screen. 


; Given: A text file containing an unknown number of student names (first, last pairs)
; with 3 exam scores for each, print a list sorted by last names of
; first   last    average

; convert each line (a string) to list of strings, breaking on whitespace
(define (line-to-list stringlist)
   ; Given a list and a string, split the string into a list of substrings. 
   ; append this list onto the list that's passed in, returning the updated 
   ; list  
   (define (one-string string outlist)
    (define tmp (list (string-split string)))
    (append outlist tmp))
	; iterate through the list of strings, 1 at a time. Tail recursive, of course. 
  (define (iter strings outlist)
    (if (empty? strings) outlist
        (iter (rest strings) (one-string (first strings) outlist))))
	; main function passes all of our data and an empty list to start the process 
  (iter stringlist '()))
  
; sort by last name (2nd item), breaking ties on first name (first item),
; bringing everything else along for the ride

; for convenience, functions to pull out first name, and last name, and 
; sorts-as name, consisting of "lastname firstname"
(define (lastname stringlist)
    (first (rest stringlist)))
(define (firstname stringlist)
    (first stringlist))
(define (sortname stringlist)
  (string-append (lastname stringlist) " " (firstname stringlist)))

; which of 2 strings is smaller? Allow (require) user to pass in the 
; comparison function to be used. So "smaller" means "comes earlier based
; on whatever the passed-in function does." 
(define (min-str str1 str2 lt-func)
  (if (lt-func str1 str2) str1 str2))

; given a list of names, and a function to pull the sort key from the data, 
; find the smallest (earliest) name in the list. string-ci<? is a library 
; function--string comparison, case insensitive, less-than comparison. 
(define (min-name lst sel)
  (define (iter lst m)
    (cond
      [(empty? lst) m]
      [else  (iter (rest lst) (min-str m (sel (first lst)) string-ci<?))]))
  (iter lst "zzzzzzzzzzzzzzzzz"))


; selection sort. (Yes, just as inefficient as any other selection sort, but 
; this is a small data set. If list is empty or 1 item, return it. Otherwise 
; find earliest name; filter out all records with that name (in the general case, 
; there may be duplicates), append it to the sorted list of everything with a 
; name larger than the minimum. Note that this is NOT tail-recursive and should 
; not be used for large lists. 
(define (sort-name lst)
  (if (< (length lst) 2)
      lst
      (let
          ([m (min-name lst sortname)])
          (append (filter (lambda (x) (string-ci=? (sortname x) m)) lst)
                  (sort-name (filter (lambda (x) (string-ci<? m (sortname x))) lst))))))


; given a list of strings representing numbers, find the numeric average. 
; exact->inexact forces the answer to floating-point. This tail-recursive function
; iterates through a list of our student records--pull out first and last names, 
; pass the rest of the list down to be averaged. 
(define (compute-avgs lst)
  (define (avg-score lst)
    (let ([nums (map string->number lst)])
      (apply + (map (lambda (x) (* x .35)) (list (first nums) (second nums) (third nums) (fourth nums) (fifth nums))))))
      ;(* (exact->inexact (/ (+ (first nums) (second nums) (third nums) (fourth nums) (fifth nums)) 5)) .35 )
      ;(* (exact->inexact (/ (+ (sixth nums) (seventh nums) (eighth nums)) 3)) .65 )))
      
  
  (define (avg-scores lst)
    (let ([nums (map string->number lst)])
      (exact->inexact (/ (apply + nums) (length nums)))))
  (if (empty? lst)
      empty
      (let
          ([m (first lst)]
          [n (first (rest lst))])
          (list m n (avg-score (rest (rest lst)))))))


; output preparation. Tail recursive. Take the first list-of-strings, 
; build the first name, last name, convert the average to a string, finish 
; with newline. Add the 'line' string to our growing output string, iterate to 
; next line. After last line, our output is in one big string. 
(define (prep-for-file lst)
  (define (iter lst so-far)
    (if (empty? lst)
        so-far
        (letrec (
                 [line (first lst)]
                 [outline (string-append (cadr line) " " (car line) " " (number->string (caddr line)) "\n")])
          (iter (rest lst) (string-append so-far outline)))))
  (iter lst ""))


;  MAIN PROGRAM

; this (probably over-complicated) line opens the file in text mode, reading 
; the input as one big string; then splits the string on newlines; then converts 
; our list-of-strings to a list of lists, each sublist being a list of strings: 
; ("first" "last" "score1" "score2" "score3")
(define workinglist (line-to-list (string-split (first (port->lines  
   (open-input-file "input1.txt"
                       #:mode 'text) #:line-mode 'return)) "\n")))

; compute averages, sort by name, save that list. 
(define printoutlist (sort-name (map compute-avgs workinglist)))

; open output file, text mode, replacing old file if one already exists. 
(define out (open-output-file "output1.txt" #:mode 'text #:exists 'replace))
; send to output
(display (prep-for-file printoutlist) out)
; close file. 
(close-output-port out)