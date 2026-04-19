; CSI 2120 - Project Part 4 - Scheme (Functional)
; Gabriel Amorocho Goenaga (300404048) 
; Maximus Teo (300134556)
; ------------------------
; How to run: racket / (require "main.scm") / (function_name)
; Write output to file: (with-output-to-file "output.txt" (lambda () (display(time(gale-shapley RLIST PLIST '() #:exists 'replace)))))

#lang racket

(require "rpReader.scm")

(provide RLIST PLIST gale-shapley gale-shapley-print offer evaluate)

(provide get-resident-info get-program-info rank matched? get-match add-resident-to-match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Given functions

; if want to display csv content
;(read-residents "assets/residentSmall.csv")
;(read-programs "assets/programSmall.csv")

(define RLIST (read-residents "assets/residents4000.csv"))
(define PLIST (read-programs "assets/programs4000.csv"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper functions

(define (get-resident-info rid rlist)
	(cond
	 ((null? rlist) #f)	;not found
	 ((= rid (car (car rlist))) (car rlist))	;match found
	 (else (get-resident-info rid (cdr rlist)))))


(define (get-program-info pid plist)
	(cond
	 ((null? plist) #f)	;not found
	 ((string=? pid (car (car plist))) (car plist))	;match found
	 (else (get-program-info pid (cdr plist)))))


;Usage probably in the form (rank (rid (get-program-info pid)))
(define (rank rid pinfo)
	(define (find-index lst idx)
	 (cond
	  ((null? lst) #f) ; not found
	  ((= rid (car lst)) idx)
	  (else (find-index (cdr lst) (+ idx 1)))))
	(find-index (cadddr pinfo) 0))
	

;Usage example: (matched? 403 (gale-shapely RLIST PLIST '()))
(define (matched? rid matches)
	(cond
	 ((null? matches) #f)
	 ((let loop ((pairs (cadr (car matches))))
		(cond
		 ((null? pairs) #f)
		 ((= rid (car (car pairs))) #t)
		 (else (loop (cdr pairs))))) #t)
    (else (matched? rid (cdr matches)))))

(define (get-match pid matches)
	(cond
	 ((null? matches) #f)	;not found
	 ((string=? pid (car (car matches))) (car matches))	;match found
	 (else (get-match pid (cdr matches)))))


;adding a new resident to match. reverse order insertion according to p rol so that least preferred is always added first in the list
(define (add-resident-to-match pair match)
  (define (insert lst)
    (cond
      ((null? lst) (list pair))
      ((> (cdr pair) (cdr (car lst))) (cons pair lst))
      (else (cons (car lst) (insert (cdr lst))))))
  
  (list (car match)
        (insert (cadr match))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Printing functions

(define (get-not-matched-list rlist matches)
  (cond
    ((null? rlist) '())
    ((matched? (car (car rlist)) matches)
     (get-not-matched-list (cdr rlist) matches))
    (else
     (cons (car (car rlist))
           (get-not-matched-list (cdr rlist) matches)))))

(define (display-program-matches match rlist plist)
  (let* ((pid (car match))
         (pinfo (get-program-info pid plist))
         (pname (cadr pinfo)))

    (for-each
     (lambda (pair)
       (let* ((rid (car pair))
              (rinfo (get-resident-info rid rlist))
              (fname (cadr rinfo))
              (lname (caddr rinfo)))

         (display lname) (display ",")
         (display fname) (display ",")
         (display rid) (display ",")
         (display pid) (display ",")
         (display pname)
         (newline)))
     (cadr match))))


(define (display-not-matched not-matched-list rlist)
  (for-each
   (lambda (rid)
     (let* ((rinfo (get-resident-info rid rlist))
            (fname (cadr rinfo))
            (lname (caddr rinfo)))

       (display lname) (display ",")
       (display fname) (display ",")
       (display rid) (display ",")
       (display "XXX,NOT_MATCHED")
       (newline)))
   not-matched-list))

(define (get-total-available-positions matches plist)

  ;; total capacity
  (define (total-capacity plist)
    (if (null? plist)
        0
        (+ (caddr (car plist))
           (total-capacity (cdr plist)))))

  ;; total matched
  (define (total-matched matches)
    (if (null? matches)
        0
        (+ (length (cadr (car matches)))
           (total-matched (cdr matches)))))

  (- (total-capacity plist)
     (total-matched matches)))


(define (gale-shapley-print rlist plist)
    (let* ((matches (gale-shapley rlist plist '()))
        (not-matched-list (get-not-matched-list rlist matches)))
    (for-each (lambda(m)
        (display-program-matches m rlist plist)) matches)
    (display-not-matched not-matched-list rlist)
    (display "Number of unmatched residents: ")
        (display (length not-matched-list)) (newline)
    (display "Number of positions available: ")
    (display (get-total-available-positions matches plist))
    (newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main functions

(define (gale-shapley rlist plist matches)
  (let loop ((unprocessed rlist) (current-matches matches))
    (if (null? unprocessed)
        current-matches
        (loop (cdr unprocessed)
              (offer (car unprocessed) rlist plist current-matches)))))

(define (offer rinfo rlist plist matches)
    (let loop ((prefs (cadddr rinfo)))
        (cond
            ((null? prefs) matches)
        (else
            (let* ((pid (car prefs))
            (pinfo (get-program-info pid plist))
            (new-matches (evaluate rinfo pinfo rlist plist matches)))
                (if (equal? new-matches matches)
                    (loop (cdr prefs)) ; rejected → try next
                    new-matches))))))

(define (evaluate rinfo pinfo rlist plist matches)
  (let* ((pid (car pinfo))
         (rid (car rinfo))
         (r-rank (rank rid pinfo))
         (match (get-match pid matches))
         (quota (caddr pinfo)))

    (cond
      ;; resident not ranked by program -> reject
      ((not r-rank) matches)

      ;; no match yet → create new
      ((not match)
       (cons (list pid (list (cons rid r-rank))) matches))

      ;; space available
      ((< (length (cadr match)) quota)
       (cons (add-resident-to-match (cons rid r-rank) match)
             (remove match matches)))

      ;; full → compare worst
      (else
        (let* ((current (cadr match))
               (worst (car current))) ; worst is first
          (if (< r-rank (cdr worst)) ; better candidate
              (let* ((updated
                      (add-resident-to-match (cons rid r-rank)
                                             (list pid (cdr current)))))
                (offer (get-resident-info (car worst) rlist)
                       rlist
                       plist
                       (cons updated (remove match matches))))
              matches))))))

