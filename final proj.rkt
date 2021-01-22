#lang racket
(require csc151)
(require plot)
(require racket/gui/base)
(require bestfit)

(define source (open-input-file "us_states_covid19_daily.csv"))

;;; Procedure
;;; plot-total-pos
;;; Purpose
;;; create a graph showing how many people have been tested and the number of positive results
;;; Parameters
;;; state, a string and date, a string
;;; Produces
;;; graph with  columns, one being number of people tested and one being positive tests
;;; Preconditions
;;; must have "us_states_covid19_daily.csv" as an input file that will open when (open-input-file "us_states_covid19_daily.csv") is called
;;; state must be a string of the 2 letter abreviate, ex "IA"
;;; date must be a string in the form "YEARMODAY" where year is the year, month is the 2 letter abbreviation of the month and day is the day, ex "20200419" is april 19 2020
;;; Postconditions
;;; only displays data which is available in the state on the given day
;;; the left column displays how many people had been tested in the state up to the given day
;;; the right column displays the number of positive results up to that day
;;; the date will be displayed at the bottom, and population will be the y axis

(define plot-total-pos
  (lambda (state date)
    (let kernel ([source (open-input-file "us_states_covid19_daily.csv")]
                 [line-lst (string-split (read-line source) ",")])
      (cond
        [(eof-object? (peek-char source)) ;;checks to see if there is data on the given day
         (display "No data found. Please enter a different date.")
         (newline)
         (close-input-port source)]
        [(and (string-ci=? state (cadr line-lst))
              (equal? date (car line-lst))) ;;checks to see if date and state match
         (plot (discrete-histogram
                (list (list "Total Tested" (string->number (list-ref line-lst 17))) ;;adds total tests as the left column
                      (list "Positive Results" (string->number (caddr line-lst))))) ;;adds total positive results as the right column
                #:x-label #f
                #:y-label "Population")]
        [else
         (kernel source (string-split (read-line source) ","))]))))

;;; Procedure
;;; my-string->number
;;; Purpose
;;; convert a string to a number
;;; Parameters
;;; str, a string
;;; Produces
;;; num, a number
;;; Preconditions
;;; none
;;; Postconditions
;;; if str is "", produces 0
;;; otherwise produces str as a number

(define my-string->number
  (lambda (str)
    (if (equal? str "")
        0
        (string->number str))))

;;; Procedure
;;; plot-hospitalized
;;; Purpose
;;; create a stacked histogram of data in the state on the given date
;;; Parameters
;;; state, a string and date, a string
;;; Produces
;;; a stacked histogram
;;; Preconditions
;;; state must be a string of the 2 letter abreviate, ex "IA"
;;; date must be a string in the form "YEARMODAY" where year is the year, month is the 2 letter abbreviation of the month and day is the day, ex "20200419" is april 19 2020
;;; must have "us_states_covid19_daily.csv" as an input file that will open when (open-input-file "us_states_covid19_daily.csv") is called
;;; Postconditions
;;; only displays data which is available in the state on the given day
;;; at the bottom of the stacked histogram in blue is hospitalized
;;; on top of hospitalized in black is death
;;; on top of death in green is recovered
;;; the date will be displayed at the bottom, and population will be the y axis

(define plot-hospitalized-death-recovered
  (lambda (state date)
    (let kernel ([source (open-input-file "us_states_covid19_daily.csv")]
                 [line-lst (string-split (read-line source) ",")])
      (cond
        [(eof-object? (peek-char source)) 
         (display "No data found. Please enter a different date.")
         (newline)
         (close-input-port source)]
        [(and (string-ci=? state (cadr line-lst)) ;;checks to see if date and state equal entered date and state
              (equal? date (car line-lst)))
         (plot (stacked-histogram
                (list (list date (list (my-string->number (list-ref line-lst 6)) ;;extracts hospitalized
                                       (my-string->number (list-ref line-lst 14)) ;;extracts death
                                       (my-string->number (list-ref line-lst 11))))) ;;extracts recovered
                #:labels (list "Hospitalized" "Death" "Recovered")
                #:colors (list "blue" "black" "green"))
                #:x-label "Date"
                #:y-label "Population")]
        [else
         (kernel source (string-split (read-line source) ","))]))))

;;; Procedure
;;; state-all-info
;;; Purpose
;;; to extract all information about a state in the csv file
;;; Parameters
;;; str, a string
;;; Produces
;;; lst, a list
;;; Preconditions
;;; must have "us_states_covid19_daily.csv" automatically open as a csv file
;;; str must be the 2 digit state abbreviation
;;; Postconditions
;;; will return a table of all data about the state
;;; each list inside the table will contain all data from the row, and if a column doesn't have data returns "" as that element
;;; returns data from oldest to newest

(define state-all-info
  (lambda (state)
    (let kernel ([source (open-input-file "us_states_covid19_daily.csv")]
                 [line-lst (string-split (read-line source) ",")]
                 [info-so-far null])
      (cond
        [(eof-object? (peek-char source))
         info-so-far]
        [(string-ci=? state (cadr line-lst))
         (kernel source (string-split (read-line source) ",") (cons line-lst info-so-far))]
        [else
         (kernel source (string-split (read-line source) ",") info-so-far)]))))

(define day-of-month (list 31 29 31 30 31 30 31 31 30 31 30 31))

;;; Procedure
;;; date-relative-to-0101
;;; Purpose
;;; find the date relative to another date
;;; Parameters
;;; str, a string
;;; Produces
;;; num, a number
;;; Preconditions
;;; str must be in the form "YEARMODA" year where year is 2020, MO is the month (ex april is 04) and DA is the day (ex 04 is the 4th day of the month)
;;; Postconditions
;;; num will be an inexact integer
;;; num will be the number of days since january 1st (not including january first)

(define date-relative-to-0101
  (lambda (str)
    (let ([month (string->number (substring str 4 6))]
          [day (string->number (substring str 6 8))])
      (exact->inexact
       (+ (apply + (take day-of-month (- month 1)))
          (- day 1))))))

;;; Procedure
;;; state-linear-fit
;;; Purpose
;;; predict the number of positive confirmed cases on the specific date
;;; Parameters
;;; state, a string, date, a string
;;; Produces
;;; displays a number in the interactions window
;;; Preconditions
;;; state is the 2 letter abbreviation for the state
;;; date must be in the form "YEARMODA" year where year is 2020, MO is the month (ex april is 04) and DA is the day (ex 04 is the 4th day of the month)
;;; Postconditions
;;; displays an inexact number in the interactions window
;;; predicts using a linear fit how many cumulative confirmed positive cases there will be in the state on the given date

(define state-linear-fit
  (lambda (state date)
    (let* ([x-val-raw (map (o date-relative-to-0101 car) (state-all-info state))]
           [first-day (car x-val-raw)]
           [x-val (map (section - <> (- first-day 1)) x-val-raw)]
           [y-val (map (o exact->inexact string->number caddr) (state-all-info state))]
           [line (linear-fit x-val y-val)]
           [date-relative (date-relative-to-0101 date)])
      (line (- date-relative first-day)))))

(plot-new-window? #t)

(define frame (new frame% [label "Coronavirus in the USA"]
                   [width 700]
                   [height 700]
                   [stretchable-width #t]
                   [stretchable-height #t]))

(define state (new text-field%
                   [label "State (two-letter abbreviation)"]
                   [parent frame]))

(define date-month (new choice%
                        [label "Month"]
                        [parent frame]
                        [choices (list "01" "02" "03" "04" "05")]))

(define date-day (new choice%
                      [label "Day"]
                      [parent frame]
                      [choices (map number->string (cdr (iota 32)))]))

(define graph-choice (new radio-box%
                          (label "Graph")
                          (parent frame)
                          (choices (list "discrete histogram of total tested and positive results"
                                         "stacked histogram of hospitalized, death and recovered"))))
;;; Procedure
;;; convert-int
;;; Purpose
;;; change a number to a string ")" + that numbers
;;; Parameters
;;; num, a number
;;; Produces
;;; str, a string
;;; Preconditions
;;; none
;;; Postconditions
;;; if num is an exact integer 1-9 produces "0num"
;;; otherwise produces that number as a string

(define convert-int
  (lambda (num)
    (cond
      [(equal? num 1)
       "01"]
      [(equal? num 2)
       "02"]
      [(equal? num 3)
       "03"]
      [(equal? num 4)
       "04"]
      [(equal? num 5)
       "05"]
      [(equal? num 6)
       "06"]
      [(equal? num 7)
       "07"]
      [(equal? num 8)
       "08"]
      [(equal? num 9)
       "09"]
      [else
       (number->string num)])))

(define button (new button%
                    [label "Graph"]
                    [parent frame]
                    [callback (lambda (button event)
                                (let* ([state (send state get-value)]
                                       [date-yr-month (string-append "20200" (number->string (+ 1 (send date-month get-selection))))]
                                       [date (string-append date-yr-month (convert-int (+ 1 (send date-day get-selection))))]
                                       [graph (send graph-choice get-selection)])
                                  (display state)
                                  (newline)
                                  (display date)
                                  (newline)
                                  (if (= graph 0)
                                      (plot-total-pos state date)
                                      (plot-hospitalized-death-recovered state date))))]))

(define date-predicted-positive (new text-field%
                                     [label "Please enter a future date in 2020 (in the following format 20200101) to see the predicted number of positive cases in the state based on the current linear line of best fit."]
                                     [parent frame]))

(define button2 (new button%
                     [label "Calculate"]
                     [parent frame]
                     [callback (lambda (button event)
                                 (let* ([state (send state get-value)]
                                        [date (send date-predicted-positive get-value)])
                                   (display (state-linear-fit state date))
                                   (newline)))]))

(send frame show #t)

;;; Procedure
;;;
;;; Purpose
;;;
;;; Parameters
;;;
;;; Produces
;;;
;;; Preconditions
;;;
;;; Postconditions
;;; 