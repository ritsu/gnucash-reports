;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Barcharts with tag support
;;   - Based on the standard barcharts for asset, expense, income
;;     and liablity (category-barchart.scm)
;;   - Reads tags from account notes as comma-separated key:value
;;     pairs, for example:
;;       type: equity, cap: small, style: value, dom: us
;;       type: equity, cap: large, style: growth, dom: intl
;;   - Each generated report is associated with a tag-key, and
;;     balances are grouped by tag-values.
;;   - The following options have been added to the Tag tab:
;;       Group by (multi-choice):
;;         Choose from tag keys found in existing account notes
;;       Use parent tags as fallback (boolean):
;;         If an account does not have the group-by tag defined
;;         use the tag from the nearest parent that has it
;;       Display untagged balances (boolean):
;;         Display balances that are not assigned to any tag value
;;       Normalize balances for each interval (boolean):
;;         Display data as percentages, with balances from each
;;         period always adding up to 100%.
;;       Display table of accounts by tag (boolean):
;;         Display table showing which accounts contributed to
;;         which tag values.
;;
;; Note:
;;   - If an account has multiple values for the same tag,
;;     the current behaviour is to count the balance for the
;;     account multiple times, once for each tag value.
;;   - Consdering changing the above behaviour so accounts
;;     are only counted once. Q: For first tag value seen,
;;     or first tag value alphabetically?
;;
;; TODO:
;;   - Refactor: There are probably a lot of unnecessary
;;     loops and data structures that can be trimmed down.
;;   - Additional TODO items in code comments
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report tag-barchart-renderer))

;; Are these necessary?
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-26))
(use-modules (ice-9 match))
(use-modules (ice-9 string-fun))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

;; Option names
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))
(define optname-levels (N_ "Show Accounts until level"))

(define optname-fullname (N_ "Show long account names"))

(define optname-chart-type (N_ "Chart Type"))

(define optname-stacked (N_ "Use Stacked Charts"))
(define optname-slices (N_ "Maximum Bars"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))

(define optname-sort-method (N_ "Sort Method"))

(define optname-averaging (N_ "Show Average"))
(define opthelp-averaging (N_ "Select whether the amounts should be shown over the full time period or rather as the average e.g. per month."))

;; Tags: Tag options names
(define pagename-tags (N_ "Tags"))
(define optname-group-by (N_ "Group by"))
(define optname-use-parent (N_ "Use parent tags as fallback"))
(define optname-display-untagged (N_ "Display untagged balances"))
(define optname-normalize (N_ "Normalize balances for each interval"))
(define optname-show-accounts (N_ "Display table of accounts by tag"))

;; Tags: Get tags from account notes, as a list of (key value) pairs
;;       "key1: value1, key2: value2" -> ((key1 value1) (key2 value2))
;; Note: This is used in both options and renderer
(define (account->tag-pairs acct)
  (let ((tag-strings
          (filter (lambda (s) (memq #\: (string->list s)))
                  (string-split (xaccAccountGetNotes acct) #\,))))
    (map (lambda (s)
           (map (lambda (ss)
                  (string-trim-right (string-trim ss)))
             (string-split s #\:)))
      tag-strings)))

;; Options for the report (called by loader after it reloads this module)
(define (tag-barchart-options account-types do-intervals?)

  ;; Tags: Get unique tag-keys from existing accounts
  (define (account->tag-keys acct)
    (let* ((res '()))
      (for-each (lambda (tag-pair)
          (set! res (cons (car tag-pair) res)))
        (account->tag-pairs acct))
      res))
  (define tag-keys
    (let ((res '())
          (account-list (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      (for-each (lambda (acct)
          (set! res (append (account->tag-keys acct) res)))
        account-list)
      (delete-duplicates res)))

  (let* ((options (gnc:new-options))
    ;; This is just a helper function for making options.
    ;; See libgnucash/app-utils/options.scm for details.
    (add-option
     (lambda (new-option)
      (gnc:register-option options new-option))))

    ;; General tab
    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-from-date optname-to-date "a")

    (gnc:options-add-interval-choice!
     options gnc:pagename-general optname-stepsize "b" 'MonthDelta)

    (gnc:options-add-currency!
     options gnc:pagename-general optname-report-currency "c")

    (gnc:options-add-price-source!
     options gnc:pagename-general
     optname-price-source "d" 'weighted-average)

    (if do-intervals?
        (add-option
         (gnc:make-multichoice-option
          gnc:pagename-general optname-averaging
          "e" opthelp-averaging
          'None
          (list (vector 'None
                        (N_ "No Averaging")
                        (N_ "Just show the amounts, without any averaging."))
                (vector 'MonthDelta
                        (N_ "Monthly")
                        (N_ "Show the average monthly amount during the reporting period."))
                (vector 'WeekDelta
                        (N_ "Weekly")
                        (N_ "Show the average weekly amount during the reporting period."))
                (vector 'DayDelta
                        (N_ "Daily")
                        (N_ "Show the average daily amount during the reporting period."))))))

    ;; Tags: Add tab for tag-related options
    (if (null? tag-keys)
      (add-option
       (gnc:make-multichoice-option
        pagename-tags optname-group-by
        "a" (N_ "Group amounts by this tag type") 'None
        (list (vector 'None
                      "None"
                      "Please add tags to account notes"))))
      (add-option
       (gnc:make-multichoice-option
        pagename-tags optname-group-by
        "a" (N_ "Group amounts by this tag type") (string->symbol (car tag-keys))
        (map
          (lambda (tag-key)
            (vector
              (string->symbol tag-key)
              tag-key
              (string-append "Group by " tag-key)))
          tag-keys))))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-tags optname-use-parent
      "b" (N_ "User nearest parent tag if tag does not exist for an account") #t))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-tags optname-display-untagged
      "c" (N_ "Display balances for untagged accounts") #t))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-tags optname-normalize
      "d" (N_ "Normalize balances to 100% over each period") #f))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-tags optname-show-accounts
      "e" (N_ "Display table of accounts listed by tag value") #f))

    ;; Accounts tab
    (add-option
     (gnc:make-account-list-option gnc:pagename-accounts
      optname-accounts "a"
      (N_ "Include these accounts in the report.")
      (lambda ()
        (gnc:filter-accountlist-type
         account-types
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      (lambda (accounts)
        (list #t
              (gnc:filter-accountlist-type account-types accounts)))
      #t))

    ;; Tags: Force account-levels to 'all
    ; (gnc:options-add-account-levels!
    ;  options gnc:pagename-accounts optname-levels "c"
    ;  (N_ "Show accounts to this depth and not further.")
    ;  2)

    ;; Display tab
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-fullname
      "a" (N_ "Show the full account name in legend?") #f))

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-display optname-chart-type
      "b" "Select which chart type to use"
      'barchart
      (list (vector 'barchart
                    (N_ "Bar Chart")
                    (N_ "Use bar charts."))
            (vector 'linechart
                    (N_ "Line Chart")
                    (N_ "Use line charts.")))))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-stacked
      "c"
      (N_ "Show charts as stacked charts?")
      #t))

    (add-option
     (gnc:make-number-range-option
      gnc:pagename-display optname-slices
      "d" (N_ "Maximum number of stacks in the chart.") 8
      2 24 0 1))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      (N_ "Show table")
      "e" (N_ "Display a table of the selected data.")
      #f))

    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height "f" (cons 'percent 100.0) (cons 'percent 100.0))

    ;; Tags: TODO: Remove acct-code from list of options
    (gnc:options-add-sort-method!
     options gnc:pagename-display
     optname-sort-method "g" 'amount)

    (gnc:options-set-default-section options gnc:pagename-general)
    options))

; report renderer (called by loader after it reloads this module)
(define (tag-barchart-renderer report-obj reportname reportguid
                               account-types do-intervals?
                               reverse-bal? export-type)
  ;; A helper functions for looking up option values.
  (define (get-option section name)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) section name)))

  (gnc:report-starting reportname)
  (let* ((to-date-t64 (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general
                                    optname-to-date))))
         (from-date-t64 (gnc:time64-start-day-time
                         (gnc:date-option-absolute-time
                          (get-option gnc:pagename-general
                                      optname-from-date))))
         (interval (get-option gnc:pagename-general optname-stepsize))
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))
         (report-title (get-option gnc:pagename-general
                                   gnc:optname-reportname))
         (averaging-selection (if do-intervals?
                                  (get-option gnc:pagename-general
                                              optname-averaging)
                                  'None))

         (accounts (get-option gnc:pagename-accounts optname-accounts))
         ;; Tags: Force account-levels to 'all
         ; (account-levels (get-option gnc:pagename-accounts optname-levels))
         (account-levels 'all)

         (chart-type (get-option gnc:pagename-display optname-chart-type))
         (stacked? (get-option gnc:pagename-display optname-stacked))
         (show-fullname? (get-option gnc:pagename-display optname-fullname))
         (max-slices (inexact->exact
                      (get-option gnc:pagename-display optname-slices)))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))
         (sort-method (get-option gnc:pagename-display optname-sort-method))

         (group-by (get-option pagename-tags optname-group-by))
         (use-parent? (get-option pagename-tags optname-use-parent))
         (display-untagged? (get-option pagename-tags optname-display-untagged))
         (normalize? (get-option pagename-tags optname-normalize))
         (show-accounts? (get-option pagename-tags optname-show-accounts))

         (work-done 0)
         (work-to-do 0)
         (show-table? (get-option gnc:pagename-display (N_ "Show table")))
         (document (gnc:make-html-document))
         (chart (gnc:make-html-chart))
         (topl-accounts (gnc:filter-accountlist-type
                         account-types
                         (gnc-account-get-children-sorted
                          (gnc-get-current-root-account)))))

    ;; Returns true if the account a was selected in the account
    ;; selection option.
    (define (show-acct? a)
      (member a accounts))

    (define tree-depth (if (eq? account-levels 'all)
                           (gnc:get-current-account-tree-depth)
                           account-levels))

    ;; Tags: Divide gnc:monetary amounts
    (define (gnc:monetary/ a b)
      (/ (gnc:gnc-monetary-amount a)
         (gnc:gnc-monetary-amount b)))

    ;; Tags: n-digit precision rounding of a float
    ;; (f-round 5.678 2) -> 5.68
    (define (f-round f n)
      (let ((p (expt 10 n)))
        (/ (round (* p f)) p)))

    ;; (gnc:debug accounts)
    (if (not (null? accounts))

        ;; Define more helper variables.
        (let* ((commodity-list #f)
               (report-title (string-append report-title " [" (symbol->string group-by) "]"))
               (exchange-fn #f)
               (averaging-fraction-func (gnc:date-get-fraction-func averaging-selection))
               (interval-fraction-func (gnc:date-get-fraction-func interval))
               (averaging-multiplier
                (if averaging-fraction-func
                    ;; Calculate the divisor of the amounts so that an
                    ;; average is shown. Multiplier factor is a gnc-numeric
                    (let* ((start-frac-avg (averaging-fraction-func from-date-t64))
                           (end-frac-avg (averaging-fraction-func (1+ to-date-t64)))
                           (diff-avg (- end-frac-avg start-frac-avg))
                           (diff-avg-numeric (/ (inexact->exact (round (* diff-avg 1000000))) ; 6 decimals precision
                                                1000000))
                           (start-frac-int (interval-fraction-func from-date-t64))
                           (end-frac-int (interval-fraction-func (1+ to-date-t64)))
                           (diff-int (- end-frac-int start-frac-int))
                           (diff-int-numeric (inexact->exact diff-int)))
                      ;; Extra sanity check to ensure a number smaller than 1
                      (if (> diff-avg diff-int)
                          (/ diff-int-numeric diff-avg-numeric)
                          1))
                    1))
               ;; If there is averaging, the report-title is extended
               ;; accordingly.
               (report-title
                (case averaging-selection
                  ((MonthDelta) (string-append report-title " " (G_ "Monthly Average")))
                  ((WeekDelta) (string-append report-title " " (G_ "Weekly Average")))
                  ((DayDelta) (string-append report-title " " (G_ "Daily Average")))
                  (else report-title)))
               (currency-frac (gnc-commodity-get-fraction report-currency))
               ;; This is the list of date intervals to calculate.
               (dates-list (gnc:make-date-list
                            ((if do-intervals?
                                 gnc:time64-start-day-time
                                 gnc:time64-end-day-time) from-date-t64)
                            (gnc:time64-end-day-time to-date-t64)
                            (gnc:deltasym-to-delta interval)))
               ;; Here the date strings for the x-axis labels are
               ;; created.
               (other-anchor "")
               (all-data '())
               ;; Tags: placeholders
               (grouped-accounts '())
               (grouped-data '())
               (untagged-data '())
               (grouped-data-totals '())
               (grouped-data-normalized '()))

          ;; Converts a commodity-collector into gnc-monetary in the report's
          ;; currency using the exchange-fn calculated above. Returns a gnc-monetary
          ;; multiplied by the averaging-multiplier (smaller than one; multiplication
          ;; instead of division to avoid division-by-zero issues) in case
          ;; the user wants to see the amounts averaged over some value.
          (define (collector->monetary c date)
            (gnc:make-gnc-monetary
             report-currency
             (* averaging-multiplier
                (gnc:gnc-monetary-amount
                 (gnc:sum-collector-commodity
                  c report-currency
                  (lambda (a b) (exchange-fn a b date)))))))

          ;; copy of gnc:not-all-zeros using gnc-monetary
          (define (not-all-zeros data)
            (cond ((gnc:gnc-monetary? data) (not (zero? (gnc:gnc-monetary-amount data))))
                  ((list? data) (or-map not-all-zeros data))
                  (else #f)))

          ;; this is an alist of account-balances
          ;; (list (list acc0 bal0 bal1 bal2 ...)
          ;;       (list acc1 bal0 bal1 bal2 ...)
          ;;       ...)
          ;; whereby each balance is a gnc-monetary
          (define account-balances-alist
            (map
             (lambda (acc)
               (let* ((comm (xaccAccountGetCommodity acc))
                      (split->elt (if reverse-bal?
                                      (lambda (s)
                                        (gnc:make-gnc-monetary
                                         comm (- (xaccSplitGetNoclosingBalance s))))
                                      (lambda (s)
                                        (gnc:make-gnc-monetary
                                         comm (xaccSplitGetNoclosingBalance s))))))
                 (cons acc
                       (gnc:account-accumulate-at-dates
                        acc dates-list
                        #:split->elt split->elt
                        #:nosplit->elt (gnc:make-gnc-monetary comm 0)))))
             ;; all selected accounts (of report-specific type), *and*
             ;; their descendants (of any type) need to be scanned.
             (gnc:accounts-and-all-descendants accounts)))

          ;; Creates the <balance-list> to be used in the function
          ;; below.
          (define (account->balance-list account subacct?)
            (let* ((accountslist (cons account
                                   (if subacct?
                                       (gnc-account-get-descendants account)
                                       '())))
                   (selected-balances (filter
                                       (lambda (entry)
                                         (member (car entry) accountslist))
                                       account-balances-alist))
                   (selected-monetaries (map cdr selected-balances))
                   (list-of-mon-collectors (apply map gnc:monetaries-add selected-monetaries)))
              (let loop ((list-of-mon-collectors list-of-mon-collectors)
                         (dates-list dates-list)
                         (result '()))
                (if (null? (if do-intervals?
                               (cdr list-of-mon-collectors)
                               list-of-mon-collectors))
                    (reverse result)
                    (loop (cdr list-of-mon-collectors)
                          (cdr dates-list)
                          (cons (if do-intervals?
                                    (collector->monetary
                                     (gnc:collector- (cadr list-of-mon-collectors)
                                                     (car list-of-mon-collectors))
                                     (cadr dates-list))
                                    (collector->monetary
                                     (car list-of-mon-collectors)
                                     (car dates-list)))
                                result))))))

          (define (count-accounts current-depth accts)
            (if (< current-depth tree-depth)
                (let ((sum 0))
                  (for-each
                   (lambda (a)
                     (set! sum (+ sum (1+ (count-accounts (1+ current-depth)
                                                          (gnc-account-get-children a))))))
                   accts)
                  sum)
                (length (filter show-acct? accts))))

          ;; Calculates all account's balances. Returns a list of pairs:
          ;; (<account> <balance-list>), like '((Earnings (10.0 11.2))
          ;; (Gifts (12.3 14.5))), where each element of <balance-list>
          ;; is the balance corresponding to one element in
          ;; <dates-list>.
          ;;
          ;; If current-depth >= tree-depth, then the balances are
          ;; calculated *with* subaccount's balances. Else only the
          ;; current account is regarded. Note: All accounts in accts
          ;; and all their subaccounts are processed, but a balances is
          ;; calculated and returned *only* for those accounts where
          ;; show-acct? is true. This is necessary because otherwise we
          ;; would forget an account that is selected but not its
          ;; parent.
          (define (traverse-accounts current-depth accts)
            (if (< current-depth tree-depth)
                (let ((res '()))
                  (for-each
                   (lambda (a)
                     (begin
                       (set! work-done (1+ work-done))
                       (gnc:report-percent-done (+ 20 (* 70 (/ work-done work-to-do))))
                       (if (show-acct? a)
                           (set! res
                             (cons (list a (account->balance-list a #f))
                                   res)))
                       (set! res (append
                                  (traverse-accounts
                                   (1+ current-depth)
                                   (gnc-account-get-children a))
                                  res))))
                   accts)
                  res)
                ;; else (i.e. current-depth == tree-depth)
                (map
                 (lambda (a)
                   (set! work-done (1+ work-done))
                   (gnc:report-percent-done (+ 20 (* 70 (/ work-done work-to-do))))
                   (list a (account->balance-list a #t)))
                 (filter show-acct? accts))))

          ;; Tags: Get value(s) for group-by tag from account
          (define (account->tag-values acct orig-acct depth)
            (let* ((res '()))
              (for-each (lambda (tag-pair)
                  (when (equal? (car tag-pair) (symbol->string group-by))
                    (set! res (cons (cadr tag-pair) res))))
                (account->tag-pairs acct))
              ; If account did not have tag, search parents if use-parent selected
              ; TODO: detect when we are at root account
              ; FIXME: magic number
              (if (and (null? res) use-parent? (< depth 6))
                (account->tag-values (gnc-account-get-parent acct) orig-acct (1+ depth))
                (begin
                  (when (and (null? res) display-untagged?)
                    (set! res (list "Untagged")))
                  (for-each (lambda (r)
                      (set! grouped-accounts (cons (list r orig-acct) grouped-accounts)))
                    res)
                  res))))

          ;; Tags: Get unique values for group-by tag from list of accounts
          (define (data->tag-values all-data)
            (let ((res '()))
              (for-each (lambda (d)
                  (set! res (append res (account->tag-values (car d) (car d) 0))))
                all-data)
              (delete-duplicates res)))

          ;; Tags: Create list of balances for each unique tag-value in all-data
          (define (group-by-tag-value all-data)
            (let ((res '()))
              (for-each (lambda (v)
                  (let* ((matched-data (filter
                           ; TODO: define a precomputed list of (<account> <tag-values>)
                           ; so we don't have to recalculate tag-values every time?
                           (lambda (d) (member v (account->tag-values (car d) (car d) 0)))
                           all-data))
                         (value-sum (map
                           (lambda (l) (apply gnc:monetary+ l))
                           (apply zip (map cadr matched-data)))))
                    (set! res (cons (list v value-sum) res))))
                (data->tag-values all-data))
            res))

          ;; The percentage done numbers here are a hack so that
          ;; something gets displayed. On my system the
          ;; gnc:case-exchange-time-fn takes about 20% of the time
          ;; building up a list of prices for later use. Either this
          ;; routine needs to send progress reports, or the price
          ;; lookup should be distributed and done when actually
          ;; needed so as to amortize the cpu time properly.
          (gnc:report-percent-done 1)
          (set! commodity-list (gnc:accounts-get-commodities
                                (gnc:accounts-and-all-descendants accounts)
                                report-currency))
          (set! exchange-fn (gnc:case-exchange-time-fn
                             price-source report-currency
                             commodity-list to-date-t64
                             5 15))

          (set! work-to-do (count-accounts 1 topl-accounts))

          ;; Sort the account list according to the account code field.
          (set! all-data
            (sort
             (filter (lambda (l)
                       (not (zero? (gnc:gnc-monetary-amount
                                    (apply gnc:monetary+ (cadr l))))))
                     (traverse-accounts 1 topl-accounts))
             (case sort-method
               ((alphabetical)
                (lambda (a b)
                  (if show-fullname?
                      (gnc:string-locale<? (gnc-account-get-full-name (car a))
                                           (gnc-account-get-full-name (car b)))
                      (gnc:string-locale<? (xaccAccountGetName (car a))
                                           (xaccAccountGetName (car b))))))
               ((acct-code)
                (lambda (a b)
                  (gnc:string-locale<? (xaccAccountGetCode (car a))
                                       (xaccAccountGetCode (car b)))))
               ((amount)
                (lambda (a b)
                  (> (gnc:gnc-monetary-amount (apply gnc:monetary+ (cadr a)))
                     (gnc:gnc-monetary-amount (apply gnc:monetary+ (cadr b)))))))))

          ;; Tags: Create grouped-data, sorted by user option
          (set! grouped-data
            (sort
              (group-by-tag-value all-data)
              (case sort-method
                ((alphabetical)
                 (lambda (a b)
                   (gnc:string-locale<? (car a)
                                        (car b))))
                ;; Tags: Need to account for acct-code case until we remove it from options
                (else
                 (lambda (a b)
                   (> (gnc:gnc-monetary-amount (apply gnc:monetary+ (cadr a)))
                      (gnc:gnc-monetary-amount (apply gnc:monetary+ (cadr b)))))))))

          ;; Proceed if the data is non-zeros
          ;; Tags: Replaced all-data with grouped-data
          ;;       Probably does not make a difference, since conditionals for both
          ;;       will (should?) always be the same
          (if
           (and (not (null? grouped-data))
                (not-all-zeros  (map cadr grouped-data)))

           (let* ((dates-list (if do-intervals?
                                  (list-head dates-list (1- (length dates-list)))
                                  dates-list))
                  (date-string-list (map qof-print-date dates-list)))

             ;; Set chart title, subtitle etc.

             (gnc:html-chart-set-type!
              chart (if (eq? chart-type 'barchart) 'bar 'line))

             (gnc:html-chart-set-title!
              chart (list report-title
                          (format #f
                                  (if do-intervals?
                                      (G_ "~a to ~a")
                                      (G_ "Balances ~a to ~a"))
                                  (qof-print-date from-date-t64)
                                  (qof-print-date to-date-t64))))

             (gnc:html-chart-set-width! chart width)
             (gnc:html-chart-set-height! chart height)

             (gnc:html-chart-set-data-labels! chart date-string-list)

             ;; Tags: Normalized label
             (if normalize?
               (gnc:html-chart-set-y-axis-label! chart "Percent")
               (gnc:html-chart-set-y-axis-label!
                 chart (gnc-commodity-get-mnemonic report-currency)))

             ;; If we have too many categories, we sum them into a new
             ;; 'other' category and add a link to a new report with just
             ;; those accounts.
             ;; Tags: Replaced all-data with grouped-data
             (if (> (length grouped-data) max-slices)
                 (let* ((start (take grouped-data (1- max-slices)))
                        (finish (drop grouped-data (1- max-slices)))
                        (other-sum (map
                                    (lambda (l) (apply gnc:monetary+ l))
                                    (apply zip (map cadr finish)))))
                   (set! grouped-data
                     (append start
                             (list (list (G_ "Other") other-sum))))
                   (let* ((options (gnc:make-report-options reportguid)))
                     ;; now copy all the options
                     (gnc:options-copy-values
                      (gnc:report-options report-obj) options)
                     ;; and set the destination accounts
                     (gnc:option-set-value
                      (gnc:lookup-option options gnc:pagename-accounts
                                         optname-accounts)
                      (map car finish))
                     ;; Set the URL to point to this report.
                     (set! other-anchor
                       (gnc:report-anchor-text
                        (gnc:make-report reportguid options))))))

             (gnc:report-percent-done 92)

             ;; Tags: Normalize to 100% total for each period
             (when normalize?
               (let*
                 ((zipped-data
                    (apply zip
                      (map (lambda (d) (append (list (car d)) (cadr d)))
                           grouped-data)))
                  (norm-data
                    (cons
                      (car zipped-data)
                      (map (lambda (dms)
                        (let* ((tm (apply gnc:monetary+ dms))
                               ;; Hash map so we can recover original order after sorting
                               (index 0)
                               (nmap (map
                                   (lambda (dm)
                                     (set! index (+ 1 index))
                                     (if (zero? (gnc:gnc-monetary-amount tm))
                                       (list index 0)
                                       (list index (f-round (* 100 (gnc:monetary/ dm tm)) 2))))
                                   dms))
                               ;; Set max < 100 to avoid y-axis overflow from rounding
                               (norm-diff (- 99.99999 (apply + (map cadr nmap)))))
                          (if (zero? norm-diff)
                            (map cadr nmap)
                            ;; Add rounding error diff to largest element in nmap
                            (let*
                              ((nmap-sorted
                                 (sort
                                   nmap
                                   (lambda (a b) (> (cadr a) (cadr b)))))
                               (nmap-fixed
                                 (cons
                                   (list (car (car nmap-sorted))
                                         (+ norm-diff (cadr (car nmap-sorted))))
                                   (cdr nmap-sorted))))
                              ; (map cadr nmap-fixed)))))
                              (map cadr
                                   (sort
                                     nmap-fixed
                                     (lambda (a b) (< (car a) (car b)))))))))
                        (cdr zipped-data)))))
                  (set! grouped-data (map (lambda (d) (list (car d) (cdr d)))
                       (apply zip norm-data)))))

             ;; Tags: Replaced all-data with grouped-data in this (for-each) block
             ;;       See iterable arguments to (lambda ())
             (for-each
              (lambda (series color stack)
                (let* ((acct (car series))
                       (label (cond
                               ((string? acct)
                                (car series))
                               (show-fullname?
                                (gnc-account-get-full-name acct))
                               (else (xaccAccountGetName acct))))
                       ;; Tags: Normalized amounts
                       (amounts (if normalize?
                         (cadr series)
                         (map gnc:gnc-monetary-amount (cadr series))))
                       (stack (if stacked?
                                  "default"
                                  (number->string stack)))
                       (fill (eq? chart-type 'barchart))
                       (urls (cond
                              ((string? acct)
                               other-anchor)

                              ((null? (gnc-account-get-children acct))
                               (gnc:account-anchor-text acct))

                              ;; because the tree-depth option for
                              ;; accounts/levels goes up to 6. FIXME:
                              ;; magic number.
                              ((>= tree-depth 6)
                               (gnc:account-anchor-text acct))

                              (else
                               (gnc:make-report-anchor
                                reportguid report-obj
                                (list
                                 (list gnc:pagename-accounts optname-accounts
                                       (cons acct (gnc-account-get-children acct)))
                                 (list gnc:pagename-accounts optname-levels
                                       (1+ tree-depth))
                                 (list gnc:pagename-general
                                       gnc:optname-reportname
                                       (if show-fullname?
                                           (gnc-account-get-full-name acct)
                                           (xaccAccountGetName acct)))))))))
                  (gnc:html-chart-add-data-series!
                   chart label amounts color
                   'stack stack 'fill fill 'urls urls)))
              grouped-data
              (gnc:assign-colors (length grouped-data))
              (iota (length grouped-data)))
             ;; END (for-each ... )

             (gnc:html-chart-set-stacking?! chart stacked?)

             ;; Tags: Do not show currency for normalized chart
             ;; TODO: Find a way to actually set this to percentages?
             (gnc:html-chart-set-currency-iso!
              chart (gnc-commodity-get-mnemonic report-currency))
             (if normalize?
               (gnc:html-chart-set-currency-symbol!
                chart "")
               (gnc:html-chart-set-currency-symbol!
                chart (gnc-commodity-get-nice-symbol report-currency)))

             (gnc:report-percent-done 98)
             (gnc:html-document-add-object! document chart)

             ;; Tags: Replaced all-data with grouped-data in this (when) block
             (when show-table?
               (let ((table (gnc:make-html-table))
                     (scu (gnc-commodity-get-fraction report-currency))
                     (cols>1? (pair? (cdr grouped-data))))

                 (define (make-cell contents)
                   (gnc:make-html-table-cell/markup "number-cell" contents))

                 (for-each
                  (lambda (date row)
                    (gnc:html-table-append-row!
                     table
                     (append (list (make-cell date))
                             (map make-cell row)
                             (if cols>1?
                                 (list
                                  ;; Tags: Normalized amounts
                                  (if normalize?
                                    (make-cell (apply + row))
                                    (make-cell (apply gnc:monetary+ row))))
                                 '()))))
                  date-string-list
                  (apply zip (map cadr grouped-data)))

                 (gnc:html-table-set-col-headers!
                  table
                  (append
                   (list (G_ "Date"))
                   (map
                    (lambda (col)
                      (cond
                       ((string? col) col)
                       (show-fullname? (gnc-account-get-full-name col))
                       (else (xaccAccountGetName col))))
                    (map car grouped-data))
                   (if cols>1?
                       (list (G_ "Grand Total"))
                       '())))

                 (gnc:html-document-add-object! document table)))
             ;; END (when ... )

             ;; Tags: Show which accounts contributed to which tag-values
             ;; TODO: Currently, if multiple accounts have the same name,
             ;; the name only appears once under any given tag-value.
             ;; In some cases this helps reduce clutter, in other cases it
             ;; may cause confusion. Consider changing behaviour...
             (when show-accounts?
               (let ((groups (delete-duplicates (map car grouped-accounts)))
                     (table (gnc:make-html-table)))
                 (gnc:html-table-set-style!
                   table "th"
                   'attribute '("align" "left"))
                 (gnc:html-table-set-col-headers! table groups)
                 (gnc:html-table-append-row! table
                   (map (lambda (group)
                       (apply
                         string-append
                         (sort (delete-duplicates
                             (map (lambda (ga)
                                 (string-append
                                   (if show-fullname?
                                     (gnc-account-get-full-name (cadr ga))
                                     (xaccAccountGetName (cadr ga)))
                                   "<br/>"))
                                  (filter
                                    (lambda (g) (equal? (car g) group))
                                    grouped-accounts)))
                             (lambda (a b)
                               (gnc:string-locale<? a b)))))
                     groups))
                 (gnc:html-document-add-object! document table)))

             ;; Tags: Replaced all-data with grouped-data in this (cond) block
             (cond
              ((eq? export-type 'csv)
               (let ((iso-date (qof-date-format-get-string QOF-DATE-FORMAT-ISO)))
                 (gnc:html-document-set-export-string
                  document
                  (gnc:lists->csv
                   (cons (append
                          (list (G_ "Date"))
                          (map
                           (lambda (col)
                             (cond
                              ((string? col) col)
                              (show-fullname? (gnc-account-get-full-name col))
                              (else (xaccAccountGetName col))))
                           (map car grouped-data))
                          (if (pair? (cdr grouped-data))
                              (list (G_ "Grand Total"))
                              '()))
                         (map
                          (lambda (date row)
                            (append
                             (list date)
                             row
                             (if (pair? (cdr grouped-data))
                                 (list (apply gnc:monetary+ row))
                                 '())))
                          (map (cut gnc-print-time64 <> iso-date) dates-list)
                          (apply zip (map cadr grouped-data))))))))))
             ;; END (cond ... )

           ;; else if empty data
           (gnc:html-document-add-object!
            document
            (gnc:html-make-empty-data-warning
             report-title (gnc:report-id report-obj)))))

        ;; else if no accounts selected
        (gnc:html-document-add-object!
         document
         (gnc:html-make-no-account-warning
          report-title (gnc:report-id report-obj))))

    (unless (gnc:html-document-export-string document)
      (gnc:html-document-set-export-error document (G_ "No exportable data")))


    (gnc:report-finished)
    document))


(export tag-barchart-options)
(export tag-barchart-renderer)
