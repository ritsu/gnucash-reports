;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Piecharts with tag support
;;   - Based on the standard piecharts for asset, expense, income
;;     and liablity (account-piecharts.scm)
;;   - Reads tags from account notes as comma-separated key:value
;;     pairs, for example:
;;       type: equity, cap: small, style: value, dom: us
;;       type: equity, cap: large, style: growth, dom: intl
;;   - Each generated report is associated with a tag-key, and
;;     balances are grouped by tag-values.
;;
;; Note:
;;   - If an account has multiple values for the same tag,
;;     the current behaviour is to count the balance for the
;;     account multiple times, once for each tag value.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report tag-piechart-renderer))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))

(define menuname-income (N_ "Income Piechart"))
(define menuname-expense (N_ "Expense Piechart"))
(define menuname-assets (N_ "Asset Piechart"))
(define menuname-securities (N_ "Security Piechart"))
(define menuname-liabilities (N_ "Liability Piechart"))
;; The names are used in the menu

;; The menu statusbar tips.
(define menutip-income
  (N_ "Shows a piechart with the Income per given time interval"))
(define menutip-expense
  (N_ "Shows a piechart with the Expenses per given time interval"))
(define menutip-assets
  (N_ "Shows a piechart with the Assets balance at a given time"))
(define menutip-securities
  (N_ "Shows a piechart with distribution of assets over securities"))
(define menutip-liabilities
  (N_ "Shows a piechart with the Liabilities \
balance at a given time"))

;; The names here are used 1. for internal identification, 2. as
;; tab labels, 3. as default for the 'Report name' option which
;; in turn is used for the printed report title.
(define reportname-income (N_ "Income Accounts"))
(define reportname-expense (N_ "Expense Accounts"))
(define reportname-assets (N_ "Assets"))
(define reportname-securities (N_ "Securities"))
(define reportname-liabilities (N_ "Liabilities"))

(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))
(define optname-levels (N_ "Show Accounts until level"))

(define optname-fullname (N_ "Show long names"))
(define optname-show-total (N_ "Show Totals"))
(define optname-show-percent (N_ "Show Percents"))
(define optname-slices (N_ "Maximum Slices"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))
(define optname-sort-method (N_ "Sort Method"))

(define optname-averaging (N_ "Show Average"))
(define opthelp-averaging (N_ "Select whether the amounts should be shown over the full time period or rather as the average e.g. per month."))

;; Tags: Tag options names
(define pagename-tags (N_ "Tags"))
(define optname-group-by (N_ "Group by"))
(define optname-custom-sort (N_ "Custom sort"))
(define optname-use-parent (N_ "Use parent tags as fallback"))
(define optname-display-untagged (N_ "Display untagged balances"))
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

;; The option-generator. The only dependence on the type of piechart
;; is the list of account types that the account selection option
;; accepts.
(define (tag-piechart-options account-types reverse-balance? do-intervals?)

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
         (add-option
          (lambda (new-option)
            (gnc:register-option options new-option))))

    (add-option
     (gnc:make-internal-option "__report" "reverse-balance?" reverse-balance?))

    (if do-intervals?
        (gnc:options-add-date-interval!
         options gnc:pagename-general
         optname-from-date optname-to-date "a")
        (gnc:options-add-report-date!
         options gnc:pagename-general
         optname-to-date "a"))

    (gnc:options-add-currency!
     options gnc:pagename-general optname-report-currency "b")

    (gnc:options-add-price-source!
     options gnc:pagename-general
     optname-price-source "c" 'pricedb-nearest)

    (if do-intervals?
        (add-option
         (gnc:make-multichoice-option
          gnc:pagename-general optname-averaging
          "f" opthelp-averaging
          'None
          (list (vector 'None
                        (N_ "No Averaging")
                        (N_ "Just show the amounts, without any averaging."))
                (vector 'YearDelta
                        (N_ "Yearly")
                        (N_ "Show the average yearly amount during the reporting period."))
                (vector 'MonthDelta
                        (N_ "Monthly")
                        (N_ "Show the average monthly amount during the reporting period."))
                (vector 'WeekDelta
                        (N_ "Weekly")
                        (N_ "Show the average weekly amount during the reporting period."))
                )
          ))
        )

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
     (gnc:make-string-option
      pagename-tags optname-custom-sort
      "b" (N_ "Sort according to a list of tag values") (N_ "Value1, Value2, Value3")))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-tags optname-use-parent
      "c" (N_ "User nearest parent tag if tag does not exist for an account") #t))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-tags optname-display-untagged
      "d" (N_ "Display balances for untagged accounts") #t))

    (add-option
     (gnc:make-simple-boolean-option
      pagename-tags optname-show-accounts
      "e" (N_ "Display table of accounts listed by tag value") #f))

    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "a"
      (N_ "Report on these accounts, if chosen account level allows.")
      (lambda ()
        (gnc:filter-accountlist-type
         account-types
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      (lambda (accounts)
        (list #t
              (gnc:filter-accountlist-type
               account-types
               accounts)))
      #t))

    ;; Tags: Force account-levels to 'all
    ; (gnc:options-add-account-levels!
    ;  options gnc:pagename-accounts optname-levels "b"
    ;  (N_ "Show accounts to this depth and not further.")
    ;  2)

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-fullname
      "a"
      (N_ "Show the full account name in legend?")
      #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-show-total
      "b" (N_ "Show the total balance in legend?") #t))


     (add-option
      (gnc:make-simple-boolean-option
       gnc:pagename-display optname-show-percent
       "b" (N_ "Show the percentage in legend?") #t))


    (add-option
     (gnc:make-number-range-option
      gnc:pagename-display optname-slices
      "c" (N_ "Maximum number of slices in pie.") 7
      2 24 0 1))

    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height "d" (cons 'percent 100.0) (cons 'percent 100.0))

    ;; Tags: Add custom sort option
    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-display optname-sort-method
      "g" "Choose the method for sorting accounts."
      'amount
      (list (vector 'alphabetical
                    (N_ "Alphabetical")
                    (N_ "Alphabetical by account name."))
            (vector 'amount
                    (N_ "Amount")
                    (N_ "By amount, largest to smallest."))
            (vector 'custom
                    (N_ "Custom")
                    (N_ "By custom order defined in tag options.")))))

    (gnc:options-set-default-section options gnc:pagename-general)

    options))


;; The rendering function. Since it works for a bunch of different
;; account settings, you have to give the reportname, the
;; account-types to work on and whether this report works on
;; intervals as arguments.
(define (tag-piechart-renderer report-obj reportname report-guid
                               account-types do-intervals?)

  ;; This is a helper function for looking up option values.
  (define (get-option section name)
    (gnc:option-value
      (gnc:lookup-option
        (gnc:report-options report-obj) section name)))

  (gnc:report-starting reportname)

  ;; Get all options
  (let ((to-date (gnc:time64-end-day-time
                   (gnc:date-option-absolute-time
                     (get-option gnc:pagename-general optname-to-date))))
        (from-date (if do-intervals?
                     (gnc:time64-start-day-time
                       (gnc:date-option-absolute-time
                         (get-option gnc:pagename-general
                                     optname-from-date)))
                     '()))
        (accounts (get-option gnc:pagename-accounts optname-accounts))
        ;; Tags: Force account-levels to 'all
        ; (account-levels
        ;   (get-option gnc:pagename-accounts optname-levels))
        (account-levels 'all)
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

        (show-fullname? (get-option gnc:pagename-display optname-fullname))
        (show-total? (get-option gnc:pagename-display optname-show-total))
        (show-percent? (get-option gnc:pagename-display optname-show-percent))
        (max-slices (inexact->exact
                      (get-option gnc:pagename-display optname-slices)))
        (height (get-option gnc:pagename-display optname-plot-height))
        (width (get-option gnc:pagename-display optname-plot-width))
        (sort-method (get-option gnc:pagename-display optname-sort-method))
        (reverse-balance? (get-option "__report" "reverse-balance?"))

        ;; Tags: tag tab options
        (group-by (get-option pagename-tags optname-group-by))
        (custom-sort (get-option pagename-tags optname-custom-sort))
        (use-parent? (get-option pagename-tags optname-use-parent))
        (display-untagged? (get-option pagename-tags optname-display-untagged))
        (show-accounts? (get-option pagename-tags optname-show-accounts))

        (work-done 0)
        (work-to-do 0)
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

    ;; Calculates the net balance (profit or loss) of an account
    ;; over the selected reporting period. If subaccts? == #t, all
    ;; subaccount's balances are included as well. Returns a
    ;; commodity-collector.
    (define (profit-fn account subaccts?)
      (if do-intervals?
        (gnc:account-get-comm-balance-interval
          account from-date to-date subaccts?)
        (gnc:account-get-comm-balance-at-date
          account to-date subaccts?)))

    (if (not (null? accounts))

      ;; Define more helper variables.
      (let* ((exchange-fn (gnc:case-exchange-fn
                            price-source report-currency to-date))
             (tree-depth (gnc:get-current-account-tree-depth))
             (averaging-fraction-func (gnc:date-get-fraction-func averaging-selection))
             (averaging-multiplier
               (if averaging-fraction-func
                 ;; Calculate the divisor of the amounts so that an
                 ;; average is shown
                 (let* ((start-frac (averaging-fraction-func from-date))
                        (end-frac (averaging-fraction-func (+ 1 to-date)))
                        (diff (- end-frac start-frac)))
                   ;; Extra sanity check to ensure a positive number
                   (if (> diff 0)
                     (/ 1 diff)
                     1))
                 ;; No interval-report, or no averaging interval chosen,
                 ;; so just use the multiplier one
                 1))
             ;; If there is averaging, the report-title is extended
             ;; accordingly.
             (report-title
               (case averaging-selection
                 ((YearDelta) (string-append report-title " " (G_ "Yearly Average")))
                 ((MonthDelta) (string-append report-title " " (G_ "Monthly Average")))
                 ((WeekDelta) (string-append report-title " " (G_ "Weekly Average")))
                 (else report-title)))
             (combined '())
             (other-anchor "")
             ;; Tags: placeholders
             (grouped-hash-table (make-hash-table))
             (grouped-data '()))

        ;; Converts a commodity-collector into one single inexact
        ;; number, depending on the report's currency and the
        ;; exchange-fn calculated above. Returns the absolute value
        ;; multiplied by the averaging-multiplier (smaller than one;
        ;; multiplication instead of division to avoid division-by-zero
        ;; issues) in case the user wants to see the amounts averaged
        ;; over some value.
        (define (collector->amount c)
          ;; Future improvement: Let the user choose which kind of
          ;; currency combining she want to be done. Right now
          ;; everything foreign gets converted
          ;; (gnc:sum-collector-commodity) based on the average
          ;; cost of all holdings.
          (* (gnc:gnc-monetary-amount
               (gnc:sum-collector-commodity c report-currency exchange-fn))
             averaging-multiplier))

        ;; Get balance of an account as an inexact number converted to,
        ;; and using precision of the report's currency.
        (define (account-balance a subaccts?)
          (collector->amount (profit-fn a subaccts?)))

        (define (count-accounts current-depth accts)
          (if (< current-depth tree-depth)
            (let iter ((sum 0)
                       (remaining accts))
              (if (null? remaining)
                sum
                (let* ((cur (car remaining))
                       (tail (cdr remaining))
                       (subaccts (count-accounts (1+ current-depth)
                                                 (gnc-account-get-children cur))))
                  (iter (+ sum (1+ subaccts)) tail))))
            (length (filter show-acct? accts))))

        (define (fix-signs combined)
          (map (lambda (pair)
                 (if (reverse-balance? (cadr pair))
                   (cons (- (car pair)) (cdr pair))
                   pair))
               combined))

        ;; Tags: Get value(s) for group-by tag from account
        (define (account->tag-values acct depth)
          (let* ((res '()))
            (for-each
              (lambda (tag-pair)
                (when (equal? (car tag-pair) (symbol->string group-by))
                  (set! res (cons (cadr tag-pair) res))))
              (account->tag-pairs acct))
            ; If account did not have tag, search parents if use-parent selected
            ; TODO: detect when we are at root account
            ; FIXME: magic number
            (if (and (null? res) use-parent? (< depth 6))
              (account->tag-values (gnc-account-get-parent acct) (1+ depth))
              (begin
                (when (and (null? res) display-untagged?)
                  (set! res (list "Untagged")))
                res))))

        ;; Tags: Add data from account to hash table
        ;; (tag-value: <balance-list> <tag-value-balance>)
        (define (add-tagged-account! table account)
          (let ((balance (account-balance account #f)))
            (for-each
              (lambda (v)
                (let* ((handle (hash-create-handle! table v (list '() 0)))
                       (val (cdr handle)))
                  (hash-set! table v
                             (list (cons account (car val))
                                   (+ (cadr val) balance)))))
              (account->tag-values account 0))))

        ;; Tags: Similar to traverse-accounts, except balances are not
        ;; calculated and stored for individual accounts. Instead, balances
        ;; are added to a hash table grouped by tag values.
        (define (traverse-accounts! table accts)
          (for-each
            (lambda (a)
              (set! work-done (1+ work-done))
              (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
              (if (show-acct? a)
                (add-tagged-account! table a))
              (traverse-accounts! table (gnc-account-get-children a)))
            accts))

        ;; Tags: Create grouped-data from grouped-hash-table
        ;; ((<balance> <tag-value>) (<balance> <tag-value>) ...)
        (define (hash-table->grouped-data table)
          (hash-map->list (lambda (k v) (list (cadr v) k)) table))

        ;; Tags: Create grouped-accounts from grouped-hash-table
        (define (hash-table->grouped-accounts table)
          (hash-map->list (lambda (k v) (list k (car v))) table))

        ;; Start work
        (gnc:report-percent-done 1)
        (set! work-to-do (count-accounts 1 topl-accounts))

        ;; Tags: Populate grouped-hash-table
        (traverse-accounts! grouped-hash-table topl-accounts)

        ;; Tags: Remove Untagged group if it has no accounts
        (when
          (and (hash-get-handle grouped-hash-table "Untagged")
               (zero? (caddr (hash-get-handle grouped-hash-table "Untagged"))))
          (hash-remove! grouped-hash-table "Untagged"))

        ;; Tags: Create grouped-data, sorted by user option
        (set! grouped-data
          (sort
            (hash-table->grouped-data grouped-hash-table)
            (case sort-method
              ((alphabetical)
               (lambda (a b)
                 (gnc:string-locale<? (cadr a)
                                      (cadr b))))
              ((amount)
               (lambda (a b)
                 (> (car a)
                    (car b))))
              ((custom)
               (lambda (a b)
                 (let* ((index 0)
                        (custom-sort-list
                          (map (lambda (tag-value)
                                 (set! index (+ 1 index))
                                 (list index (string-trim-right (string-trim tag-value))))
                               (string-split custom-sort #\,)))
                        (get-key (lambda (value)
                                   (let ((res
                                           (filter (lambda (kv) (equal? (cadr kv) value))
                                                   custom-sort-list)))
                                     (if (zero? (length res))
                                       999
                                       (car (car res)))))))
                   (< (get-key (cadr a))
                      (get-key (cadr b)))))))))

        (set! combined
          (filter (lambda (pair) (not (>= 0.0 (car pair))))
                  (fix-signs grouped-data)))

        ;; if too many slices, condense them to an 'other' slice
        ;; and add a link to a new pie report with just those
        ;; accounts
        (if (> (length combined) max-slices)
          (let* ((start (take combined (- max-slices 1)))
                 (finish (drop combined (- max-slices 1)))
                 (sum (apply + (unzip1 finish))))
            (set! combined
              (append start
                      (list (list sum (G_ "Other")))))
            (let ((options (gnc:make-report-options report-guid))
                  (id #f))
              ;; now copy all the options
              (gnc:options-copy-values (gnc:report-options report-obj)
                                       options)
              ;; and set the destination accounts
              (gnc:option-set-value
                (gnc:lookup-option options gnc:pagename-accounts
                                   optname-accounts)
                (map cadr finish))
              (set! id (gnc:make-report report-guid options))
              ;; set the URL.
              (set! other-anchor (gnc:report-anchor-text id)))))

        (if
          (not (null? combined))
          (let ((urls (map
                        (lambda (series)
                          (if (string? (cadr series))
                            other-anchor
                            (let* ((acct (cadr series))
                                   (subaccts (gnc-account-get-children acct)))
                              (if (null? subaccts)
                                (gnc:account-anchor-text (cadr series))
                                (gnc:make-report-anchor
                                  report-guid
                                  report-obj
                                  (list
                                    (list gnc:pagename-accounts optname-accounts
                                          (cons acct subaccts))
                                    (list gnc:pagename-accounts optname-levels
                                          (+ 1 tree-depth))
                                    (list gnc:pagename-general
                                          gnc:optname-reportname
                                          ((if show-fullname?
                                             gnc-account-get-full-name
                                             xaccAccountGetName) acct))))))))
                        combined))
                (scu (gnc-commodity-get-fraction report-currency)))

            (define (round-scu amt)
              (gnc-numeric-convert amt scu GNC-HOW-RND-ROUND))

            (gnc:html-chart-set-type! chart 'pie)

            (gnc:html-chart-set-currency-iso!
              chart (gnc-commodity-get-mnemonic report-currency))
            (gnc:html-chart-set-currency-symbol!
              chart (gnc-commodity-get-nice-symbol report-currency))

            (gnc:html-chart-set-title!
              chart (list report-title
                          (string-append
                            (if do-intervals?
                              (format #f
                                      (G_ "~a to ~a")
                                      (qof-print-date from-date)
                                      (qof-print-date to-date))
                              (format #f
                                      (G_ "Balance at ~a")
                                      (qof-print-date to-date)))
                            (if show-total?
                              (let ((total (apply + (unzip1 combined))))
                                (format
                                  #f ": ~a"
                                  (gnc:monetary->string
                                    (gnc:make-gnc-monetary
                                      report-currency
                                      (round-scu total)))))
                              ""))))
            (gnc:html-chart-set-width! chart width)
            (gnc:html-chart-set-height! chart height)
            (gnc:html-chart-add-data-series! chart
                                             (G_ "Accounts")
                                             (map round-scu (unzip1 combined))
                                             (gnc:assign-colors (length combined))
                                             'urls urls)
            (gnc:html-chart-set-axes-display! chart #f)

            (gnc:html-chart-set-data-labels!
              chart
              (map
                (lambda (series)
                  (string-append
                    (cadr series)
                    (if show-total?
                      (string-append
                        " - "
                        (gnc:monetary->string
                          (gnc:make-gnc-monetary
                            report-currency
                            (round-scu (car series)))))
                      "")
                    (if show-percent?
                      (format #f " (~2,1f%)"
                              (* 100 (/ (car series)
                                        (apply + (unzip1 combined)))))
                      "")))
                combined))

            (gnc:html-document-add-object! document chart)

            ;; Tags: Show which accounts contributed to which tag-values
            (when show-accounts?
              (let* ((grouped-accounts (hash-table->grouped-accounts grouped-hash-table))
                     (groups (map car grouped-accounts))
                     (table (gnc:make-html-table)))
                (gnc:html-table-set-style!
                  table "th"
                  'attribute '("align" "left"))
                (gnc:html-table-set-col-headers! table groups)
                (gnc:html-table-append-row!
                  table
                  (map
                    (lambda (group)
                      (apply
                        string-append
                        (sort
                          (map
                            (lambda (ga)
                              (string-append
                                (if show-fullname?
                                  (gnc-account-get-full-name ga)
                                  (xaccAccountGetName ga))
                                "<br/>"))
                            (cadr (assv group grouped-accounts)))
                          (lambda (a b)
                            (gnc:string-locale<? a b)))))
                    groups))
                (gnc:html-document-add-object! document table))))

          ;; else if (null? combined)
          (gnc:html-document-add-object!
            document
            (gnc:html-make-empty-data-warning
              report-title (gnc:report-id report-obj)))))

      ;; else if (null? accounts)
      (gnc:html-document-add-object!
        document
        (gnc:html-make-no-account-warning
          report-title (gnc:report-id report-obj))))

    (gnc:report-finished)
    document))

(export tag-piechart-options)
(export tag-piechart-renderer)
