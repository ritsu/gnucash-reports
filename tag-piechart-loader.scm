(define-module (gnucash report tag-piechart-loader))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))

; Load the renderer module once at the top level to get the symbols
(load (gnc-build-userdata-path "ritsu-reports/tag-piechart-renderer.scm"))
(use-modules (gnucash report tag-piechart-renderer))

(debug-enable 'backtrace)

(define menuname-income (N_ "Tagged Income Piechart"))
(define menuname-expense (N_ "Tagged Expense Piechart"))
(define menuname-assets (N_ "Tagged Asset Piechart"))
(define menuname-securities (N_ "Tagged Security Piechart"))
(define menuname-liabilities (N_ "Tagged Liability Piechart"))
;; The names are used in the menu

;; The menu statusbar tips.
(define menutip-income
    (N_ "Shows a tagged piechart with the Income per given time interval"))
(define menutip-expense 
    (N_ "Shows a tagged piechart with the Expenses per given time interval"))
(define menutip-assets 
    (N_ "Shows a tagged piechart with the Assets balance at a given time"))
(define menutip-securities
    (N_ "Shows a tagged piechart with distribution of assets over securities"))
(define menutip-liabilities 
    (N_ "Shows a tagged piechart with the Liabilities \
        balance at a given time"))

;; The names here are used 1. for internal identification, 2. as
;; tab labels, 3. as default for the 'Report name' option which
;; in turn is used for the printed report title.
(define reportname-income (N_ "[T] Income Accounts"))
(define reportname-expense (N_ "[T] Expense Accounts"))
(define reportname-assets (N_ "[T] Assets"))
(define reportname-securities (N_ "[T] Securities"))
(define reportname-liabilities (N_ "[T] Liabilities"))

; TODO: generate proper uuids
(define tag-piechart-income-uuid "tagpiechartincomeuuid")
(define tag-piechart-expense-uuid "tagpiechartexpenseuuid")
(define tag-piechart-asset-uuid "tagpiechartassetuuid")
(define tag-piechart-securities-uuid "tagpiechartsecuritiesuuid")
(define tag-piechart-liabilities-uuid "tagpiechartliabilitiesuuid")

; Get and reload the module
(define (reload-report-module)
  (reload-module (resolve-module '(gnucash report tag-piechart-renderer))))

; Every time options are generated, reload the meat of the report
(define (options-generator account-types reverse-balance? do-intervals?)
  (reload-report-module)
  (tag-piechart-options account-types reverse-balance? do-intervals?))

; Every time report is rendered, reload the meat of the report
(define (piechart-renderer report-obj reportname report-guid
                           account-types do-intervals?
                           display-name sort-comparator get-data)
  (reload-report-module)
  (tag-piechart-renderer report-obj reportname report-guid
                           account-types do-intervals?
                           display-name sort-comparator get-data))

;; Get display name for account-based reports.
(define (display-name-accounts show-fullname? acc)
  ((if show-fullname?
       gnc-account-get-full-name
       xaccAccountGetName) acc))

;; Sort comparator for account-based reports.
(define (sort-comparator-accounts sort-method show-fullname?)
  (cond
   ((eq? sort-method 'acct-code)
    (lambda (a b)
      (gnc:string-locale<? (xaccAccountGetCode (cadr a))
                           (xaccAccountGetCode (cadr b)))))
   ((eq? sort-method 'alphabetical)
    (lambda (a b)
      (gnc:string-locale<? (display-name-accounts show-fullname? (cadr a))
                           (display-name-accounts show-fullname? (cadr b)))))
   (else
    (lambda (a b) (> (car a) (car b))))))

(define (build-report!
          name acct-types income-expense? menuname menutip
          reverse-balance? uuid)
  (gnc:define-report
    'version 1
    'name name
    'report-guid uuid
    'menu-path (if income-expense?
                   (list gnc:menuname-income-expense)
                   (list gnc:menuname-asset-liability))
    'menu-name menuname
    'menu-tip menutip
    'options-generator (lambda () (options-generator acct-types
                                                     reverse-balance?
                                                     income-expense?))
    'renderer (lambda (report-obj)
                (piechart-renderer report-obj name uuid
                                   acct-types income-expense?
                                   display-name-accounts
                                   sort-comparator-accounts
                                   traverse-accounts))))

;; Tags: Similar to traverse-accounts, except balances are not
;; calculated and stored for individual accounts. Instead, balances
;; are added to a hash table grouped by tag values.
(define (traverse-accounts account-balance show-acct? work-to-do
                           work-done current-depth accts)
  (define table (make-hash-table))
  (define (add-tagged-account! account balance)
    (for-each
      (lambda (v)
        (let* ((handle (hash-create-handle! table v (list '() 0)))
               (val (cdr handle)))
          (hash-set! table v
                     (list (cons account (car val))
                           (+ (cadr val) balance)))))
      (account->tag-values account 0)))

  (define (traverse! remaining initial-work)
    (if (null? remaining)
      initial-work
      (let* ((cur (car remaining))
             (tail (cdr remaining))
             (cur-work-done (1+ initial-work))
             (subaccts (gnc-account-get-children cur)))
        (gnc:report-percent-done (* 100 (/ cur-work-done (work-to-do))))
        (if (show-acct? cur)
          (let ((balance (account-balance account #f)))
            (if (not (zero? balance))
              (add-tagged-account! cur balance))))
        (traverse! tail (traverse! subaccts cur-work-done)))))

  (let ((final-work (traverse! accts work-done)))
    (cons final-work (table))))

(build-report!
  reportname-income
  (list ACCT-TYPE-INCOME)
  #t
  menuname-income menutip-income
  (lambda (x) #t)
  tag-piechart-income-uuid)

(build-report!
  reportname-expense
  (list ACCT-TYPE-EXPENSE)
  #t
  menuname-expense menutip-expense
  (lambda (x) #f)
  tag-piechart-expense-uuid)

(build-report!
  reportname-assets
  (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CHECKING
        ACCT-TYPE-SAVINGS ACCT-TYPE-MONEYMRKT
        ACCT-TYPE-RECEIVABLE ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
        ACCT-TYPE-CURRENCY)
  #f
  menuname-assets menutip-assets
  (lambda (x) #f)
  tag-piechart-asset-uuid)

(build-report!
  reportname-liabilities
  (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-CREDIT
        ACCT-TYPE-CREDITLINE)
  #f
  menuname-liabilities menutip-liabilities
  (lambda (x) #t)
  tag-piechart-liabilities-uuid)

