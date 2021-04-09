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
                           account-types do-intervals?)
  (reload-report-module)
  (tag-piechart-renderer report-obj reportname report-guid
                           account-types do-intervals?))


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
                                   acct-types income-expense?))))

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

