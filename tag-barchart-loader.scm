(define-module (gnucash report tag-barchart-loader))

; Are these necessary?
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(gnc:module-load "gnucash/report/report-system" 0)

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-26))
(use-modules (ice-9 match))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

; Load the renderer module once at the top level to get the symbols
(load (gnc-build-userdata-path "ritsu-reports/tag-barchart-renderer.scm"))
(use-modules (gnucash report tag-barchart-renderer))

(debug-enable 'backtrace)

; Variables used in report creation loop
(define menuname-income (N_ "Tagged Income Chart"))
(define menuname-expense (N_ "Tagged Expense Chart"))
(define menuname-assets (N_ "Tagged Asset Chart"))
(define menuname-liabilities (N_ "Tagged Liability Chart"))

(define menutip-income
  (N_ "Shows a chart with the Income per interval \
developing over time"))
(define menutip-expense
  (N_ "Shows a chart with the Expenses per interval \
developing over time"))
(define menutip-assets
  (N_ "Shows a chart with the Assets developing over time"))
(define menutip-liabilities
  (N_ "Shows a chart with the Liabilities \
developing over time"))

(define reportname-income (N_ "[T] Income Over Time"))
(define reportname-expense (N_ "[T] Expense Over Time"))
(define reportname-assets (N_ "[T] Assets Over Time"))
(define reportname-liabilities (N_ "[T] Liabilities Over Time"))

; TODO: generate proper uuids
(define tag-barchart-income-uuid "tagbarchartincomeuuid")
(define tag-barchart-expense-uuid "tagbarchartexpenseuuid")
(define tag-barchart-asset-uuid "tagbarchartassetuuid")
(define tag-barchart-liability-uuid "tagbarchartliabilityuuid")

; Get and reload the module
(define (reload-report-module)
  (reload-module (resolve-module '(gnucash report tag-barchart-renderer))))

; Every time options are generated, reload the meat of the report
(define (options-generator account-types do-intervals?)
  (reload-report-module)
  (tag-barchart-options account-types do-intervals?))

; Every time report is rendered, reload the meat of the report
(define (document-renderer report-obj reportname reportguid account-types
                           do-intervals? reverse-bal? export-type)
  (reload-report-module)
  (tag-barchart-renderer report-obj reportname reportguid account-types
                         do-intervals? reverse-bal? export-type))
 
(for-each
 (match-lambda
   ((reportname account-types inc-exp? menuname menutip reverse-bal? uuid)
    (gnc:define-report
     'version 1
     'name reportname
     'report-guid uuid
     'menu-path (if inc-exp?
                    (list gnc:menuname-income-expense)
                    (list gnc:menuname-asset-liability))
     'menu-name menuname
     'menu-tip menutip
     'options-generator (lambda () (options-generator account-types inc-exp?))
     'export-types '(("CSV" . csv))
     'export-thunk (lambda (report-obj export-type filename)
                     (document-renderer
                      report-obj reportname uuid account-types inc-exp? reverse-bal?
                      export-type))
     'renderer (lambda (report-obj)
                 (document-renderer
                  report-obj reportname uuid account-types inc-exp? reverse-bal?
                  #f)))))
 (list
  (list reportname-income (list ACCT-TYPE-INCOME) #t menuname-income menutip-income #t tag-barchart-income-uuid)
  (list reportname-expense (list ACCT-TYPE-EXPENSE) #t menuname-expense menutip-expense #f tag-barchart-expense-uuid)
  (list reportname-assets
        (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-CHECKING
              ACCT-TYPE-SAVINGS ACCT-TYPE-MONEYMRKT
              ACCT-TYPE-RECEIVABLE ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL
              ACCT-TYPE-CURRENCY)
        #f menuname-assets menutip-assets #f tag-barchart-asset-uuid)
  (list reportname-liabilities
        (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-CREDIT
              ACCT-TYPE-CREDITLINE)
        #f menuname-liabilities menutip-liabilities #t tag-barchart-liability-uuid)))


