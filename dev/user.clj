(ns user ; user ns is loaded by REPL startup
  (:require [hyperfiddle.rcf]
            #_[portal.api :as p]))

(hyperfiddle.rcf/enable!)

(comment 
(def p (p/open))

(add-tap #'p/submit)

(tap> :hello))

(comment
  ;; for node and jvm
  (require '[portal.console :as c]) ; in file to allow for various helpful macros. 

;; for web
;; NOTE: you might need to enable popups for the portal ui to work in the
;; browser.
  (require '[portal.web :as p])


  (def p (p/open)) ; Open a new inspector

;; or with an extension installed, do:
  (def p (p/open {:launcher :vs-code}))  ; jvm / node only
  (def p (p/open {:launcher :intellij})) ; jvm / node only

  (add-tap #'p/submit) ; Add portal as a tap> target

  (tap> :hello) ; Start tapping out values

  (p/clear) ; Clear all values

  (tap> :world) ; Tap out more values

  (prn @p) ; bring selected value back into repl

  (remove-tap #'p/submit) ; Remove portal from tap> targetset

  (p/close) ; Close the inspector when done
  )

(comment
  (require '[criterium.core :as cc])

  (bench (Thread/sleep 1000))
;;  =>
;;                    Execution time mean : 1.000803 sec
;;           Execution time std-deviation : 328.501853 us
;;          Execution time lower quantile : 1.000068 sec ( 2.5%)
;;          Execution time upper quantile : 1.001186 sec (97.5%)
;; By default bench is quiet about its progress. Run with-progress-reporting to get progress information on *out*.

  (with-progress-reporting (bench (Thread/sleep 1000) :verbose))
  (with-progress-reporting (quick-bench (Thread/sleep 1000) :verbose))

  ;; Lower level functions are available, that separate benchmark statistic generation and reporting.

  (report-result (benchmark (Thread/sleep 1000) {:verbose true}))
  (report-result (quick-benchmark (Thread/sleep 1000)))

  )