;;;; package.lisp

(defpackage #:check-json
  (:use #:cl)
  (:export :obj-type-errors
           :error-template
           :render-error-template
           :invalid-template-key
           :print-json-path
           :error-report
           :validate-json))

(defpackage #:json-bool
  (:export :true
           :false))
