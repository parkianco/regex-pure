;;;; package.lisp - regex-pure package definition
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(defpackage #:regex-pure
  (:use #:cl)
  (:export
   ;; Conditions
   #:regex-error
   #:regex-syntax-error
   #:regex-error-message
   #:regex-error-position

   ;; Core API
   #:compile-regex
   #:scan
   #:regex-replace
   #:regex-replace-all
   #:split
   #:all-matches
   #:all-matches-as-strings

   ;; Convenience macros
   #:register-groups-bind
   #:do-matches
   #:do-register-groups

   ;; Regex object
   #:regex
   #:regex-pattern
   #:regex-groups))
