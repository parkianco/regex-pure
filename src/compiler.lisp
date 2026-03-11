;;;; compiler.lisp - Regex compiler (AST to instructions)
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:regex-pure)

;;; Compiled regex structure

(defstruct regex
  (pattern "" :type string)
  (ast nil :type (or null re-node))
  (group-count 0 :type fixnum)
  (group-names nil :type list))

;;; Compilation

(defun compile-regex (pattern)
  "Compile PATTERN string into a reusable regex object."
  (multiple-value-bind (ast group-count group-names)
      (parse-pattern pattern)
    (make-regex :pattern pattern
                :ast ast
                :group-count group-count
                :group-names group-names)))

(defun ensure-regex (regex-or-string)
  "Ensure we have a compiled regex."
  (etypecase regex-or-string
    (regex regex-or-string)
    (string (compile-regex regex-or-string))))
