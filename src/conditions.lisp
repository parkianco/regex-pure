;;;; conditions.lisp - Regex error conditions
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:regex-pure)

(define-condition regex-error (error)
  ((message :initarg :message :reader regex-error-message)
   (position :initarg :position :initform nil :reader regex-error-position))
  (:report (lambda (c s)
             (format s "Regex error~@[ at position ~D~]: ~A"
                     (regex-error-position c)
                     (regex-error-message c)))))

(define-condition regex-syntax-error (regex-error)
  ()
  (:report (lambda (c s)
             (format s "Regex syntax error~@[ at position ~D~]: ~A"
                     (regex-error-position c)
                     (regex-error-message c)))))

(defun syntax-error (pos message &rest args)
  (error 'regex-syntax-error
         :position pos
         :message (apply #'format nil message args)))
