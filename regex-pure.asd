;;;; regex-pure.asd - Pure Common Lisp regular expression library
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(asdf:defsystem #:regex-pure
  :description "Pure Common Lisp regular expression library with cl-ppcre-compatible API"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "conditions")
                             (:file "charset")
                             (:file "parser")
                             (:file "compiler")
                             (:file "matcher")
                             (:file "api")))))
