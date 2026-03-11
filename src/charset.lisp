;;;; charset.lisp - Character set handling
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:regex-pure)

;;; Character classes as bit vectors for ASCII, hash-table for unicode

(defstruct charset
  (ascii (make-array 128 :element-type 'bit :initial-element 0) :type simple-bit-vector)
  (unicode nil :type (or null hash-table))
  (negated nil :type boolean))

(defun make-empty-charset ()
  (make-charset))

(defun charset-add-char (cs char)
  "Add CHAR to charset CS."
  (let ((code (char-code char)))
    (if (< code 128)
        (setf (aref (charset-ascii cs) code) 1)
        (progn
          (unless (charset-unicode cs)
            (setf (charset-unicode cs) (make-hash-table)))
          (setf (gethash code (charset-unicode cs)) t)))))

(defun charset-add-range (cs from to)
  "Add character range FROM-TO to charset CS."
  (loop for code from (char-code from) to (char-code to)
        do (charset-add-char cs (code-char code))))

(defun charset-contains-p (cs char)
  "Return T if charset CS contains CHAR."
  (let* ((code (char-code char))
         (found (if (< code 128)
                    (= 1 (aref (charset-ascii cs) code))
                    (and (charset-unicode cs)
                         (gethash code (charset-unicode cs))))))
    (if (charset-negated cs)
        (not found)
        found)))

(defun charset-negate (cs)
  "Return negated copy of charset CS."
  (let ((new (copy-charset cs)))
    (setf (charset-negated new) (not (charset-negated cs)))
    new))

;;; Predefined character classes

(defvar *charset-digit*
  (let ((cs (make-charset)))
    (charset-add-range cs #\0 #\9)
    cs))

(defvar *charset-word*
  (let ((cs (make-charset)))
    (charset-add-range cs #\a #\z)
    (charset-add-range cs #\A #\Z)
    (charset-add-range cs #\0 #\9)
    (charset-add-char cs #\_)
    cs))

(defvar *charset-space*
  (let ((cs (make-charset)))
    (charset-add-char cs #\Space)
    (charset-add-char cs #\Tab)
    (charset-add-char cs #\Newline)
    (charset-add-char cs #\Return)
    (charset-add-char cs #\Page)
    cs))

(defvar *charset-any*
  (let ((cs (make-charset)))
    ;; Match any except newline
    (loop for code from 0 below 128
          unless (= code (char-code #\Newline))
          do (setf (aref (charset-ascii cs) code) 1))
    (setf (charset-negated cs) nil)
    cs))
