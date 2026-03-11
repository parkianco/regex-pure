;;;; matcher.lisp - Regex matching engine
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:regex-pure)

;;; Match state

(defstruct match-state
  (target "" :type string)
  (pos 0 :type fixnum)
  (groups nil :type simple-vector)  ; Vector of (start . end) pairs
  )

(defun make-match (target group-count)
  (make-match-state :target target
                    :groups (make-array (1+ group-count) :initial-element nil)))

(defun record-group (state index start end)
  (setf (aref (match-state-groups state) index) (cons start end)))

;;; Core matching

(defun match-node (node state)
  "Try to match NODE at current position in STATE. Returns new position or NIL."
  (etypecase node
    (re-literal (match-literal node state))
    (re-charset (match-charset node state))
    (re-sequence (match-sequence node state))
    (re-alternation (match-alternation node state))
    (re-repetition (match-repetition node state))
    (re-group (match-group node state))
    (re-anchor (match-anchor node state))
    (re-backreference (match-backreference node state))))

(defun match-literal (node state)
  (let ((target (match-state-target state))
        (pos (match-state-pos state)))
    (when (and (< pos (length target))
               (char= (char target pos) (re-literal-char node)))
      (setf (match-state-pos state) (1+ pos))
      (1+ pos))))

(defun match-charset (node state)
  (let ((target (match-state-target state))
        (pos (match-state-pos state)))
    (when (and (< pos (length target))
               (charset-contains-p (re-charset-charset node)
                                  (char target pos)))
      (setf (match-state-pos state) (1+ pos))
      (1+ pos))))

(defun match-sequence (node state)
  (let ((elements (re-sequence-elements node)))
    (if (null elements)
        (match-state-pos state)
        (loop for elem in elements
              always (match-node elem state)
              finally (return (match-state-pos state))))))

(defun match-alternation (node state)
  (let ((saved-pos (match-state-pos state)))
    (dolist (branch (re-alternation-branches node))
      (setf (match-state-pos state) saved-pos)
      (when (match-node branch state)
        (return (match-state-pos state))))
    nil))

(defun match-repetition (node state)
  (let ((element (re-repetition-element node))
        (min (re-repetition-min node))
        (max (re-repetition-max node))
        (greedy (re-repetition-greedy node))
        (count 0)
        (positions (list (match-state-pos state))))
    ;; Match as many as possible
    (loop while (and (or (null max) (< count max))
                     (match-node element state))
          do (incf count)
             (push (match-state-pos state) positions))
    ;; Check minimum
    (when (< count min)
      (return-from match-repetition nil))
    ;; For greedy, we already have max matches
    ;; For non-greedy, try from minimum
    (if greedy
        (match-state-pos state)
        (progn
          ;; Reset to minimum matches
          (setf (match-state-pos state) (nth (- count min) (reverse positions)))
          (match-state-pos state)))))

(defun match-group (node state)
  (let ((start (match-state-pos state))
        (element (re-group-element node))
        (index (re-group-index node)))
    (when (match-node element state)
      (when index
        (record-group state index start (match-state-pos state)))
      (match-state-pos state))))

(defun match-anchor (node state)
  (let ((target (match-state-target state))
        (pos (match-state-pos state)))
    (case (re-anchor-type node)
      (:start (when (= pos 0) pos))
      (:end (when (= pos (length target)) pos))
      (:word-boundary
       (let ((before (and (> pos 0)
                         (charset-contains-p *charset-word* (char target (1- pos)))))
             (after (and (< pos (length target))
                        (charset-contains-p *charset-word* (char target pos)))))
         (when (not (eq before after)) pos))))))

(defun match-backreference (node state)
  (let* ((index (re-backreference-index node))
         (group-match (aref (match-state-groups state) index)))
    (when group-match
      (let* ((gstart (car group-match))
             (gend (cdr group-match))
             (matched (subseq (match-state-target state) gstart gend))
             (target (match-state-target state))
             (pos (match-state-pos state))
             (len (length matched)))
        (when (and (<= (+ pos len) (length target))
                   (string= matched (subseq target pos (+ pos len))))
          (setf (match-state-pos state) (+ pos len))
          (+ pos len))))))

;;; Main match function

(defun try-match (regex target start)
  "Try to match REGEX in TARGET starting at START. Returns (values match-end groups) or NIL."
  (let* ((rx (ensure-regex regex))
         (state (make-match target (regex-group-count rx))))
    (setf (match-state-pos state) start)
    (record-group state 0 start start) ; Group 0 is whole match
    (when (match-node (regex-ast rx) state)
      ;; Update group 0 end
      (setf (cdr (aref (match-state-groups state) 0)) (match-state-pos state))
      (values (match-state-pos state)
              (match-state-groups state)))))
