;;;; api.lisp - Public regex API
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:regex-pure)

;;; Core API

(defun scan (regex target &key (start 0) end)
  "Scan TARGET for REGEX starting at START.
Returns (values match-start match-end reg-starts reg-ends) or NIL."
  (let* ((rx (ensure-regex regex))
         (end (or end (length target))))
    (loop for pos from start below end
          do (multiple-value-bind (match-end groups)
                 (try-match rx target pos)
               (when match-end
                 (let* ((group-count (regex-group-count rx))
                        (reg-starts (make-array group-count))
                        (reg-ends (make-array group-count)))
                   (loop for i from 1 to group-count
                         for g = (aref groups i)
                         do (when g
                              (setf (aref reg-starts (1- i)) (car g))
                              (setf (aref reg-ends (1- i)) (cdr g))))
                   (return (values pos match-end reg-starts reg-ends))))))))

(defun regex-replace (regex target replacement &key (start 0) end)
  "Replace first match of REGEX in TARGET with REPLACEMENT."
  (let ((end (or end (length target))))
    (multiple-value-bind (match-start match-end reg-starts reg-ends)
        (scan regex target :start start :end end)
      (if match-start
          (let ((result (make-string-output-stream)))
            (write-string (subseq target 0 match-start) result)
            (write-replacement replacement target reg-starts reg-ends result)
            (write-string (subseq target match-end) result)
            (get-output-stream-string result))
          target))))

(defun regex-replace-all (regex target replacement &key (start 0) end)
  "Replace all matches of REGEX in TARGET with REPLACEMENT."
  (let* ((rx (ensure-regex regex))
         (end (or end (length target)))
         (result (make-string-output-stream))
         (pos start))
    (loop
      (multiple-value-bind (match-start match-end reg-starts reg-ends)
          (scan rx target :start pos :end end)
        (unless match-start
          (write-string (subseq target pos) result)
          (return (get-output-stream-string result)))
        (write-string (subseq target pos match-start) result)
        (write-replacement replacement target reg-starts reg-ends result)
        (setf pos (max match-end (1+ match-start)))))))

(defun write-replacement (replacement target reg-starts reg-ends stream)
  "Write REPLACEMENT to STREAM, handling backreferences."
  (let ((i 0)
        (len (length replacement)))
    (loop while (< i len)
          do (let ((c (char replacement i)))
               (cond
                 ((and (char= c #\\) (< (1+ i) len))
                  (let ((next (char replacement (1+ i))))
                    (cond
                      ((digit-char-p next)
                       (let ((idx (digit-char-p next)))
                         (when (and (< (1- idx) (length reg-starts))
                                    (aref reg-starts (1- idx)))
                           (write-string (subseq target
                                                (aref reg-starts (1- idx))
                                                (aref reg-ends (1- idx)))
                                        stream)))
                       (incf i 2))
                      ((char= next #\\)
                       (write-char #\\ stream)
                       (incf i 2))
                      (t
                       (write-char c stream)
                       (incf i)))))
                 (t
                  (write-char c stream)
                  (incf i)))))))

(defun split (regex target &key (start 0) end limit)
  "Split TARGET by REGEX matches."
  (let* ((rx (ensure-regex regex))
         (end (or end (length target)))
         (result nil)
         (pos start)
         (count 0))
    (loop
      (when (and limit (>= count (1- limit)))
        (push (subseq target pos end) result)
        (return (nreverse result)))
      (multiple-value-bind (match-start match-end)
          (scan rx target :start pos :end end)
        (unless match-start
          (push (subseq target pos end) result)
          (return (nreverse result)))
        (push (subseq target pos match-start) result)
        (incf count)
        (setf pos (max match-end (1+ pos)))))))

(defun all-matches (regex target &key (start 0) end)
  "Return list of all match positions as (start . end) pairs."
  (let* ((rx (ensure-regex regex))
         (end (or end (length target)))
         (result nil)
         (pos start))
    (loop
      (multiple-value-bind (match-start match-end)
          (scan rx target :start pos :end end)
        (unless match-start
          (return (nreverse result)))
        (push (cons match-start match-end) result)
        (setf pos (max match-end (1+ pos)))))))

(defun all-matches-as-strings (regex target &key (start 0) end)
  "Return list of all matched substrings."
  (mapcar (lambda (pair)
            (subseq target (car pair) (cdr pair)))
          (all-matches regex target :start start :end end)))

;;; Convenience macros

(defmacro register-groups-bind (vars (regex target &key start end) &body body)
  "Bind register groups to VARS and execute BODY if match found."
  (let ((match-start (gensym "MATCH-START"))
        (match-end (gensym "MATCH-END"))
        (reg-starts (gensym "REG-STARTS"))
        (reg-ends (gensym "REG-ENDS"))
        (target-var (gensym "TARGET")))
    `(let ((,target-var ,target))
       (multiple-value-bind (,match-start ,match-end ,reg-starts ,reg-ends)
           (scan ,regex ,target-var :start ,(or start 0) :end ,end)
         (declare (ignore ,match-start ,match-end))
         (when ,reg-starts
           (let ,(loop for var in vars
                       for i from 0
                       collect `(,var (when (aref ,reg-starts ,i)
                                       (subseq ,target-var
                                               (aref ,reg-starts ,i)
                                               (aref ,reg-ends ,i)))))
             ,@body))))))

(defmacro do-matches ((match-start match-end regex target &key start end) &body body)
  "Iterate over all matches of REGEX in TARGET."
  (let ((rx (gensym "RX"))
        (tgt (gensym "TGT"))
        (pos (gensym "POS"))
        (e (gensym "END")))
    `(let* ((,rx (ensure-regex ,regex))
            (,tgt ,target)
            (,pos ,(or start 0))
            (,e ,(or end `(length ,tgt))))
       (loop
         (multiple-value-bind (,match-start ,match-end)
             (scan ,rx ,tgt :start ,pos :end ,e)
           (unless ,match-start (return))
           ,@body
           (setf ,pos (max ,match-end (1+ ,pos))))))))

(defmacro do-register-groups (vars (regex target &key start end) &body body)
  "Iterate over all matches, binding register groups."
  (let ((match-start (gensym "MATCH-START"))
        (match-end (gensym "MATCH-END"))
        (reg-starts (gensym "REG-STARTS"))
        (reg-ends (gensym "REG-ENDS"))
        (rx (gensym "RX"))
        (tgt (gensym "TGT"))
        (pos (gensym "POS"))
        (e (gensym "END")))
    `(let* ((,rx (ensure-regex ,regex))
            (,tgt ,target)
            (,pos ,(or start 0))
            (,e ,(or end `(length ,tgt))))
       (loop
         (multiple-value-bind (,match-start ,match-end ,reg-starts ,reg-ends)
             (scan ,rx ,tgt :start ,pos :end ,e)
           (unless ,match-start (return))
           (let ,(loop for var in vars
                       for i from 0
                       collect `(,var (when (aref ,reg-starts ,i)
                                       (subseq ,tgt
                                               (aref ,reg-starts ,i)
                                               (aref ,reg-ends ,i)))))
             ,@body)
           (setf ,pos (max ,match-end (1+ ,pos))))))))
