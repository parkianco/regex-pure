;;;; parser.lisp - Regex pattern parser
;;;; Copyright (c) 2024-2026 Parkian Company LLC
;;;; License: BSD-3-Clause

(in-package #:regex-pure)

;;; AST node types

(defstruct re-node)

(defstruct (re-literal (:include re-node))
  (char nil :type character))

(defstruct (re-charset (:include re-node))
  (charset nil :type charset))

(defstruct (re-sequence (:include re-node))
  (elements nil :type list))

(defstruct (re-alternation (:include re-node))
  (branches nil :type list))

(defstruct (re-repetition (:include re-node))
  (element nil :type re-node)
  (min 0 :type fixnum)
  (max nil :type (or null fixnum))  ; nil = unlimited
  (greedy t :type boolean))

(defstruct (re-group (:include re-node))
  (element nil :type re-node)
  (capturing t :type boolean)
  (name nil :type (or null string))
  (index nil :type (or null fixnum)))

(defstruct (re-anchor (:include re-node))
  (type nil :type (member :start :end :word-boundary)))

(defstruct (re-backreference (:include re-node))
  (index nil :type fixnum))

;;; Parser state

(defstruct regex-parser
  (pattern "" :type string)
  (pos 0 :type fixnum)
  (group-count 0 :type fixnum)
  (group-names nil :type list))

(defun parser-peek (p)
  (when (< (regex-parser-pos p) (length (regex-parser-pattern p)))
    (char (regex-parser-pattern p) (regex-parser-pos p))))

(defun parser-read (p)
  (when (< (regex-parser-pos p) (length (regex-parser-pattern p)))
    (prog1 (char (regex-parser-pattern p) (regex-parser-pos p))
      (incf (regex-parser-pos p)))))

(defun parser-expect (p char)
  (let ((c (parser-read p)))
    (unless (and c (char= c char))
      (syntax-error (regex-parser-pos p) "Expected '~C'" char))))

;;; Main parser

(defun parse-pattern (pattern)
  "Parse PATTERN string into regex AST."
  (let ((p (make-regex-parser :pattern pattern)))
    (values (parse-alternation p)
            (regex-parser-group-count p)
            (regex-parser-group-names p))))

(defun parse-alternation (p)
  "Parse alternation (a|b|c)."
  (let ((branches (list (parse-sequence p))))
    (loop while (and (parser-peek p) (char= (parser-peek p) #\|))
          do (parser-read p)
             (push (parse-sequence p) branches))
    (if (= 1 (length branches))
        (first branches)
        (make-re-alternation :branches (nreverse branches)))))

(defun parse-sequence (p)
  "Parse sequence of atoms."
  (let ((elements nil))
    (loop while (and (parser-peek p)
                     (not (member (parser-peek p) '(#\) #\|))))
          do (push (parse-quantified p) elements))
    (cond
      ((null elements) (make-re-sequence :elements nil))
      ((= 1 (length elements)) (first elements))
      (t (make-re-sequence :elements (nreverse elements))))))

(defun parse-quantified (p)
  "Parse atom with optional quantifier."
  (let ((element (parse-atom p))
        (c (parser-peek p)))
    (cond
      ((null c) element)
      ((char= c #\*)
       (parser-read p)
       (make-re-repetition :element element :min 0 :max nil
                          :greedy (not (eql (parser-peek p) #\?))))
      ((char= c #\+)
       (parser-read p)
       (make-re-repetition :element element :min 1 :max nil
                          :greedy (not (eql (parser-peek p) #\?))))
      ((char= c #\?)
       (parser-read p)
       (make-re-repetition :element element :min 0 :max 1
                          :greedy (not (eql (parser-peek p) #\?))))
      ((char= c #\{)
       (parse-counted-repetition p element))
      (t element))))

(defun parse-counted-repetition (p element)
  "Parse {n}, {n,}, {n,m} quantifier."
  (parser-read p) ; consume {
  (let ((min (parse-number p))
        (max nil))
    (when (eql (parser-peek p) #\,)
      (parser-read p)
      (if (digit-char-p (or (parser-peek p) #\Space))
          (setf max (parse-number p))
          (setf max nil))) ; {n,} = n or more
    (unless max (setf max min)) ; {n} = exactly n
    (parser-expect p #\})
    (make-re-repetition :element element :min min :max max
                       :greedy (not (eql (parser-peek p) #\?)))))

(defun parse-number (p)
  "Parse integer from pattern."
  (let ((start (regex-parser-pos p)))
    (loop while (and (parser-peek p) (digit-char-p (parser-peek p)))
          do (parser-read p))
    (parse-integer (regex-parser-pattern p) :start start :end (regex-parser-pos p))))

(defun parse-atom (p)
  "Parse single atom."
  (let ((c (parser-peek p)))
    (cond
      ((null c) (syntax-error (regex-parser-pos p) "Unexpected end of pattern"))
      ((char= c #\() (parse-group p))
      ((char= c #\[) (parse-bracket-class p))
      ((char= c #\.) (parser-read p) (make-re-charset :charset *charset-any*))
      ((char= c #\^) (parser-read p) (make-re-anchor :type :start))
      ((char= c #\$) (parser-read p) (make-re-anchor :type :end))
      ((char= c #\\) (parse-escape p))
      ((member c '(#\* #\+ #\? #\{ #\} #\) #\|))
       (syntax-error (regex-parser-pos p) "Unexpected '~C'" c))
      (t (parser-read p) (make-re-literal :char c)))))

(defun parse-group (p)
  "Parse group (...) or (?:...) or (?<name>...)."
  (parser-read p) ; consume (
  (let ((capturing t)
        (name nil))
    ;; Check for group modifiers
    (when (eql (parser-peek p) #\?)
      (parser-read p)
      (case (parser-peek p)
        (#\: (parser-read p) (setf capturing nil))
        (#\< (parser-read p) (setf name (parse-group-name p)))
        (t (syntax-error (regex-parser-pos p) "Unknown group modifier"))))
    (let ((index (when capturing
                   (incf (regex-parser-group-count p))
                   (regex-parser-group-count p))))
      (when name
        (push (cons name index) (regex-parser-group-names p)))
      (let ((element (parse-alternation p)))
        (parser-expect p #\))
        (make-re-group :element element :capturing capturing :name name :index index)))))

(defun parse-group-name (p)
  "Parse named group name."
  (let ((start (regex-parser-pos p)))
    (loop while (and (parser-peek p)
                     (not (char= (parser-peek p) #\>)))
          do (parser-read p))
    (prog1 (subseq (regex-parser-pattern p) start (regex-parser-pos p))
      (parser-expect p #\>))))

(defun parse-escape (p)
  "Parse escape sequence."
  (parser-read p) ; consume \
  (let ((c (parser-read p)))
    (case c
      (#\d (make-re-charset :charset *charset-digit*))
      (#\D (make-re-charset :charset (charset-negate *charset-digit*)))
      (#\w (make-re-charset :charset *charset-word*))
      (#\W (make-re-charset :charset (charset-negate *charset-word*)))
      (#\s (make-re-charset :charset *charset-space*))
      (#\S (make-re-charset :charset (charset-negate *charset-space*)))
      (#\b (make-re-anchor :type :word-boundary))
      (#\n (make-re-literal :char #\Newline))
      (#\r (make-re-literal :char #\Return))
      (#\t (make-re-literal :char #\Tab))
      ((#\\ #\. #\* #\+ #\? #\[ #\] #\( #\) #\{ #\} #\| #\^ #\$)
       (make-re-literal :char c))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (make-re-backreference :index (digit-char-p c)))
      (t (syntax-error (1- (regex-parser-pos p)) "Unknown escape \\~C" c)))))

(defun parse-bracket-class (p)
  "Parse character class [...]."
  (parser-read p) ; consume [
  (let ((cs (make-charset))
        (negated (eql (parser-peek p) #\^)))
    (when negated (parser-read p))
    (loop while (and (parser-peek p) (not (char= (parser-peek p) #\])))
          do (let ((c (parser-read p)))
               (cond
                 ((char= c #\\)
                  (let ((escaped (parser-read p)))
                    (case escaped
                      (#\d (loop for code from (char-code #\0) to (char-code #\9)
                                 do (charset-add-char cs (code-char code))))
                      (#\w (progn
                             (charset-add-range cs #\a #\z)
                             (charset-add-range cs #\A #\Z)
                             (charset-add-range cs #\0 #\9)
                             (charset-add-char cs #\_)))
                      (#\s (progn
                             (charset-add-char cs #\Space)
                             (charset-add-char cs #\Tab)
                             (charset-add-char cs #\Newline)))
                      (#\n (charset-add-char cs #\Newline))
                      (#\r (charset-add-char cs #\Return))
                      (#\t (charset-add-char cs #\Tab))
                      (t (charset-add-char cs escaped)))))
                 ;; Range a-z
                 ((and (eql (parser-peek p) #\-)
                       (parser-peek p)
                       (not (char= (char (regex-parser-pattern p)
                                        (1+ (regex-parser-pos p)))
                                  #\])))
                  (parser-read p) ; consume -
                  (charset-add-range cs c (parser-read p)))
                 (t (charset-add-char cs c)))))
    (parser-expect p #\])
    (when negated
      (setf (charset-negated cs) t))
    (make-re-charset :charset cs)))
