# regex-pure

Pure Common Lisp regular expression library with a cl-ppcre-compatible API.

## Features

- cl-ppcre-like API (scan, regex-replace, split, etc.)
- Basic regex syntax: . * + ? [] () | ^ $ \d \w \s
- Compile regex for reuse
- Named groups support
- Zero external dependencies

## Installation

```lisp
(asdf:load-system :regex-pure)
```

## Usage

```lisp
(use-package :regex-pure)

;; Simple matching
(scan "\\d+" "abc123def")
;; => 3, 6, #(), #()  ; start, end, reg-starts, reg-ends

;; Compiled regex (faster for repeated use)
(let ((re (compile-regex "\\w+")))
  (scan re "hello world"))

;; Replace first match
(regex-replace "\\d+" "abc123def456" "X")
;; => "abcXdef456"

;; Replace all matches
(regex-replace-all "\\d+" "abc123def456" "X")
;; => "abcXdefX"

;; Split string
(split "\\s+" "one two  three")
;; => ("one" "two" "three")

;; Register groups
(register-groups-bind (area code)
    ("\\((\\d{3})\\)\\s*(\\d{4})" "(555) 1234")
  (format nil "Area: ~A, Number: ~A" area code))
;; => "Area: 555, Number: 1234"

;; Named groups
(scan "(?<year>\\d{4})-(?<month>\\d{2})" "2024-12")
```

## Supported Syntax

| Pattern | Meaning |
|---------|---------|
| `.` | Any character (except newline) |
| `*` | Zero or more |
| `+` | One or more |
| `?` | Zero or one |
| `[]` | Character class |
| `[^]` | Negated character class |
| `()` | Capture group |
| `(?:)` | Non-capturing group |
| `(?<name>)` | Named group |
| `\|` | Alternation |
| `^` | Start of string |
| `$` | End of string |
| `\d` | Digit [0-9] |
| `\w` | Word char [a-zA-Z0-9_] |
| `\s` | Whitespace |
| `\D \W \S` | Negated versions |

## API

- `scan regex target` - Find first match
- `compile-regex pattern` - Compile pattern for reuse
- `regex-replace regex target replacement` - Replace first match
- `regex-replace-all regex target replacement` - Replace all matches
- `split regex target` - Split string by pattern
- `register-groups-bind vars (regex target) &body` - Bind capture groups

## License

BSD-3-Clause. Copyright (c) 2024-2026 Parkian Company LLC.
