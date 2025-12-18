;;; org-transclusion-blocks.el --- Component-based transclusion framework -*- lexical-binding: t; -*-

;; Author: Gino Cornejo
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/org-transclusion-blocks
;; Keywords: hypermedia vc

;; Package-Version: 0.3.0
;; Package-Requires: ((emacs "29.1") (org-transclusion "1.4.0") (org "9.7"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Decomposes complex links into semantic header arguments with validation:
;;
;;     #+HEADER: :transclude-type my-type
;;     #+HEADER: :my-component value
;;     #+begin_src elisp
;;     #+end_src
;;
;; Direct links work without registration, including keywords:
;;
;;     #+HEADER: :transclude [[file:/path::search]]
;;     #+HEADER: :transclude-keywords ":lines 10-15 :only-contents"
;;     #+begin_quote
;;     #+end_quote
;;
;; Generic transclusion control headers work with all link types:
;;
;;     #+HEADER: :transclude [[file:/path/file.txt]]
;;     #+HEADER: :transclude-lines 10-20
;;
;;     #+HEADER: :transclude-type orgit-file
;;     #+HEADER: :orgit-repo ~/project
;;     #+HEADER: :orgit-file src/core.el
;;     #+HEADER: :transclude-thing defun
;;
;; Generic headers (:transclude-lines, :transclude-thing) are mutually
;; exclusive and take precedence over :transclude-keywords specifications.
;;
;; Variable substitution in headers:
;;
;; Transclusion headers support variable references via :var:
;;
;;     #+HEADER: :var repo="~/project"
;;     #+HEADER: :transclude-type orgit-file
;;     #+HEADER: :orgit-repo $repo
;;     #+HEADER: :orgit-file "src/core.el"
;;
;; Two reference patterns are supported:
;; - $varname - Explicit dollar-prefixed reference
;; - varname - Bare name (when value is exactly the variable name)
;;
;; Generic headers always support expansion:
;; - :transclude, :transclude-keywords, :transclude-lines, etc.
;;
;; Type-specific headers opt in via :expand-vars property:
;; - See `org-transclusion-blocks-register-type' documentation
;; - Use `org-transclusion-blocks-describe-type' to check support
;;
;; Babel control headers (:results, :session, etc.) are NEVER expanded
;; to avoid interfering with language backend variable handling.
;;
;; Org syntax escaping prevents markup collisions:
;;
;; Enabled by default for Org sources via
;; `org-transclusion-blocks-escape-org-sources'.  Override per-block with
;; :transclude-escape-org header or set file/subtree defaults via
;; #+PROPERTY: header-args.
;;
;; Converting existing transclusions:
;;
;; Existing #+transclude: keywords can be converted to header form:
;;
;;     M-x org-transclusion-blocks-convert-keyword-at-point
;;
;; Converts the keyword at point to equivalent header syntax. The
;; original line is preserved as a comment for reference.
;;
;; For batch conversion:
;;
;;     M-x org-transclusion-blocks-convert-keywords-in-buffer
;;
;; Validator composition utilities:
;;
;; - `org-transclusion-blocks-make-non-empty-validator'
;; - `org-transclusion-blocks-make-regexp-validator'
;; - `org-transclusion-blocks-make-predicate-validator'
;; - `org-transclusion-blocks-compose-validators'
;;
;; See `org-transclusion-blocks-register-type' for type registration.
;;
;; Commands:
;; - `org-transclusion-blocks-add' - Process block at point
;; - `org-transclusion-blocks-add-all' - Process all blocks in scope
;; - `org-transclusion-blocks-validate-current-block' - Test validators
;; - `org-transclusion-blocks-describe-type' - Show type documentation
;; - `org-transclusion-blocks-list-types' - List registered types
;;; Code:

(require 'org-transclusion)
(require 'org-element)
(require 'ob-core)
(require 'ol)
(require 'cl-lib)
(require 'org-transclusion-blocks-types)

;;;; Customization

(defgroup org-transclusion-blocks nil
  "Component-based transclusion framework for Org blocks."
  :group 'org-transclusion
  :prefix "org-transclusion-blocks-")

(defcustom org-transclusion-blocks-indicator-duration 2.0
  "Seconds to display success indicator after content update.

Shows checkmark overlay on updated block.
Set to 0 to disable indicator."
  :type 'number
  :group 'org-transclusion-blocks
  :package-version '(org-transclusion-blocks . "0.1.0"))

(defcustom org-transclusion-blocks-timestamp-property 'org-transclusion-blocks-fetched
  "Text property name for storing fetch timestamp.

Applied to transcluded content in block body.
Used for future refresh functionality."
  :type 'symbol
  :group 'org-transclusion-blocks
  :package-version '(org-transclusion-blocks . "0.1.0"))

(defcustom org-transclusion-blocks-show-interaction-warnings t
  "Whether to show warnings for component interactions.

When non-nil, display warnings for:
- Shadowed components
- Mode conflicts (mixed header forms)
- Soft conflicts (redundant specifications)
- Generic header conflicts

When nil, only hard conflicts (requirements, mutual exclusions,
incompatible generic headers) cause errors.

Hard conflicts always signal errors:
- Missing required components
- Mutually exclusive components present
- Both :transclude-lines and :transclude-thing specified

Soft conflicts generate warnings when this is non-nil:
- Shadowed component present
- :transclude-lines duplicates :lines in :transclude-keywords
- :transclude-thing duplicates :thing-at-point in keywords

Warnings appear in *Warnings* buffer and echo area."
  :type 'boolean
  :group 'org-transclusion-blocks
  :package-version '(org-transclusion-blocks . "0.2.0"))

(defcustom org-transclusion-blocks-escape-org-sources t
  "Whether to escape Org syntax when transcluding from Org files.

When non-nil (recommended), automatically escapes content transcluded
from links targeting Org files (file: links with .org extension,
id: links, etc.).

This prevents Org markup in source content from breaking src block
structure:
  - Headlines (*, **, etc.)
  - Keywords (#+BEGIN, #+PROPERTY, etc.)
  - Markup characters that start lines

When nil, content is inserted verbatim.  Use :transclude-escape-org
header to override per-block.

Does not affect non-Org sources (Python files, text files, etc.)."
  :type 'boolean
  :group 'org-transclusion-blocks
  :package-version '(org-transclusion-blocks . "0.2.0"))


;;;; Internal Variables

(defvar org-transclusion-blocks--last-fetch-time nil
  "Timestamp of most recent successful content fetch.

Buffer-local.
Used by `org-transclusion-blocks--show-indicator'.")
(make-variable-buffer-local 'org-transclusion-blocks--last-fetch-time)

(defvar-local org-transclusion-blocks--last-payload nil
  "Cached payload from most recent content fetch.
Used by metadata application to avoid re-fetching.")

(defvar org-transclusion-blocks-yank-excluded-properties
  '(org-transclusion-blocks-keyword
    org-transclusion-blocks-link
    org-transclusion-blocks-max-line
    org-transclusion-blocks-fetched)
  "Text properties excluded from yank operations.

These properties are implementation details for org-transclusion-blocks
and should not propagate when users copy transcluded content.

Unlike org-transclusion.el, we apply this exclusion permanently rather
than during activation/deactivation cycles, since block transclusions
are one-shot operations without a deactivation phase.")

(defvar-local org-transclusion-blocks--suppress-overlays nil
  "When non-nil, skip overlay creation during transclusion.

Set by transient menus during interactive range adjustment to
avoid expensive overlay operations and redisplay on every
incremental change.

Text properties are still applied immediately for metadata
tracking.  Overlays are created when this variable is nil or when
`org-transclusion-blocks--ensure-overlays-applied' is called
after transient exits.

Cleared by `org-transclusion-blocks--ensure-overlays-applied'.")

(defvar-local org-transclusion-blocks--undo-handle nil
  "Change group handle for transient menu undo consolidation.

Set by `org-transclusion-blocks-lines-menu' when entering the
transient menu.  Cleared by cleanup function on exit.

Used by `org-transclusion-blocks--apply-metadata' to determine
whether to suppress text property undo entries.  When non-nil,
text property changes are not recorded in undo list to prevent
undo corruption when undoing/redoing content changes.

See Info node `(elisp)Atomic Changes' for change group protocol.")

;;;; Variable Expansion Support
;;
;; Variable expansion is controlled via component metadata in type registry.
;; Generic transclusion headers have expansion enabled by default.
;; Type-specific component headers opt in via :expand-vars property.
;;
;; Expansion occurs before link construction in org-transclusion-blocks-add.
;;
;; CRITICAL: Only transclusion headers support expansion to avoid
;; interfering with Babel's own variable handling for language backends.

(defconst org-transclusion-blocks--generic-expandable-headers
  '(:transclude
    :transclude-keywords
    :transclude-lines
    :transclude-thing
    :transclude-escape-org
    :transclude-type)
  "Generic transclusion headers that support variable expansion.

These headers always support expansion regardless of :transclude-type.

Type-specific component headers opt in via :expand-vars property in
their component metadata.  See `org-transclusion-blocks-register-type'.")

(defun org-transclusion-blocks--header-expandable-p (key params)
  "Return non-nil if KEY header supports variable expansion.

KEY is header keyword symbol.
PARAMS is full parameter alist (used to check for :transclude-type).

Headers are expandable if:
1. KEY is in `org-transclusion-blocks--generic-expandable-headers', OR
2. KEY is a component header with :expand-vars t in its metadata

This prevents expansion of Babel control headers and language-specific
headers that might have their own variable handling.

Type authors control expansion per-component via :expand-vars property:

    (org-transclusion-blocks-register-type
     \\='my-type
     \\='(:component (:header :my-header
                    :expand-vars t))  ; Enable expansion
     #\\='my-constructor)"
  (or
   ;; Generic transclusion headers
   (memq key org-transclusion-blocks--generic-expandable-headers)

   ;; Type-specific component with :expand-vars t
   (when-let* ((type-raw (cdr (assq :transclude-type params)))
               (type (if (symbolp type-raw) type-raw
                       (intern (org-strip-quotes
                                (if (stringp type-raw) type-raw
                                  (format "%s" type-raw))))))
               (component-spec (alist-get type org-transclusion-blocks--type-components)))
     ;; Find component metadata for KEY and check :expand-vars
     (cl-loop for (_semantic-key meta) on component-spec by #'cddr
              when (eq key (plist-get meta :header))
              return (plist-get meta :expand-vars)))))

(defun org-transclusion-blocks--parse-var-headers (params)
  "Extract variable bindings from PARAMS.

PARAMS is alist of header arguments.

Returns alist of (NAME . VALUE) pairs suitable for variable expansion.

Parses :var headers in format \"name=value\" or (name . value).

Examples:
    (:var . \"repo=~/project\") -> ((repo . \"~/project\"))
    (:var . (repo . \"~/project\")) -> ((repo . \"~/project\"))"
  (let ((vars nil))
    (dolist (pair params)
      (when (eq (car pair) :var)
        (let ((var-spec (cdr pair)))
          (cond
           ;; Already parsed as cons cell
           ((consp var-spec)
            (push var-spec vars))

           ;; String format "name=value"
           ((stringp var-spec)
            (when (string-match "^\\([^=]+\\)=\\(.+\\)$" var-spec)
              (let ((name (intern (string-trim (match-string 1 var-spec))))
                    (value (string-trim (match-string 2 var-spec))))
                ;; Remove surrounding quotes if present
                (when (and (string-prefix-p "\"" value)
                           (string-suffix-p "\"" value))
                  (setq value (substring value 1 -1)))
                (push (cons name value) vars))))))))
    (nreverse vars)))

(defun org-transclusion-blocks--vector-to-link-string (vec)
  "Convert vector VEC to bracket link string.

VEC is a vector parsed by Babel from [[link]] syntax.
Babel parses [[link]] as (vector (vector \\='link-symbol)).

Returns string \"[[link]]\" with proper escaping preserved.

Examples:
  Input: [[file:path]]
  Babel: (vector (vector \\='file:path))
  Output: \"[[file:path]]\"

  Input: [[file:path::\\(defun]]
  Babel: (vector (vector \\='file:path::\\(defun))
  Output: \"[[file:path::\\(defun]]\""
  ;; Unwrap nested vectors to find the innermost element
  (let ((elem vec))
    (while (and (vectorp elem) (> (length elem) 0))
      (setq elem (aref elem 0)))

    ;; Convert innermost element to string and wrap in brackets
    (let ((link-content
           (cond
            ((symbolp elem) (symbol-name elem))
            ((stringp elem) elem)
            (t (format "%s" elem)))))
      (concat "[[" link-content "]]"))))

(defun org-transclusion-blocks--expand-header-vars (params)
  "Expand variable references in transclusion-related PARAMS.

PARAMS is alist of header arguments from `org-babel-get-src-block-info'.

Expands references ONLY in transclusion headers:
- `org-transclusion-blocks--generic-expandable-headers' (generic headers)
- Type-specific component headers with :expand-vars t

Does NOT expand:
- Babel control headers (`:results', `:exports', `:session', etc.)
- Language-specific headers (`:python', `:flags', etc.)
- `:var' definitions themselves
- Type-specific headers without :expand-vars property

This prevents interference with Babel language backends that have
their own variable handling.

Variable reference patterns:
- `$varname' - Explicit variable reference
- `varname' - Bare name (only if matches defined variable)
- `[[...$varname...]]' - Variable inside bracket link

Variables are resolved via `:var' header arguments.  String values are
substituted directly.  Non-string values are formatted via
`format \"%S\"'.

Returns new alist with expanded values.  Original PARAMS unchanged.

Example:

    #+HEADER: :var repo=\"~/project\"
    #+HEADER: :transclude [[file:$repo/file.org]]
    #+HEADER: :results output

After expansion, `:transclude' becomes \"[[file:~/project/file.org]]\".
`:results' remains \"output\" (not expanded)."
  (let ((vars (org-transclusion-blocks--parse-var-headers params))
        (expanded-params nil))

    (dolist (pair params)
      (let* ((key (car pair))
             (value (cdr pair))
             (expandable (org-transclusion-blocks--header-expandable-p key params)))

        ;; Determine final value
        (let ((final-value
               (cond
                ;; Non-expandable: keep original value unchanged
                ((not expandable)
                 value)

                ;; Expandable string: try expansion
                ((stringp value)
                 (if (string-empty-p value)
                     value
                   (org-transclusion-blocks--expand-value-vars value vars)))

                ;; Expandable vector: Babel parsed [[link]] as nested vector
                ((vectorp value)
                 (let ((link-str (org-transclusion-blocks--vector-to-link-string value)))
                   (org-transclusion-blocks--expand-value-vars link-str vars)))

                ;; Expandable symbol: convert to string, expand, keep as string
                ((symbolp value)
                 (let ((value-str (symbol-name value)))
                   (org-transclusion-blocks--expand-value-vars value-str vars)))

                ;; Expandable number: convert to string, expand
                ((numberp value)
                 (let ((value-str (number-to-string value)))
                   (org-transclusion-blocks--expand-value-vars value-str vars)))

                ;; Other expandable types: format and expand
                (t
                 (let ((value-str (format "%S" value)))
                   (org-transclusion-blocks--expand-value-vars value-str vars))))))

          (push (cons key final-value) expanded-params))))

    (nreverse expanded-params)))

(defun org-transclusion-blocks--expand-value-vars (value vars)
  "Expand variable references in VALUE string using VARS alist.

VALUE is header argument value string.
VARS is alist with (NAME . VALUE) pairs.

Supports three reference patterns:
- `$varname' - Explicit dollar-prefixed reference
- `varname' - Bare name matching entire value
- `[[...$varname...]]' - Variable reference inside bracket link

For bracket links, extracts content between [[ and ]], expands
variables within that content, then re-wraps in brackets.

Returns expanded string with variables substituted.
Non-string variable values are formatted via `format \"%S\"'.

If no variables match, returns VALUE unchanged.

Examples:

    VALUE: \"$repo/file.txt\"
    VARS: ((repo . \"~/project\"))
    Result: \"~/project/file.txt\"

    VALUE: \"[[file:$repo/file.org]]\"
    VARS: ((repo . \"~/project\"))
    Result: \"[[file:~/project/file.org]]\"

    VALUE: \"myvar\"
    VARS: ((myvar . \"expanded\"))
    Result: \"expanded\"

    VALUE: \"literal\"
    VARS: ((other . \"value\"))
    Result: \"literal\" (unchanged)"
  (message "[org-transclusion-blocks] expand-value-vars called with value: %S" value)
  (message "[org-transclusion-blocks] Available vars: %S" vars)

  (let ((result value))
    ;; Special handling for bracket links
    (if (and (string-prefix-p "[[" value)
             (string-suffix-p "]]" value))
        (progn
          (message "[org-transclusion-blocks] Detected bracket link")
          ;; Extract content between brackets
          (let* ((inner (substring value 2 -2))
                 (expanded-inner inner))

            (message "[org-transclusion-blocks] Inner content: %S" inner)

            ;; Try exact match on inner content (bare variable)
            (let ((exact-match (assoc (intern inner) vars)))
              (when exact-match
                (message "[org-transclusion-blocks] Found exact match for %S" inner)
                (let ((var-value (cdr exact-match)))
                  (setq expanded-inner (if (stringp var-value)
                                           var-value
                                         (format "%S" var-value))))))

            ;; Expand $varname references in inner content
            (dolist (var-pair vars)
              (let* ((var-name (symbol-name (car var-pair)))
                     (var-value (cdr var-pair))
                     (pattern (concat "\\$" (regexp-quote var-name))))
                (message "[org-transclusion-blocks] Checking pattern %S against %S"
                         pattern expanded-inner)
                (when (string-match-p pattern expanded-inner)
                  (message "[org-transclusion-blocks] Pattern matched! Expanding $%s" var-name)
                  (setq expanded-inner
                        (replace-regexp-in-string
                         pattern
                         (if (stringp var-value)
                             var-value
                           (format "%S" var-value))
                         expanded-inner
                         t t))
                  (message "[org-transclusion-blocks] After expansion: %S" expanded-inner))))

            ;; Re-wrap in brackets
            (setq result (concat "[[" expanded-inner "]]"))
            (message "[org-transclusion-blocks] Re-wrapped result: %S" result)))

      ;; Non-bracket value: original logic
      (progn
        (message "[org-transclusion-blocks] Non-bracket value, using standard expansion")
        ;; Try exact match first (bare variable name)
        (let ((exact-match (assoc (intern value) vars)))
          (when exact-match
            (message "[org-transclusion-blocks] Found exact match for %S" value)
            (let ((var-value (cdr exact-match)))
              (setq result (if (stringp var-value)
                               var-value
                             (format "%S" var-value))))))

        ;; Expand $varname references
        (dolist (var-pair vars)
          (let* ((var-name (symbol-name (car var-pair)))
                 (var-value (cdr var-pair))
                 (pattern (concat "\\$" (regexp-quote var-name))))
            (when (string-match-p pattern result)
              (message "[org-transclusion-blocks] Expanding $%s in %S" var-name result)
              (setq result
                    (replace-regexp-in-string
                     pattern
                     (if (stringp var-value)
                         var-value
                       (format "%S" var-value))
                     result
                     t t))
              (message "[org-transclusion-blocks] After expansion: %S" result))))))

    (message "[org-transclusion-blocks] Final result: %S" result)
    result))

;;;; Block Type Support

(defun org-transclusion-blocks--source-is-org-p (link-string)
  "Return non-nil if LINK-STRING targets Org content.

LINK-STRING is complete link including [[ ]] brackets.

Detects:
- file: links with .org extension
- id: links (always Org)
- Custom ID links (always Org)
- Org headline links

Used by `org-transclusion-blocks--should-escape-p'."
  (when (stringp link-string)
    (or
     ;; file:path.org or file:path.org::search
     (string-match-p (rx "[[file:" (* any) ".org" (or "::" "]]")) link-string)

     ;; id: links
     (string-match-p (rx "[[id:") link-string)

     ;; Custom ID links
     (string-match-p (rx "[[#") link-string)

     ;; Headline search in any file (heuristic)
     (string-match-p (rx "::" (* space) "*") link-string))))

(defun org-transclusion-blocks--should-escape-p (keyword-plist params)
  "Determine if content should be escaped.

KEYWORD-PLIST is org-transclusion keyword plist with :link property.
PARAMS is alist of header arguments.

Checks in priority order:
1. Explicit :transclude-escape-org header
2. Source file type via `org-transclusion-blocks-escape-org-sources'
3. Default to nil

Returns non-nil if escaping should occur.

Called by `org-transclusion-blocks-add'."
  (let ((explicit (assoc-default :transclude-escape-org params))
        (link-string (plist-get keyword-plist :link)))
    (cond
     ;; 1. Explicit header overrides everything
     ((and explicit (not (string-empty-p explicit)))
      (not (member explicit '("nil" "no" "false" "0"))))

     ;; 2. Check source type
     ((and org-transclusion-blocks-escape-org-sources
           (org-transclusion-blocks--source-is-org-p link-string))
      t)

     ;; 3. Default: no escaping
     (t nil))))

(defun org-transclusion-blocks--supported-block-p (element)
  "Return non-nil if ELEMENT supports transclusion.

ELEMENT is org-element block context.

Supports any block with #+begin/#+end delimiters.

Used by `org-transclusion-blocks-add' and
`org-transclusion-blocks-add-all'."
  (let ((type (org-element-type element)))
    (and (symbolp type)
         (string-suffix-p "-block" (symbol-name type))
         (not (string-prefix-p "inline-" (symbol-name type))))))

(defun org-transclusion-blocks--get-content-bounds (element)
  "Return (BEG . END) cons of content area for ELEMENT.

ELEMENT is org-element block context.

For src-block, uses `org-src--contents-area'.
For other blocks, calculates bounds from element properties.

Used by `org-transclusion-blocks--update-content' and
`org-transclusion-blocks--apply-timestamp'."
  (if (eq (org-element-type element) 'src-block)
      (let ((area (org-src--contents-area element)))
        (cons (nth 0 area) (nth 1 area)))
    (save-excursion
      (goto-char (org-element-property :begin element))
      (while (looking-at "^[ \t]*#\\+HEADER:")
        (forward-line))
      (unless (looking-at "^[ \t]*#\\+begin_")
        (error "Expected #+begin line at position %d" (point)))
      (forward-line)
      (let ((beg (point)))
        (unless (re-search-forward "^[ \t]*#\\+end_" nil t)
          (error "No matching #+end for block at position %d"
                 (org-element-property :begin element)))
        (forward-line 0)
        (cons beg (point))))))

(defun org-transclusion-blocks--ensure-trailing-newline (content)
  "Ensure CONTENT ends with newline without stripping trailing blank lines.

Unlike `org-element-normalize-string', this preserves all internal
and trailing whitespace including blank lines.  Only adds a newline
if content lacks one entirely.

Returns CONTENT unchanged if not a string or empty.
Returns CONTENT with single appended newline if missing.
Returns CONTENT unchanged if already ends with newline.

Used by `org-transclusion-blocks--update-content' to prevent block
delimiter concatenation while preserving whitespace fidelity."
  (cond
   ;; Not a string or empty - return unchanged
   ((not (stringp content)) content)
   ((string= "" content) content)
   ;; Already ends with newline - return unchanged
   ((string-suffix-p "\n" content) content)
   ;; Missing newline - append exactly one
   (t (concat content "\n"))))

(defun org-transclusion-blocks--update-content (element content)
  "Replace ELEMENT's content with CONTENT string.

ELEMENT is org-element block context.
CONTENT is string to insert.

Ensures content ends with newline via
`org-transclusion-blocks--ensure-trailing-newline' to prevent
block delimiter concatenation, while preserving all internal
and trailing whitespace including blank lines.

Replaces content directly for all block types to preserve
whitespace fidelity for line-range transclusions."
  (let* ((bounds (org-transclusion-blocks--get-content-bounds element))
         (beg (car bounds))
         (end (cdr bounds)))
    (unless (and beg end)
      (error "Could not determine content bounds for %s at position %d"
             (org-element-type element)
             (org-element-property :begin element)))
    (delete-region beg end)
    (goto-char beg)
    ;; Ensure trailing newline without stripping whitespace
    (insert (org-transclusion-blocks--ensure-trailing-newline content))))

;;;; Header Parsing

(defun org-transclusion-blocks--has-typed-components-p (element)
  "Return non-nil if ELEMENT has :transclude-type header.

ELEMENT is org-element block context.

Checks block headers, src-block parameters line, and property headers.

Used by `org-transclusion-blocks-add' to detect pre-validation requirement."
  (let ((raw-headers (org-element-property :header element))
        (raw-params (org-element-property :parameters element))
        (property-headers (org-transclusion-blocks--get-property-headers element)))
    (or (and raw-params (string-match-p ":transclude-type" raw-params))
        (seq-some (lambda (h) (string-match-p ":transclude-type" h))
                  raw-headers)
        (and property-headers (string-match-p ":transclude-type" property-headers)))))

(defun org-transclusion-blocks--get-property-headers (element)
  "Extract header-args properties from ELEMENT's parent headline.

ELEMENT is org-element block context.

Returns string of :HEADER-ARGS: property value or nil.

Used by `org-transclusion-blocks--has-typed-components-p',
`org-transclusion-blocks--parse-headers-direct', and
`org-transclusion-blocks--pre-validate-headers'."
  (let ((parent (org-element-property :parent element)))
    (while (and parent (not (eq (org-element-type parent) 'headline)))
      (setq parent (org-element-property :parent parent)))
    (when parent
      (org-element-property :HEADER-ARGS parent))))

(defun org-transclusion-blocks--parse-headers-direct (element)
  "Parse headers for non-Babel ELEMENT.

ELEMENT is org-element block context.

Returns alist of (KEYWORD . VALUE) pairs.

Includes headers from #+HEADER: lines and :HEADER-ARGS: property.

Used by `org-transclusion-blocks-add' for non-src-block types."
  (let ((headers nil)
        (raw-headers (org-element-property :header element)))
    (dolist (header-str raw-headers)
      (when (string-match "^:\\([^ \t]+\\)\\(?:[ \t]+\\(.+\\)\\)?$" header-str)
        (let ((key (intern (concat ":" (match-string 1 header-str))))
              (val (or (match-string 2 header-str) "")))
          (push (cons key val) headers))))
    (when-let ((prop-headers (org-transclusion-blocks--get-property-headers element)))
      (when (string-match "^:\\([^ \t]+\\)\\(?:[ \t]+\\(.+\\)\\)?$" prop-headers)
        (push (cons (intern (concat ":" (match-string 1 prop-headers)))
                    (or (match-string 2 prop-headers) ""))
              headers)))
    (nreverse headers)))

(defun org-transclusion-blocks--pre-validate-headers (element)
  "Pre-validate headers in ELEMENT using registered validators.

ELEMENT is org-element block context.

Runs :validator-pre validators for each component BEFORE variable
expansion.  These validators check Babel parsing safety.

Called by `org-transclusion-blocks-add' before variable expansion.

Returns t always."
  (let* ((raw-headers (org-element-property :header element))
         (raw-params (org-element-property :parameters element))
         (property-headers (org-transclusion-blocks--get-property-headers element))
         (all-header-strings (delq nil
                                   (append
                                    (when property-headers (list property-headers))
                                    (when raw-params (list raw-params))
                                    raw-headers))))
    (let ((type-keyword nil)
          (parsed-params nil))
      ;; Extract :transclude-type
      (dolist (header-str all-header-strings)
        (when (string-match ":transclude-type[ \t]+\\([^ \t\n]+\\)" header-str)
          (setq type-keyword (intern (match-string 1 header-str)))))
      
      (when-let ((component-spec (alist-get type-keyword
                                            org-transclusion-blocks--type-components)))
        (let ((pre-validators (make-hash-table :test 'eq)))
          ;; Build pre-validator map
          (cl-loop for (semantic-key meta) on component-spec by #'cddr
                   for header-key = (plist-get meta :header)
                   for validator-pre = (plist-get meta :validator-pre)
                   when validator-pre
                   do (puthash header-key validator-pre pre-validators))
          
          ;; Parse and validate headers
          (dolist (header-str all-header-strings)
            (when (string-match "^:\\([^ \t]+\\)\\(?:[ \t]+\\(.+\\)\\)?$" header-str)
              (let* ((key (intern (concat ":" (match-string 1 header-str))))
                     (val (or (match-string 2 header-str) ""))
                     (validator (gethash key pre-validators)))
                (push (cons key val) parsed-params)
                (when validator
                  (funcall validator val key type-keyword)))))))))
  t)

(defun org-transclusion-blocks--post-validate-headers (params)
  "Post-validate PARAMS using registered validators.

PARAMS is alist of header arguments (already parsed and expanded).

Runs :validator-post and :validator validators for each component
AFTER variable expansion.  These validators check semantic correctness.

Called by `org-transclusion-blocks-add' after variable expansion.

Returns t always."
  (let ((type-keyword nil))
    ;; Extract :transclude-type from params
    (when-let ((type-raw (cdr (assq :transclude-type params))))
      (setq type-keyword (if (symbolp type-raw) type-raw
                           (intern (org-strip-quotes
                                    (if (stringp type-raw) type-raw
                                      (format "%s" type-raw)))))))
    
    (when-let ((component-spec (alist-get type-keyword
                                          org-transclusion-blocks--type-components)))
      (let ((post-validators (make-hash-table :test 'eq)))
        ;; Build post-validator map
        (cl-loop for (semantic-key meta) on component-spec by #'cddr
                 for header-key = (plist-get meta :header)
                 ;; Check :validator-post first, fall back to :validator
                 for validator-post = (or (plist-get meta :validator-post)
                                          (plist-get meta :validator))
                 when validator-post
                 do (puthash header-key validator-post post-validators))
        
        ;; Run validators on expanded params
        (dolist (pair params)
          (let* ((key (car pair))
                 (val (cdr pair))
                 (validator (gethash key post-validators)))
            (when validator
              ;; Convert value to string if needed
              (let ((val-str (cond
                              ((stringp val) val)
                              ((symbolp val) (symbol-name val))
                              ((numberp val) (number-to-string val))
                              (t (format "%S" val)))))
                (funcall validator val-str key type-keyword)))))
        
        ;; Check interactions
        (when-let ((warnings (org-transclusion-blocks--check-interactions
                              type-keyword
                              params
                              component-spec)))
          (when org-transclusion-blocks-show-interaction-warnings
            (display-warning 'org-transclusion-blocks
                             (concat "Component interaction issues:\n"
                                     (mapconcat (lambda (w) (concat "  • " w))
                                                warnings
                                                "\n"))
                             :warning))))))
  t)

;;;; Block Type
(defun org-transclusion-blocks--check-generic-conflicts (params)
  "Check for conflicts in generic transclusion headers.

PARAMS is alist of header arguments.

Signals error for hard conflicts:
- Both :transclude-lines and :transclude-thing present

Warns for soft conflicts:
- Header duplicates :transclude-keywords specification

Called by `org-transclusion-blocks--check-mode-compat'."
  (let ((lines-spec (org-transclusion-blocks--get-lines-spec params))
        (thing-spec (org-transclusion-blocks--get-thing-spec params))
        (keywords (assoc-default :transclude-keywords params)))

    ;; Hard conflict: both lines and thing specified
    (when (and lines-spec thing-spec)
      (user-error "Cannot use both :transclude-lines and :transclude-thing
Headers are mutually exclusive - choose one:
  :transclude-lines for line ranges
  :transclude-thing for semantic units"))

    ;; Soft conflict warnings for redundancy
    (when (and org-transclusion-blocks-show-interaction-warnings
               keywords)
      (let ((lines-in-keywords
             (org-transclusion-blocks--extract-from-keywords
              keywords ":lines"))
            (thing-in-keywords
             (org-transclusion-blocks--extract-from-keywords
              keywords ":thing-at-point")))

        (when (and lines-spec lines-in-keywords)
          (display-warning
           'org-transclusion-blocks
           (format ":transclude-lines header value %S will override
:lines specification in :transclude-keywords

Found in keywords: %S
Header takes precedence. Remove :lines from :transclude-keywords
to eliminate this warning."
                   lines-spec
                   (substring keywords
                              (car lines-in-keywords)
                              (cdr lines-in-keywords)))
           :warning))

        (when (and thing-spec thing-in-keywords)
          (display-warning
           'org-transclusion-blocks
           (format ":transclude-thing header value %S will override
:thing-at-point specification in :transclude-keywords

Found in keywords: %S
Header takes precedence. Remove :thing-at-point from :transclude-keywords
to eliminate this warning."
                   thing-spec
                   (substring keywords
                              (car thing-in-keywords)
                              (cdr thing-in-keywords)))
           :warning))))))

(defun org-transclusion-blocks--detect-mode (params)
  "Determine which construction mode PARAMS represents.

PARAMS is alist of header arguments.

Returns one of:
  \\='direct - :transclude header present
  \\='type-specific - :transclude-type with registered type
  nil - no mode detected

Used by `org-transclusion-blocks--check-mode-compat'."
  (cond
   ((assoc :transclude params) 'direct)
   ((and (assoc :transclude-type params)
         (alist-get (intern (cdr (assoc :transclude-type params)))
                    org-transclusion-blocks--type-components))
    'type-specific)
   (t nil)))

(defun org-transclusion-blocks--check-mode-compat (params)
  "Warn if PARAMS mixes construction modes or has generic conflicts.

PARAMS is alist of header arguments.

Emits warnings when `org-transclusion-blocks-show-interaction-warnings'
is non-nil.

Checks for:
1. Direct mode (:transclude) mixed with component mode (:transclude-type)
2. Headers from other registered types appearing in current type's block
3. Conflicting generic headers (:transclude-lines vs :transclude-thing)
4. Redundant specifications (header + keyword duplication)

Called by `org-transclusion-blocks-add'."
  (when org-transclusion-blocks-show-interaction-warnings
    (let ((mode (org-transclusion-blocks--detect-mode params)))
      (pcase mode
        ('direct
         ;; Warn if :transclude-type also present
         (when (assoc :transclude-type params)
           (display-warning
            'org-transclusion-blocks
            ":transclude header takes priority; :transclude-type ignored"
            :warning)))

        ('type-specific
         ;; Check for cross-type contamination
         (let* ((current-type (intern (cdr (assoc :transclude-type params))))
                (header-registry (org-transclusion-blocks--get-all-registered-headers))
                (foreign-headers
                 (seq-filter
                  (lambda (pair)
                    (let ((key (car pair)))
                      (when-let ((owner-type (alist-get key header-registry)))
                        ;; Header is registered to a different type
                        (not (eq owner-type current-type)))))
                  params)))

           (when foreign-headers
             (display-warning
              'org-transclusion-blocks
              (format "Type-specific mode active for %s; headers from other types detected:

%s

These headers belong to:
%s"
                      current-type
                      (mapconcat (lambda (pair)
                                   (format "  %s" (car pair)))
                                 foreign-headers
                                 "\n")
                      (mapconcat (lambda (pair)
                                   (format "  %s -> %s type"
                                           (car pair)
                                           (alist-get (car pair) header-registry)))
                                 foreign-headers
                                 "\n"))
              :warning))))))

    ;; Check generic header conflicts (works in both modes)
    (org-transclusion-blocks--check-generic-conflicts params)))


;;;; Link conversion
(defun org-transclusion-blocks--parse-keyword-line ()
  "Parse #+transclude: keyword at point.

Returns plist with :link, :lines, :thing-at-point, :raw-keyword.

Point must be on keyword line.

Used by conversion functions to extract components."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^[ \t]*#\\+transclude:")
      (error "Not on #+transclude: keyword line"))

    (org-transclusion-keyword-string-to-plist)))

(defun org-transclusion-blocks--validate-conversion-context ()
  "Check if current position is suitable for keyword conversion.

Returns t if on #+transclude: line and not inside block.
Signals user-error with explanation otherwise.

Used to guard conversion commands."
  (save-excursion
    (beginning-of-line)
    (cond
     ((not (looking-at "^[ \t]*#\\+transclude:"))
      (user-error "Point is not on #+transclude: keyword line"))

     ((org-transclusion-blocks--inside-block-p)
      (user-error "Cannot convert keyword inside block; \
move to keyword line first"))

     (t t))))

(defun org-transclusion-blocks--inside-block-p ()
  "Return non-nil if point is inside a block structure.

Checks for enclosing #+begin_/#+end_ delimiters."
  (let ((element (org-element-at-point)))
    (and element
         (memq (org-element-type element)
               '(src-block quote-block example-block export-block
                 special-block verse-block center-block comment-block)))))


;;;###autoload
(defun org-transclusion-blocks-convert-keyword-at-point ()
  "Convert #+transclude: keyword at point to header block form.

Parses the keyword line using `org-transclusion-keyword-string-to-plist',
extracts :link, :lines, :thing-at-point, and residual keywords,
then generates equivalent header form:

  ,#+HEADER: :transclude LINK
  ,#+HEADER: :transclude-lines RANGE   ; if :lines present
  ,#+HEADER: :transclude-thing THING   ; if :thing-at-point present
  ,#+HEADER: :transclude-keywords RESIDUAL  ; if other keywords remain
  ,#+begin_src LANG
  ,#+end_src

Original keyword line is commented for reference.

Block language defaults to `elisp'.  Customize with prefix argument:
with \\[universal-argument], prompt for block type.

Point must be on #+transclude: keyword line.

Returns t on success, nil if not on keyword line.

See Info node `(org-transclusion-blocks) Converting Keywords'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at "^[ \t]*#\\+transclude:")
      (user-error "Point is not on #+transclude: keyword line"))

    (let* ((original-line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
           (keyword-plist (org-transclusion-keyword-string-to-plist))
           (link (plist-get keyword-plist :link))
           (lines-spec (plist-get keyword-plist :lines))
           (thing-spec (plist-get keyword-plist :thing-at-point))
           (raw-keyword-string (plist-get keyword-plist :raw-keyword))
           (block-type (if current-prefix-arg
                           (completing-read "Block type: "
                                            '("src" "quote" "example")
                                            nil t nil nil "src")
                         "src"))
           (block-lang (when (string= block-type "src")
                         (if current-prefix-arg
                             (read-string "Language: " "elisp")
                           "elisp")))
           (residual-keywords
            (org-transclusion-blocks--extract-residual-keywords
             raw-keyword-string lines-spec thing-spec)))

      (unless link
        (user-error "No link found in #+transclude: keyword"))

      ;; Comment original line
      (beginning-of-line)
      (insert ";; Original: ")
      (end-of-line)
      (insert "\n")

      ;; Generate header lines
      (let ((indent (save-excursion
                      (forward-line -1)
                      (if (looking-at "^[ \t]*;; Original:")
                          (current-indentation)
                        0))))
        (insert (make-string indent ?\s)
                "#+HEADER: :transclude " link "\n")

        (when lines-spec
          (insert (make-string indent ?\s)
                  "#+HEADER: :transclude-lines " lines-spec "\n"))

        (when thing-spec
          (insert (make-string indent ?\s)
                  "#+HEADER: :transclude-thing " thing-spec "\n"))

        (when (and residual-keywords
                   (not (string-empty-p (string-trim residual-keywords))))
          (insert (make-string indent ?\s)
                  "#+HEADER: :transclude-keywords \""
                  residual-keywords "\"\n"))

        ;; Insert block delimiters
        (insert (make-string indent ?\s)
                "#+begin_" block-type)
        (when block-lang
          (insert " " block-lang))
        (insert "\n")
        (let ((body-start (point)))
          (insert (make-string indent ?\s)
                  "#+end_" block-type "\n")
          ;; Position point in block body
          (goto-char body-start)))

      (message "Converted #+transclude: keyword to header form")
      t)))

(defun org-transclusion-blocks--extract-residual-keywords (raw-keyword-string
                                                           lines-spec
                                                           thing-spec)
  "Extract keywords not promoted to dedicated headers.

RAW-KEYWORD-STRING is the complete keyword portion after link.
LINES-SPEC is value of :lines if present.
THING-SPEC is value of :thing-at-point if present.

Returns string of remaining keywords or nil.

Removes :lines and :thing-at-point specifications, preserves all
other keyword arguments like :only-contents, :level, :src, :export."
  (when (and raw-keyword-string
             (not (string-empty-p (string-trim raw-keyword-string))))
    (let ((cleaned raw-keyword-string))
      ;; Remove :lines specification if present
      (when lines-spec
        (setq cleaned
              (replace-regexp-in-string
               (concat ":lines[ \t]+" (regexp-quote lines-spec))
               ""
               cleaned)))

      ;; Remove :thing-at-point specification if present
      (when thing-spec
        (setq cleaned
              (replace-regexp-in-string
               (concat ":thing-at-point[ \t]+" (regexp-quote thing-spec))
               ""
               cleaned)))

      ;; Clean up multiple spaces
      (setq cleaned (replace-regexp-in-string "[ \t]+" " " cleaned))
      (setq cleaned (string-trim cleaned))

      (if (string-empty-p cleaned)
          nil
        cleaned))))

;;;###autoload
(defun org-transclusion-blocks-convert-keywords-in-region (beg end)
  "Convert all #+transclude: keywords to header form in region.

BEG and END delimit region to process.

Converts each #+transclude: keyword line within region using
`org-transclusion-blocks-convert-keyword-at-point'.

Returns list of positions where conversions occurred.

Interactively, operates on active region if present, otherwise
prompts for region boundaries.

See Info node `(org-transclusion-blocks) Batch Converting'."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (read-number "Start position: " (point-min))
           (read-number "End position: " (point-max)))))

  (let ((converted-positions nil))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^[ \t]*#\\+transclude:" end t)
        (beginning-of-line)
        (let ((pos (point)))
          (when (org-transclusion-blocks-convert-keyword-at-point)
            (push pos converted-positions))
          ;; Move past the newly inserted block
          (when (re-search-forward "^[ \t]*#\\+end_" nil t)
            (forward-line)))))

    (message "Converted %d #+transclude: keyword%s"
             (length converted-positions)
             (if (= (length converted-positions) 1) "" "s"))

    (nreverse converted-positions)))

;;;###autoload
(defun org-transclusion-blocks-convert-keywords-in-buffer ()
  "Convert all #+transclude: keywords to header form in buffer.

Delegates to `org-transclusion-blocks-convert-keywords-in-region'
with buffer bounds.

Returns list of positions where conversions occurred."
  (interactive)
  (org-transclusion-blocks-convert-keywords-in-region
   (point-min)
   (point-max)))


;;;; Link Construction
(defun org-transclusion-blocks--params-hash (params)
  "Generate hash of PARAMS for cache validation.

Only hashes parameters that affect link construction, excluding
result-handling parameters that don't impact content fetching."
  (sxhash-equal
   (seq-filter
    (lambda (pair)
      (not (memq (car pair)
                 '(:result-params :result-type :exports :cache))))
    params)))

;;;; Org Syntax Escaping

(defun org-transclusion-blocks--escape-org-syntax (content)
  "Escape Org syntax in CONTENT string.

CONTENT is transcluded text.

Returns escaped string with Org markup prepended by commas.

Uses `org-escape-code-in-region'.

Called by `org-transclusion-blocks-add'."
  (with-temp-buffer
    (insert content)
    (org-escape-code-in-region (point-min) (point-max))
    (buffer-string)))

;;;; Content Fetching

(defun org-transclusion-blocks--fetch-content (keyword-plist)
  "Fetch transcluded content using KEYWORD-PLIST.

KEYWORD-PLIST is org-transclusion keyword plist.

Returns content string or nil.

Delegates to `org-transclusion-add-functions' hook.
Caches payload in `org-transclusion-blocks--last-payload'.

Called by `org-transclusion-blocks-add'."
  (when-let* ((link-string (plist-get keyword-plist :link))
              (link (org-transclusion-wrap-path-to-link link-string)))
    (save-window-excursion
      (when-let* ((payload (run-hook-with-args-until-success
                            'org-transclusion-add-functions
                            link
                            keyword-plist)))
        (setq org-transclusion-blocks--last-payload payload)
        (plist-get payload :src-content)))))

;;;; Metadata insertion
(defun org-transclusion-blocks--find-existing-overlay (beg end)
  "Find existing transclusion overlay in region BEG to END.

Returns overlay with `org-transclusion-blocks-overlay' property
or nil if none exists.

Used by `org-transclusion-blocks--apply-metadata' to reuse
overlays across content updates."
  (seq-find
   (lambda (ov)
     (overlay-get ov 'org-transclusion-blocks-overlay))
   (overlays-in beg end)))

(defun org-transclusion-blocks--get-or-create-id (beg end)
  "Return existing transclusion ID for region BEG to END or create new one.

Checks text properties in region for existing `org-transclusion-id'.
If found, returns that ID to maintain stability across updates.
Otherwise generates new UUID.

ID stability is critical for org-transclusion.el integration—
changing IDs on every update breaks source overlay pairing."
  (or (get-text-property beg 'org-transclusion-id)
      (org-id-uuid)))

(defun org-transclusion-blocks--ensure-overlays-applied ()
  "Apply overlays if suppression was active.

Called after transient menu exits to create overlays for the
current transclusion after interactive adjustment completes.

Text properties are already present from
`org-transclusion-blocks-add' calls during adjustment, so this
function only creates overlays using existing metadata."
  (when org-transclusion-blocks--suppress-overlays
    (setq org-transclusion-blocks--suppress-overlays nil)
    (save-excursion
      (let ((element (org-element-at-point)))
        (when (org-transclusion-blocks--supported-block-p element)
          (let* ((block-beg (org-element-property :begin element))
                 (block-end (org-element-property :end element))
                 (id (get-text-property block-beg 'org-transclusion-id))
                 (keyword-plist (get-text-property block-beg 'org-transclusion-blocks-keyword))
                 (link-string (get-text-property block-beg 'org-transclusion-blocks-link)))
            (when (and id keyword-plist link-string)
              (let* ((payload (org-transclusion-blocks--get-payload-for-metadata link-string keyword-plist))
                     (src-beg (plist-get payload :src-beg))
                     (src-end (plist-get payload :src-end))
                     (src-buf (plist-get payload :src-buf)))
                (when (and src-beg src-end src-buf)
                  (org-transclusion-blocks--create-overlays
                   block-beg block-end src-beg src-end src-buf id))))))))))

(defun org-transclusion-blocks--create-overlays (block-beg block-end src-beg src-end src-buf id)
  "Create transclusion overlays for block and source regions.

BLOCK-BEG and BLOCK-END delimit transclusion block in current buffer.
SRC-BEG and SRC-END delimit source region in SRC-BUF.
ID is unique transclusion identifier.

Deletes existing overlays with matching ID before creating new ones
to avoid accumulation while preserving overlays from other transclusions.

Called by `org-transclusion-blocks--apply-metadata' and
`org-transclusion-blocks--ensure-overlays-applied'."
  (let ((tc-buffer (current-buffer)))
    ;; Delete existing overlays for THIS transclusion only
    (dolist (ov (overlays-in block-beg block-end))
      (when (and (overlay-get ov 'org-transclusion-blocks-overlay)
                 ;; Only delete if ID matches or overlay has no ID (old overlay)
                 (or (not (overlay-get ov 'org-transclusion-id))
                     (equal (overlay-get ov 'org-transclusion-id) id)))
        (delete-overlay ov)))

    ;; Create fresh overlays
    (let ((ov-src (make-overlay src-beg src-end src-buf))
          (ov-tc (make-overlay block-beg block-end)))

      ;; Configure source overlay
      (overlay-put ov-src 'org-transclusion-by id)
      (overlay-put ov-src 'org-transclusion-buffer tc-buffer)
      (overlay-put ov-src 'evaporate t)
      (overlay-put ov-src 'org-transclusion-pair ov-tc)
      (overlay-put ov-src 'org-transclusion-id id)  ; Add ID to source overlay

      ;; Configure transclusion overlay
      (overlay-put ov-tc 'evaporate t)
      (overlay-put ov-tc 'org-transclusion-pair ov-src)
      (overlay-put ov-tc 'org-transclusion-blocks-overlay t)
      (overlay-put ov-tc 'org-transclusion-id id)  ; Add ID to transclusion overlay

      ;; Update text property to reference new source overlay
      (put-text-property block-beg block-end 'org-transclusion-pair ov-src))))

(defun org-transclusion-blocks--apply-metadata (block-beg block-end keyword-plist link-string)
  "Apply transclusion metadata properties to block region BLOCK-BEG to BLOCK-END.

BLOCK-BEG is beginning of entire block including delimiters.
BLOCK-END is end of entire block including delimiters.
KEYWORD-PLIST is the org-transclusion keyword plist.
LINK-STRING is the constructed link string (with [[ ]] brackets).

Always applies text properties immediately for metadata tracking.
Creates overlays only when `org-transclusion-blocks--suppress-overlays'
is nil.

When `org-transclusion-blocks--undo-handle' is non-nil (during
transient menu), text property changes are not recorded in undo
list to prevent undo corruption when undoing/redoing content
changes.

Properties stored:
- `org-transclusion-blocks-keyword' - Full keyword plist
- `org-transclusion-blocks-link' - Constructed link string
- `org-transclusion-blocks-max-line' - Source buffer line count
- `org-transclusion-pair' - Source overlay (when overlays created)
- `org-transclusion-type' - Type for hook dispatch
- `org-transclusion-id' - Unique transclusion identifier

Properties applied to entire block region (including #+HEADER:,
\"#+begin_src\", and \"#+end_src\" lines) to ensure
`org-transclusion-at-point' from org-transclusion.el can locate
transclusion boundaries during `save-buffer' hooks.

Called by `org-transclusion-blocks-add'."
  (let* ((max-line (org-transclusion-blocks--get-source-line-count link-string))
         (payload (org-transclusion-blocks--get-payload-for-metadata link-string keyword-plist))
         (src-beg (plist-get payload :src-beg))
         (src-end (plist-get payload :src-end))
         (src-buf (plist-get payload :src-buf))
         (tc-type (plist-get payload :tc-type))
         (id (or (get-text-property block-beg 'org-transclusion-id)
                 (org-id-uuid)))
         ;; Save undo list position before text property changes
         (undo-list-before (when org-transclusion-blocks--undo-handle
                             buffer-undo-list)))

    ;; Apply text properties
    (if (and src-beg src-end src-buf)
        ;; Full metadata with source info
        (add-text-properties
         block-beg block-end
         `(org-transclusion-blocks-keyword ,keyword-plist
           org-transclusion-blocks-link ,link-string
           org-transclusion-blocks-max-line ,max-line
           org-transclusion-type ,tc-type
           org-transclusion-id ,id))
      ;; Fallback metadata without source info
      (add-text-properties
       block-beg block-end
       `(org-transclusion-blocks-keyword ,keyword-plist
         org-transclusion-blocks-link ,link-string
         org-transclusion-blocks-max-line ,max-line)))

    ;; Remove text property undo entries during transient menu
    (when org-transclusion-blocks--undo-handle
      (setq buffer-undo-list undo-list-before))

    ;; Create overlays only when not suppressed
    (unless org-transclusion-blocks--suppress-overlays
      (when (and src-beg src-end src-buf)
        (org-transclusion-blocks--create-overlays
         block-beg block-end src-beg src-end src-buf id)))))

(defun org-transclusion-blocks--get-payload-for-metadata (link-string keyword-plist)
  "Obtain payload for metadata storage from LINK-STRING and KEYWORD-PLIST.

LINK-STRING is complete link including [[ ]] brackets.
KEYWORD-PLIST is org-transclusion keyword plist.

Returns payload plist with :src-beg, :src-end, :src-buf, :tc-type.

This is a lightweight call to org-transclusion-add-functions
solely for metadata extraction, not content fetching.

Called by `org-transclusion-blocks--apply-metadata'."
  (or org-transclusion-blocks--last-payload
      (when-let* ((link (org-transclusion-wrap-path-to-link link-string)))
        (run-hook-with-args-until-success
         'org-transclusion-add-functions
         link
         keyword-plist))))

(defun org-transclusion-blocks--get-source-line-count (link-string)
  "Return line count of buffer targeted by LINK-STRING.

LINK-STRING is complete link including [[ ]] brackets.

Returns nil if buffer cannot be opened or has no content.

Used by `org-transclusion-blocks--apply-metadata'."
  (condition-case nil
      (let ((link (org-transclusion-wrap-path-to-link link-string)))
        (save-window-excursion
          (save-excursion
            ;; Open link in background
            (org-link-open link)
            ;; Count lines in opened buffer
            (with-current-buffer (current-buffer)
              (count-lines (point-min) (point-max))))))
    (error nil)))
;;;; Content Insertion

(defun org-transclusion-blocks--apply-timestamp (beg end)
  "Apply fetch timestamp property to region BEG to END.

BEG is buffer position.
END is buffer position.

Sets `org-transclusion-blocks-timestamp-property' text property.

Called by `org-transclusion-blocks-add'."
  (add-text-properties beg end
                       (list org-transclusion-blocks-timestamp-property
                             (current-time))))

(defun org-transclusion-blocks--show-indicator (element)
  "Display success indicator overlay on block ELEMENT.

ELEMENT is org-element block context.

Shows checkmark for `org-transclusion-blocks-indicator-duration' seconds.
Displays fetch timestamp in echo area if available.

Reuses existing indicator overlay if present, extending its timer.
This prevents overlay accumulation during rapid refreshes.

Called by `org-transclusion-blocks-add'."
  (when (> org-transclusion-blocks-indicator-duration 0)
    (let* ((beg (org-element-property :begin element))
           (end (save-excursion
                  (goto-char beg)
                  (line-end-position)))
           ;; Check for existing indicator overlay
           (existing-ov
            (seq-find
             (lambda (ov)
               (overlay-get ov 'org-transclusion-blocks-indicator))
             (overlays-in beg end)))
           (ov (or existing-ov (make-overlay beg end))))

      ;; Cancel existing timer if overlay was reused
      (when existing-ov
        (when-let ((timer (overlay-get ov 'org-transclusion-blocks-timer)))
          (cancel-timer timer)))

      ;; Set overlay properties (idempotent if reusing)
      (overlay-put ov 'before-string
                   (propertize "☑ " 'face '(:foreground "green" :weight bold)))
      (overlay-put ov 'org-transclusion-blocks-indicator t)

      ;; Create new timer and store it on overlay
      (let ((timer (run-at-time org-transclusion-blocks-indicator-duration
                                nil
                                (lambda (overlay)
                                  (when (overlay-buffer overlay)
                                    (delete-overlay overlay)))
                                ov)))
        (overlay-put ov 'org-transclusion-blocks-timer timer))))

  ;; (when org-transclusion-blocks--last-fetch-time
  ;; (message "Content fetched at %s"
  ;;          (format-time-string "%H:%M:%S"
  ;;                              org-transclusion-blocks--last-fetch-time)))
  )

;;;; Public Commands

;;;###autoload
(defun org-transclusion-blocks-add ()
  "Fetch and insert transcluded content into block at point.

Supports two mutually exclusive header forms:

1. Direct link mode:
   #+HEADER: :transclude [[TYPE:PATH::SEARCH]]
   #+HEADER: :transclude-keywords \":lines 10-15 :only-contents\"

2. Component mode (requires registered type):
   #+HEADER: :transclude-type REGISTERED-TYPE
   #+HEADER: :TYPE-COMPONENT-1 VALUE
   #+HEADER: :TYPE-COMPONENT-2 VALUE

Variable substitution is supported in transclusion headers:
   #+HEADER: :var file=\"~/project/file.org\"
   #+HEADER: :transclude [[file:$file]]
   #+HEADER: :transclude-lines 10-20

Generic transclusion headers always support variable expansion.
Type-specific headers support expansion when registered with
:expand-vars t property.  Babel control headers (:results, :session)
are never expanded.

Validation occurs in two phases:
1. Pre-validation (before expansion): :validator-pre checks syntax
2. Post-validation (after expansion): :validator-post checks semantics

Point must be on or within a supported block type.

For src-blocks, uses Babel for header processing.
For other blocks, parses headers directly.

Runs pre-validators via `org-transclusion-blocks--pre-validate-headers'
before variable expansion.  Runs post-validators via
`org-transclusion-blocks--post-validate-headers' after expansion.

Applies Org syntax escaping via
`org-transclusion-blocks--escape-org-syntax' when
`org-transclusion-blocks--should-escape-p' returns non-nil.

Stores metadata in text properties for boundary checking:
- `org-transclusion-blocks-keyword' - keyword plist
- `org-transclusion-blocks-link' - constructed link
- `org-transclusion-blocks-max-line' - source line count
- `org-transclusion-id' - unique identifier
- `org-transclusion-type' - transclusion type
- `org-transclusion-pair' - source overlay

Properties applied to entire block region (including headers and
delimiters) to ensure compatibility with `org-transclusion-at-point'
from org-transclusion.el during save-buffer hooks.

Text properties are always applied immediately.  Overlays are
created only when `org-transclusion-blocks--suppress-overlays' is
nil.  This allows transient menus to update content rapidly
without expensive overlay operations.

See `org-transclusion-blocks-list-types' for available types.

Returns t on success, nil if no headers or fetch failed."
  (interactive)
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))

    (if (not (org-transclusion-blocks--supported-block-p element))
        (progn
          (message "Not on a supported block (point on: %s)" type)
          nil)

      (save-excursion
        (goto-char (org-element-property :begin element))

        ;; Pre-validate BEFORE variable expansion (syntax checks)
        (when (org-transclusion-blocks--has-typed-components-p element)
          (org-transclusion-blocks--pre-validate-headers element))

        (let* ((params (if (eq type 'src-block)
                           (nth 2 (org-babel-get-src-block-info))
                         (org-transclusion-blocks--parse-headers-direct element)))
               ;; Expand variables in transclusion headers only
               (expanded-params (org-transclusion-blocks--expand-header-vars params)))

          ;; Post-validate AFTER variable expansion (semantic checks)
          (when (org-transclusion-blocks--has-typed-components-p element)
            (org-transclusion-blocks--post-validate-headers expanded-params))

          (let ((keyword-plist (org-transclusion-blocks--params-to-plist expanded-params)))

            (org-transclusion-blocks--check-mode-compat expanded-params)

            (if (not keyword-plist)
                (progn
                  (message "No transclusion headers found")
                  nil)

              (let ((link-string (plist-get keyword-plist :link)))
                (if-let ((content (org-transclusion-blocks--fetch-content keyword-plist)))
                    (progn
                      (when (org-transclusion-blocks--should-escape-p keyword-plist expanded-params)
                        (setq content (org-transclusion-blocks--escape-org-syntax content)))

                      ;; Remember position before content update
                      (let ((block-start (org-element-property :begin element)))
                        ;; Update content
                        (org-transclusion-blocks--update-content element content)
                        (setq org-transclusion-blocks--last-fetch-time (current-time))

                        ;; Force re-parse by moving to block start and getting fresh element
                        (goto-char block-start)
                        (setq element (org-element-at-point))

                        (let* ((bounds (org-transclusion-blocks--get-content-bounds element))
                               (content-beg (car bounds))
                               (content-end (cdr bounds))
                               (block-beg (org-element-property :begin element))
                               (block-end (org-element-property :end element)))

                          ;; Verify we got valid boundaries
                          (unless (and block-beg block-end content-beg content-end
                                       (< block-beg content-beg)
                                       (< content-beg content-end)
                                       (< content-end block-end))
                            (error "Invalid block boundaries after content insertion: block[%s-%s] content[%s-%s]"
                                   block-beg block-end content-beg content-end))

                          ;; Always apply timestamp to content
                          (org-transclusion-blocks--apply-timestamp content-beg content-end)
                          ;; Always apply metadata (properties immediately, overlays conditionally)
                          (org-transclusion-blocks--apply-metadata block-beg block-end keyword-plist link-string))

                        (org-transclusion-blocks--show-indicator element)
                        t))

                  (message "Failed to fetch transclusion content")
                  nil)))))))))

;;;###autoload
(defun org-transclusion-blocks-add-all (&optional scope)
  "Fetch and insert content for all blocks in SCOPE.

SCOPE can be:
  nil or \\='buffer - entire buffer (default)
  \\='subtree       - current subtree
  \\='region        - active region

Processes all supported block types with transclusion headers.

Returns list of successfully processed block positions."
  (interactive)
  (let ((scope (or scope 'buffer))
        (success-count 0)
        (failure-count 0)
        (processed-positions nil))

    (save-excursion
      (save-restriction
        (pcase scope
          ('buffer (widen))
          ('subtree (org-narrow-to-subtree))
          ('region (when (use-region-p)
                     (narrow-to-region (region-beginning) (region-end)))))

        (org-element-map (org-element-parse-buffer)
            '(src-block quote-block example-block export-block special-block
              verse-block center-block comment-block)
          (lambda (element)
            (when (or (org-transclusion-blocks--has-typed-components-p element)
                      (let ((raw-headers (org-element-property :header element)))
                        (seq-some (lambda (h) (string-match-p ":transclude" h))
                                  raw-headers)))
              (goto-char (org-element-property :begin element))
              (condition-case err
                  (when (org-transclusion-blocks-add)
                    (push (point) processed-positions)
                    (cl-incf success-count))
                (error
                 (cl-incf failure-count)
                 (message "Error at block line %d: %s"
                          (line-number-at-pos)
                          (error-message-string err)))))))))

    (message "Processed %d block%s (%d succeeded, %d failed)"
             (+ success-count failure-count)
             (if (= (+ success-count failure-count) 1) "" "s")
             success-count
             failure-count)

    (nreverse processed-positions)))

;;;###autoload
(defun org-transclusion-blocks-validate-current-block ()
  "Validate current block without inserting content.

Tests validator configurations without content fetching overhead.

Runs `org-transclusion-blocks--pre-validate-headers' when typed components
detected.

Useful for testing validator configurations during development."
  (interactive)
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))
    (if (not (org-transclusion-blocks--supported-block-p element))
        (message "Not on a supported block (point on: %s)" type)
      (condition-case err
          (progn
            (when (org-transclusion-blocks--has-typed-components-p element)
              (org-transclusion-blocks--pre-validate-headers element))
            (message "Validation passed for %s block" type))
        (error
         (message "Validation failed: %s" (error-message-string err)))))))


(defun org-transclusion-blocks--ensure-yank-exclusions ()
  "Add transclusion properties to `yank-excluded-properties' if missing.

This function is idempotent and safe to call multiple times.
It preserves any existing exclusions while adding our properties."
  (dolist (prop org-transclusion-blocks-yank-excluded-properties)
    (unless (memq prop yank-excluded-properties)
      (push prop yank-excluded-properties))))

;; Install exclusions at load time
(org-transclusion-blocks--ensure-yank-exclusions)

(provide 'org-transclusion-blocks)
;;; org-transclusion-blocks.el ends here
