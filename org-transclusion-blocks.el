;;; org-transclusion-blocks.el --- Component-based transclusion framework -*- lexical-binding: t; -*-

;; Author: Gino Cornejo
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/org-transclusion-blocks
;; Keywords: hypermedia vc

;; Package-Version: 0.2.0
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

;; Framework for component-based transclusion link construction.
;;
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
;; Org syntax escaping prevents markup collisions:
;;
;; Enabled by default for Org sources via
;; `org-transclusion-blocks-escape-org-sources'.  Override per-block with
;; :transclude-escape-org header or set file/subtree defaults via
;; #+PROPERTY: header-args.
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
- Soft conflicts

When nil, only hard conflicts (requirements, mutual exclusions) cause errors.

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

(defvar org-transclusion-blocks--type-components nil
  "Alist mapping link types to component specifications.

Each entry: (TYPE . COMPONENT-SPEC)

COMPONENT-SPEC is plist:
  (:semantic-name (:header :header-keyword
                   :validator FUNCTION-OR-NIL
                   :required BOOLEAN-OR-NIL
                   :shadowed-by LIST-OR-NIL
                   :requires LIST-OR-NIL
                   :conflicts LIST-OR-NIL) ...)

Populated via `org-transclusion-blocks-register-type'.
Queried by `org-transclusion-blocks--extract-type-components' and
`org-transclusion-blocks--pre-validate-headers'.")

(defvar org-transclusion-blocks--type-constructors nil
  "Alist mapping link types to constructor functions.

Each entry: (TYPE . CONSTRUCTOR-FUNC)

CONSTRUCTOR-FUNC receives plist of component values,
returns raw link string (without [[ ]] brackets).

Populated via `org-transclusion-blocks-register-type'.
Used by `org-transclusion-blocks--construct-link'.")

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

(defun org-transclusion-blocks--update-content (element content)
  "Replace ELEMENT's content with CONTENT string.

ELEMENT is org-element block context.
CONTENT is string to insert.

For src-block, delegates to `org-babel-update-block-body'.
For other blocks, directly replaces content region.

Called by `org-transclusion-blocks-add'."
  (if (eq (org-element-type element) 'src-block)
      (org-babel-update-block-body content)
    (let* ((bounds (org-transclusion-blocks--get-content-bounds element))
           (beg (car bounds))
           (end (cdr bounds)))
      (unless (and beg end)
        (error "Could not determine content bounds for %s at position %d"
               (org-element-type element)
               (org-element-property :begin element)))
      (delete-region beg end)
      (goto-char beg)
      (insert content))))

;;;; Validator Composition Utilities

(defun org-transclusion-blocks-make-non-empty-validator (component-description)
  "Return validator requiring non-empty string.

COMPONENT-DESCRIPTION is string like \"repository path\" for error messages.

Returns function with signature (VALUE HEADER-KEY TYPE).

Example:

  :validator ,(org-transclusion-blocks-make-non-empty-validator \"file path\")

See `org-transclusion-blocks-register-type' for usage."
  (lambda (value header-key _type)
    (if (or (not (stringp value))
            (string-empty-p value))
        (user-error "Header %s: %s cannot be empty"
                    header-key component-description)
      value)))

(defun org-transclusion-blocks-make-regexp-validator (pattern component-description)
  "Return validator checking VALUE matches PATTERN.

PATTERN is regular expression string.
COMPONENT-DESCRIPTION is string like \"git revision\" for error messages.

Returns function with signature (VALUE HEADER-KEY TYPE).

Example:

  :validator ,(org-transclusion-blocks-make-regexp-validator
               \"^[0-9]\\\\{4\\\\}$\" \"year\")

See `org-transclusion-blocks-register-type' for usage."
  (lambda (value header-key _type)
    (unless (string-match-p pattern value)
      (user-error "Header %s: %s must match pattern %s, got: %S"
                  header-key component-description pattern value))
    value))

(defun org-transclusion-blocks-make-predicate-validator (predicate component-description)
  "Return validator using PREDICATE function.

PREDICATE receives VALUE and returns non-nil if valid.
COMPONENT-DESCRIPTION is string for error messages.

Returns function with signature (VALUE HEADER-KEY TYPE).

Example:

  :validator ,(org-transclusion-blocks-make-predicate-validator
               #'file-exists-p \"file path\")

See `org-transclusion-blocks-register-type' for usage."
  (lambda (value header-key _type)
    (unless (funcall predicate value)
      (user-error "Header %s: invalid %s: %S"
                  header-key component-description value))
    value))

(defun org-transclusion-blocks-compose-validators (&rest validators)
  "Return validator applying VALIDATORS in sequence.

VALIDATORS is list of validator functions.

Composition stops at first error.

Returns function with signature (VALUE HEADER-KEY TYPE).

Example:

  :validator ,(org-transclusion-blocks-compose-validators
               (org-transclusion-blocks-make-non-empty-validator \"path\")
               (org-transclusion-blocks-make-predicate-validator
                #'file-exists-p \"path\"))

See `org-transclusion-blocks-register-type' for usage."
  (lambda (value header-key type)
    (dolist (validator validators value)
      (setq value (funcall validator value header-key type)))))

;;;; Error Formatting Utilities

(defun org-transclusion-blocks-format-validation-error (header-key problem value &rest fix-options)
  "Format validation error message consistently.

HEADER-KEY identifies which header failed.
PROBLEM is string describing what's wrong.
VALUE is the problematic input.
FIX-OPTIONS are strings describing how to fix (one per line).

Returns formatted error string.

Used in custom validators.  See validator composition utilities for examples."
  (concat
   (format "Header %s: %s\n\nValue: %S\n\n" header-key problem value)
   (when fix-options
     (concat "Fix:\n"
             (mapconcat (lambda (fix) (concat "     " fix))
                        fix-options
                        "\n")))))

;;;; Constructor Helpers

(defun org-transclusion-blocks-make-simple-constructor (type separator)
  "Return constructor joining components with SEPARATOR.

TYPE is link type prefix.
SEPARATOR is string separator between components.

Components joined in order they appear in component-spec.

Returns function receiving components plist and returning link string.

Example:

  (org-transclusion-blocks-make-simple-constructor \"mytype\" \"::\")

See `org-transclusion-blocks-register-type' for usage."
  (lambda (components)
    (let ((parts (list (format "%s:" type))))
      (cl-loop for (key val) on components by #'cddr
               when val
               do (push val parts))
      (string-join (nreverse parts) separator))))

;;;; Interaction Checking

(defun org-transclusion-blocks--check-interactions (type params component-spec)
  "Check component interactions, emit warnings/errors.

TYPE is link type symbol.
PARAMS is alist from header parsing.
COMPONENT-SPEC is plist of component metadata.

Returns list of warning strings (empty if no issues).
Signals error for hard conflicts or missing requirements.

Called by `org-transclusion-blocks--pre-validate-headers'."
  (let ((warnings nil)
        (present-components
         (cl-loop for (key meta) on component-spec by #'cddr
                  for header = (plist-get meta :header)
                  when (assoc header params)
                  collect key)))

    ;; Check required components
    (cl-loop for (key meta) on component-spec by #'cddr
             when (plist-get meta :required)
             unless (memq key present-components)
             do (user-error "Type %s: required component %s (header %s) is missing"
                            type key (plist-get meta :header)))

    ;; Check shadowing relationships
    (cl-loop for (key meta) on component-spec by #'cddr
             when (memq key present-components)
             for shadowed-by = (plist-get meta :shadowed-by)
             when shadowed-by
             do (cl-loop for shadow in shadowed-by
                         when (assoc shadow params)
                         do (push (format "Header %s shadows %s; latter will be ignored"
                                          shadow (plist-get meta :header))
                                  warnings)))

    ;; Check dependencies
    (cl-loop for (key meta) on component-spec by #'cddr
             when (memq key present-components)
             for requires = (plist-get meta :requires)
             when requires
             do (cl-loop for req in requires
                         unless (memq req present-components)
                         do (user-error "Component %s requires %s to be present"
                                        (plist-get meta :header)
                                        (plist-get (plist-get component-spec req) :header))))

    ;; Check hard conflicts
    (cl-loop for (key meta) on component-spec by #'cddr
             when (memq key present-components)
             for conflicts = (plist-get meta :conflicts)
             when conflicts
             do (cl-loop for conflict in conflicts
                         when (memq conflict present-components)
                         do (user-error "Components %s and %s cannot be used together"
                                        (plist-get meta :header)
                                        (plist-get (plist-get component-spec conflict) :header))))

    (nreverse warnings)))

;;;; Type Registry API

(defun org-transclusion-blocks-register-type (type component-spec constructor)
  "Register link TYPE with COMPONENT-SPEC and CONSTRUCTOR.

TYPE is symbol naming link type (e.g., \\='my-link).

COMPONENT-SPEC is plist mapping semantic component names to metadata:
  (:semantic-name (:header :header-keyword
                   :validator FUNCTION-OR-NIL
                   :required BOOLEAN-OR-NIL
                   :shadowed-by LIST-OR-NIL
                   :requires LIST-OR-NIL
                   :conflicts LIST-OR-NIL) ...)

CONSTRUCTOR receives plist of validated components,
returns raw link string (without [[ ]] brackets).

Example:

  (org-transclusion-blocks-register-type
   \\='my-type
   \\='(:path (:header :my-path
              :validator my-validator-fn
              :required t))
   (lambda (components)
     (format \"my-type:%s\" (plist-get components :path))))

Overwrites existing TYPE registration if present.

Populates `org-transclusion-blocks--type-components' and
`org-transclusion-blocks--type-constructors'.

Returns TYPE symbol."
  (setf (alist-get type org-transclusion-blocks--type-components)
        component-spec)
  (setf (alist-get type org-transclusion-blocks--type-constructors)
        constructor)
  type)

;;;; Mode Detection

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
  "Warn if PARAMS mixes construction modes.

PARAMS is alist of header arguments.

Emits warnings when `org-transclusion-blocks-show-interaction-warnings' is t.

Called by `org-transclusion-blocks-add'."
  (when org-transclusion-blocks-show-interaction-warnings
    (let ((mode (org-transclusion-blocks--detect-mode params)))
      (pcase mode
        ('direct
         (when (assoc :transclude-type params)
           (display-warning
            'org-transclusion-blocks
            ":transclude header takes priority; :transclude-type ignored"
            :warning)))

        ('type-specific
         (let ((type-symbol (intern (cdr (assoc :transclude-type params)))))
           (when (seq-some (lambda (pair)
                             (let ((key (car pair)))
                               (and (not (memq key '(:transclude-type :transclude-keywords)))
                                    (string-prefix-p ":" (symbol-name key))
                                    (not (cl-loop for (_ meta) on (alist-get type-symbol
                                                                             org-transclusion-blocks--type-components)
                                                  by #'cddr
                                                  thereis (eq key (plist-get meta :header)))))))
                           params)
             (display-warning
              'org-transclusion-blocks
              (format "Type-specific mode active for %s; unrecognized component headers present"
                      type-symbol)
              :warning))))))))

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

Runs validators for each component before content fetching.
Emits warnings via `org-transclusion-blocks--check-interactions'.

Called by `org-transclusion-blocks-add' when typed components detected.

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
      (dolist (header-str all-header-strings)
        (when (string-match ":transclude-type[ \t]+\\([^ \t\n]+\\)" header-str)
          (setq type-keyword (intern (match-string 1 header-str)))))
      (when-let ((component-spec (alist-get type-keyword
                                            org-transclusion-blocks--type-components)))
        (let ((header-validators (make-hash-table :test 'eq)))
          (cl-loop for (semantic-key meta) on component-spec by #'cddr
                   for header-key = (plist-get meta :header)
                   for validator = (plist-get meta :validator)
                   when validator
                   do (puthash header-key validator header-validators))
          (dolist (header-str all-header-strings)
            (when (string-match "^:\\([^ \t]+\\)\\(?:[ \t]+\\(.+\\)\\)?$" header-str)
              (let* ((key (intern (concat ":" (match-string 1 header-str))))
                     (val (or (match-string 2 header-str) ""))
                     (validator (gethash key header-validators)))
                (push (cons key val) parsed-params)
                (when validator
                  (funcall validator val key type-keyword)))))
          (when-let ((warnings (org-transclusion-blocks--check-interactions
                                type-keyword
                                parsed-params
                                component-spec)))
            (when org-transclusion-blocks-show-interaction-warnings
              (display-warning 'org-transclusion-blocks
                               (concat "Component interaction issues:\n"
                                       (mapconcat (lambda (w) (concat "  • " w))
                                                  warnings
                                                  "\n"))
                               :warning))))))
    t))

;;;; Component Extraction

(defun org-transclusion-blocks--extract-type-components (type params)
  "Extract components for TYPE from PARAMS.

TYPE is symbol naming registered link type.
PARAMS is alist of header arguments.

Returns plist of component values or nil if TYPE not registered.

Used by `org-transclusion-blocks--construct-link'."
  (when-let ((component-spec (alist-get type org-transclusion-blocks--type-components)))
    (let ((result nil))
      (cl-loop for (semantic-key meta) on component-spec by #'cddr
               for header-key = (plist-get meta :header)
               for value = (assoc-default header-key params)
               when value
               do (setq result (plist-put result semantic-key
                                          (org-strip-quotes
                                           (if (stringp value) value
                                             (format "%s" value))))))
      (when result result))))

;;;; Link Construction

(defun org-transclusion-blocks--construct-link (params)
  "Construct org link from component headers in PARAMS.

PARAMS is alist of header arguments.

Returns bracket link string [[...]] or nil.

Supports two forms:
1. Direct: :transclude header with complete link
2. Type-specific: :transclude-type with component headers

Called by `org-transclusion-blocks--construct-transclude-line'."
  (or
   ;; Form 1: Direct link
   (when-let ((direct (assoc-default :transclude params)))
     (let ((link-str (org-strip-quotes
                      (if (stringp direct) direct (format "%s" direct)))))
       (if (string-prefix-p "[[" link-str)
           link-str
         (org-link-make-string link-str nil))))

   ;; Form 2: Type-specific components
   (when-let* ((type-raw (assoc-default :transclude-type params))
               (type (if (symbolp type-raw) type-raw
                       (intern (org-strip-quotes
                                (if (stringp type-raw) type-raw
                                  (format "%s" type-raw))))))
               (components (org-transclusion-blocks--extract-type-components type params))
               (constructor (alist-get type org-transclusion-blocks--type-constructors)))
     (when-let ((raw-link (funcall constructor components)))
       (org-link-make-string raw-link nil)))))

(defun org-transclusion-blocks--construct-transclude-line (params)
  "Construct complete #+transclude: line from PARAMS.

PARAMS is alist of header arguments.

Returns string or nil.

Includes :transclude-keywords if present.

Called by `org-transclusion-blocks--params-to-plist'."
  (when-let ((link (org-transclusion-blocks--construct-link params)))
    (let ((keywords (assoc-default :transclude-keywords params)))
      (if keywords
          (format "%s %s" link (org-strip-quotes keywords))
        link))))

(defun org-transclusion-blocks--params-to-plist (params lang)
  "Convert PARAMS alist to org-transclusion keyword plist.

PARAMS is alist of header arguments.
LANG currently unused.

Returns plist for `org-transclusion-add-functions' or nil.

Called by `org-transclusion-blocks-add'."
  (when-let ((transclude-line
              (org-transclusion-blocks--construct-transclude-line params)))
    (with-temp-buffer
      (let ((org-inhibit-startup t))
        (delay-mode-hooks (org-mode))
        (insert "#+transclude: " transclude-line "\n")
        (goto-char (point-min))
        (let ((plist (org-transclusion-keyword-string-to-plist))
              (escape-org (assoc-default :transclude-escape-org params)))
          (when escape-org
            (setq plist (plist-put plist :transclude-escape-org t)))
          plist)))))

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

Called by `org-transclusion-blocks-add'."
  (when-let* ((link-string (plist-get keyword-plist :link))
              (link (org-transclusion-wrap-path-to-link link-string))
              (payload (run-hook-with-args-until-success
                        'org-transclusion-add-functions
                        link
                        keyword-plist)))
    (plist-get payload :src-content)))

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

Called by `org-transclusion-blocks-add'."
  (when (> org-transclusion-blocks-indicator-duration 0)
    (let* ((beg (org-element-property :begin element))
           (end (save-excursion
                  (goto-char beg)
                  (line-end-position)))
           (ov (make-overlay beg end)))
      (overlay-put ov 'before-string
                   (propertize "☑ " 'face '(:foreground "green" :weight bold)))
      (overlay-put ov 'org-transclusion-blocks-indicator t)
      (run-at-time org-transclusion-blocks-indicator-duration
                   nil
                   (lambda (overlay)
                     (when (overlay-buffer overlay)
                       (delete-overlay overlay)))
                   ov)))
  (when org-transclusion-blocks--last-fetch-time
    (message "Content fetched at %s"
             (format-time-string "%H:%M:%S"
                                 org-transclusion-blocks--last-fetch-time))))

;;;; Public Commands

;;;###autoload
(defun org-transclusion-blocks-add ()
  "Fetch and insert transcluded content into block at point.

Supports two mutually exclusive header forms:

1. Direct link mode:
   #+HEADER: :transclude [[TYPE:PATH::SEARCH]]
   #+HEADER: :transclude-keywords \":level 2\"  ; optional

2. Component mode (requires registered type):
   #+HEADER: :transclude-type REGISTERED-TYPE
   #+HEADER: :TYPE-COMPONENT-1 VALUE
   #+HEADER: :TYPE-COMPONENT-2 VALUE

Point must be on or within a supported block type.

For src-blocks, uses Babel for header processing.
For other blocks, parses headers directly.

Runs validators via `org-transclusion-blocks--pre-validate-headers' when
typed components detected.

Applies Org syntax escaping via
`org-transclusion-blocks--escape-org-syntax' when
`org-transclusion-blocks--should-escape-p' returns non-nil.

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

        (when (org-transclusion-blocks--has-typed-components-p element)
          (org-transclusion-blocks--pre-validate-headers element))

        (let* ((params (if (eq type 'src-block)
                           (nth 2 (org-babel-get-src-block-info))
                         (org-transclusion-blocks--parse-headers-direct element)))

               (lang (when (eq type 'src-block)
                       (nth 0 (org-babel-get-src-block-info))))

               (keyword-plist (org-transclusion-blocks--params-to-plist params lang)))

          (org-transclusion-blocks--check-mode-compat params)

          (if (not keyword-plist)
              (progn
                (message "No transclusion headers found")
                nil)

            (if-let ((content (org-transclusion-blocks--fetch-content keyword-plist)))
                (progn
                  (when (org-transclusion-blocks--should-escape-p keyword-plist params)
                    (setq content (org-transclusion-blocks--escape-org-syntax content)))

                  (setq element (org-element-at-point))
                  (org-transclusion-blocks--update-content element content)
                  (setq org-transclusion-blocks--last-fetch-time (current-time))

                  (setq element (org-element-at-point))
                  (let* ((bounds (org-transclusion-blocks--get-content-bounds element))
                         (beg (car bounds))
                         (end (cdr bounds)))
                    (org-transclusion-blocks--apply-timestamp beg end))

                  (org-transclusion-blocks--show-indicator element)
                  (message "Transclusion content inserted into %s block" type)
                  t)

              (message "Failed to fetch transclusion content")
              nil)))))))

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

;;;###autoload
(defun org-transclusion-blocks-describe-type (type)
  "Display comprehensive documentation for TYPE.

TYPE is symbol naming registered link type.

Shows:
- Component specifications
- Validators for each component
- Interaction constraints (required, shadowed-by, requires, conflicts)
- Constructor function
- Usage example

See `org-transclusion-blocks-list-types' for available types."
  (interactive
   (list (intern
          (completing-read "Describe type: "
                           (mapcar (lambda (entry) (symbol-name (car entry)))
                                   org-transclusion-blocks--type-components)
                           nil t))))
  (if-let ((spec (alist-get type org-transclusion-blocks--type-components)))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (insert (format "Type: %s\n\n" type))

          (insert "Components:\n")
          (cl-loop for (key meta) on spec by #'cddr
                   for header = (plist-get meta :header)
                   for validator = (plist-get meta :validator)
                   for required = (plist-get meta :required)
                   do (progn
                        (insert (format "  %-12s -> %-20s" key header))
                        (when required
                          (insert " [REQUIRED]"))
                        (when validator
                          (insert (format " [validator: %s]" validator)))
                        (insert "\n")))

          (insert "\nInteractions:\n")
          (let ((has-interactions nil))
            (cl-loop for (key meta) on spec by #'cddr
                     do (progn
                          (when (plist-get meta :required)
                            (setq has-interactions t)
                            (insert (format "  %s is REQUIRED\n" key)))
                          (when-let ((shadowed (plist-get meta :shadowed-by)))
                            (setq has-interactions t)
                            (insert (format "  %s shadowed by: %s\n"
                                            key
                                            (mapconcat #'symbol-name shadowed ", "))))
                          (when-let ((requires (plist-get meta :requires)))
                            (setq has-interactions t)
                            (insert (format "  %s requires: %s\n"
                                            key
                                            (mapconcat #'symbol-name requires ", "))))
                          (when-let ((conflicts (plist-get meta :conflicts)))
                            (setq has-interactions t)
                            (insert (format "  %s conflicts with: %s\n"
                                            key
                                            (mapconcat #'symbol-name conflicts ", "))))))
            (unless has-interactions
              (insert "  (none)\n")))

          (when-let ((ctor (alist-get type org-transclusion-blocks--type-constructors)))
            (insert (format "\nConstructor: %s\n" ctor)))

          (insert "\nExample usage:\n")
          (insert (format "  #+HEADER: :transclude-type %s\n" type))
          (cl-loop for (key meta) on spec by #'cddr
                   for header = (plist-get meta :header)
                   do (insert (format "  #+HEADER: %s VALUE\n" header)))
          (insert "  #+begin_src elisp\n")
          (insert "  #+end_src\n")))
    (message "Type %s is not registered" type)))

;;;###autoload
(defun org-transclusion-blocks-list-types ()
  "Show all registered types with component counts.

Displays table of registered link types from
`org-transclusion-blocks--type-components'.

Use `org-transclusion-blocks-describe-type' for detailed information about
specific types."
  (interactive)
  (if org-transclusion-blocks--type-components
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (insert "Registered link types:\n\n")
          (cl-loop for (type . spec) in org-transclusion-blocks--type-components
                   for component-count = (/ (length spec) 2)
                   do (insert (format "  %-20s  %d component%s\n"
                                      type
                                      component-count
                                      (if (= component-count 1) "" "s"))))))
    (message "No types registered")))

(provide 'org-transclusion-blocks)
;;; org-transclusion-blocks.el ends here
