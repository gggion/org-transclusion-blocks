;;; org-transclusion-blocks.el --- Transclude into src blocks via headers -*- lexical-binding: t; -*-

;; Author: Gino Cornejo
;; Mantainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/org-transclusion-blocks
;; Keywords: hypermedia vc

;; Package-Version: 0.1.0
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

;; Transclude content into Org src blocks via header arguments.
;; Content persists in file when transclusion inactive.
;;
;; Basic usage with complete link:
;;
;;     #+HEADER: :transclude [[file:path.el::search-term]]
;;     #+begin_src elisp
;;     #+end_src
;;
;; Usage with generic component headers:
;;
;;     #+HEADER: :transclude-type file
;;     #+HEADER: :transclude-path /path/to/file.el
;;     #+HEADER: :transclude-search defun function-name
;;     #+begin_src elisp
;;     #+end_src
;;
;; Register custom link types:
;;
;;     (org-transclusion-blocks-register-type
;;      'orgit-file
;;      '(:repo :orgit-repo
;;        :rev :orgit-rev
;;        :file :orgit-file
;;        :search :orgit-search)
;;      (lambda (components)
;;        (format "orgit-file:%s::%s::%s%s"
;;                (plist-get components :repo)
;;                (plist-get components :rev)
;;                (plist-get components :file)
;;                (if-let ((s (plist-get components :search)))
;;                    (concat "::" s) ""))))
;; NOTE: requires corresponding transclusion backend for link type
;;
;; Supported header arguments:
;;
;; Direct form:
;; - :transclude LINK - Complete org link (highest priority)
;;
;; Type-specific component form (requires registration):
;; - :transclude-type TYPE - Link type symbol
;; - TYPE-specific headers defined in registry
;;
;; Generic component form:
;; - :transclude-type TYPE - Link type (file, id, etc.)
;; - :transclude-path PATH - Path component
;; - :transclude-search SEARCH - Search option after ::
;;
;; Abbreviation form:
;; - :transclude-abbrev ABBREV - Key from `org-link-abbrev-alist'
;; - :transclude-tag TAG - Value for %s substitution
;;
;; Additional properties (work with all forms):
;; - :transclude-thing THING - Thing at point (sexp, defun, paragraph)
;; - :transclude-lines RANGE - Line range (N or N-M)
;;
;; Key commands:
;; - `org-transclusion-blocks-add' - Fetch and insert content at point
;; - `org-transclusion-blocks-add-all' - Process all blocks in buffer/subtree/region
;;
;; Docstrings too long!? WHERE IS THE CODE!?
;; You can use -> https://github.com/gggion/lisp-docstring-toggle

;;; Code:

(require 'org-transclusion)
(require 'org-element)
(require 'ob-core)
(require 'ol)

;;;; Customization

(defgroup org-transclusion-blocks nil
  "Transclude content into Org src blocks via headers."
  :group 'org-transclusion)

(defcustom org-transclusion-blocks-indicator-duration 2.0
  "Seconds to display success indicator after content update.

Shows checkmark overlay on updated src block for this duration.
Set to 0 to disable indicator."
  :type 'number
  :group 'org-transclusion-blocks)

(defcustom org-transclusion-blocks-timestamp-property 'org-transclusion-blocks-fetched
  "Text property name for storing fetch timestamp.

Applied to transcluded content in src block body.
Used for detecting outdated content in future refresh functionality."
  :type 'symbol
  :group 'org-transclusion-blocks)

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

When nil, content is inserted verbatim. Use :transclude-escape-org
header to override per-block.

Does not affect non-Org sources (Python files, text files, etc.)."
  :type 'boolean
  :group 'org-transclusion-blocks
  :package-version '(org-transclusion-blocks . "0.2.0"))

;;;; Internal Variables

(defvar org-transclusion-blocks--last-fetch-time nil
  "Timestamp of most recent successful content fetch.
Buffer-local. Used by indicator to display fetch time.")
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
Queried by validation and extraction functions.")

(defvar org-transclusion-blocks--type-constructors nil
  "Alist mapping link types to constructor functions.

Each entry has form (TYPE . CONSTRUCTOR-FUNC).

CONSTRUCTOR-FUNC receives plist of extracted component values
and returns raw link string (without [[ ]] brackets).

Example entry:
  (orgit-file . #\\='orgit-file--construct-link)

Where orgit-file--construct-link is:
  (lambda (components)
    (format \"orgit-file:%s::%s::%s\"
            (plist-get components :repo)
            (plist-get components :rev)
            (plist-get components :file)))

Populated via `org-transclusion-blocks-register-type'.
Queried by `org-transclusion-blocks--construct-link'.")

;;;; Validator Composition Utilities

(defun org-transclusion-blocks-make-non-empty-validator (component-description)
  "Return validator requiring non-empty string.
COMPONENT-DESCRIPTION is string like \"repository path\" for error messages."
  (lambda (value header-key _type)
    (if (or (not (stringp value))
            (string-empty-p value))
        (user-error "Header %s: %s cannot be empty"
                    header-key component-description)
      value)))

(defun org-transclusion-blocks-make-regexp-validator (pattern component-description)
  "Return validator checking VALUE matches PATTERN.
COMPONENT-DESCRIPTION is string like \"git revision\" for error messages."
  (lambda (value header-key _type)
    (unless (string-match-p pattern value)
      (user-error "Header %s: %s must match pattern %s, got: %S"
                  header-key component-description pattern value))
    value))

(defun org-transclusion-blocks-make-predicate-validator (predicate component-description)
  "Return validator using PREDICATE function.
PREDICATE receives VALUE and returns non-nil if valid.
COMPONENT-DESCRIPTION is string for error messages."
  (lambda (value header-key _type)
    (unless (funcall predicate value)
      (user-error "Header %s: invalid %s: %S"
                  header-key component-description value))
    value))

(defun org-transclusion-blocks-compose-validators (&rest validators)
  "Return validator that applies VALIDATORS in sequence.
Composition stops at first error."
  (lambda (value header-key type)
    (dolist (validator validators value)
      (setq value (funcall validator value header-key type)))))


;;;; Interaction Checking

(defun org-transclusion-blocks--check-interactions (type params component-spec)
  "Check component interactions, emit warnings/errors.
TYPE is link type symbol.
PARAMS is alist from header parsing.
COMPONENT-SPEC is plist of component metadata.

Returns list of warning strings (empty if no issues).
Signals error for hard conflicts or missing requirements."
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

;;;; Error Formatting Utilities

(defun org-transclusion-blocks-format-validation-error (header-key problem value &rest fix-options)
  "Format validation error message consistently.
HEADER-KEY identifies which header failed.
PROBLEM is string describing what's wrong.
VALUE is the problematic input.
FIX-OPTIONS are strings describing how to fix (one per line)."
  (concat
   (format "Header %s: %s\n\nValue: %S\n\n" header-key problem value)
   (when fix-options
     (concat "Fix:\n"
             (mapconcat (lambda (fix) (concat "     " fix))
                        fix-options
                        "\n")))))

;;;; Header Parsing

(defun org-transclusion-blocks--get-property-headers (element)
  "Extract header-args properties from ELEMENT's parent headline."
  (let ((parent (org-element-property :parent element)))
    (while (and parent (not (eq (org-element-type parent) 'headline)))
      (setq parent (org-element-property :parent parent)))
    (when parent
      (org-element-property :HEADER-ARGS parent))))

(defun org-transclusion-blocks--parse-headers-direct (element)
  "Parse headers for non-Babel ELEMENT.
Returns alist of (KEYWORD . VALUE) pairs."
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

(defun org-transclusion-blocks--has-typed-components-p (element)
  "Return non-nil if ELEMENT has :transclude-type header."
  (let ((raw-headers (org-element-property :header element))
        (raw-params (org-element-property :parameters element))
        (property-headers (org-transclusion-blocks--get-property-headers element)))
    (or (and raw-params (string-match-p ":transclude-type" raw-params))
        (seq-some (lambda (h) (string-match-p ":transclude-type" h))
                  raw-headers)
        (and property-headers (string-match-p ":transclude-type" property-headers)))))

(defun org-transclusion-blocks--pre-validate-headers (element)
  "Pre-validate headers in ELEMENT using registered validators."
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

;;;; Block Type Support
(defun org-transclusion-blocks--source-is-org-p (link-string)
  "Return non-nil if LINK-STRING targets Org content.

Detects:
- file: links with .org extension
- id: links (always Org)
- Custom ID links (always Org)
- Org headline links"
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

Checks in priority order:
1. Explicit :transclude-escape-org header
2. Source file type (via `org-transclusion-blocks-escape-org-sources')
3. Default to nil

KEYWORD-PLIST contains :link property with link string.
PARAMS is alist of header arguments."
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
  "Return non-nil if ELEMENT is a block supporting transclusion.

Supports any block with #+begin/#+end delimiters."
  (let ((type (org-element-type element)))
    (and (symbolp type)
         (string-suffix-p "-block" (symbol-name type))
         (not (string-prefix-p "inline-" (symbol-name type))))))

(defun org-transclusion-blocks--get-content-bounds (element)
  "Return (BEG . END) of content area for ELEMENT.

For src-block, uses `org-src--contents-area'.
For other blocks, calculates bounds from element properties."
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

For src-block, delegates to `org-babel-update-block-body'.
For other blocks, directly replaces content region."
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
Returns TYPE symbol."
  (setf (alist-get type org-transclusion-blocks--type-components)
        component-spec)
  (setf (alist-get type org-transclusion-blocks--type-constructors)
        constructor)
  type)

(defun org-transclusion-blocks--extract-type-components (type params)
  "Extract components for TYPE from PARAMS.
Returns plist or nil if TYPE not registered."
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



;;;; Mode Detection

(defun org-transclusion-blocks--detect-mode (params)
  "Determine which construction mode PARAMS represents.
Returns one of: \\='direct, \\='type-specific, or nil."
  (cond
   ((assoc :transclude params) 'direct)
   ((and (assoc :transclude-type params)
         (alist-get (intern (cdr (assoc :transclude-type params)))
                    org-transclusion-blocks--type-components))
    'type-specific)
   (t nil)))

(defun org-transclusion-blocks--check-mode-compat (params)
  "Warn if PARAMS mixes construction modes."
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

;;;; Link Construction

(defun org-transclusion-blocks--construct-link (params)
  "Construct org link from component headers in PARAMS.

PARAMS is alist from `org-babel-get-src-block-info'.

Handles four forms with priority order:

1. Direct link (highest priority):
   :transclude VALUE
   -> Use VALUE as-is

2. Type-specific components (requires type registration):
   :transclude-type TYPE
   TYPE-specific headers (defined in registry)
   -> Extracts components, invokes registered constructor

3. Generic component construction:
   :transclude-type TYPE
   :transclude-path PATH
   :transclude-search SEARCH (optional)
   -> Constructs TYPE:PATH or TYPE:PATH::SEARCH

4. Abbreviation expansion:
   :transclude-abbrev ABBREV
   :transclude-tag TAG
   -> Expands via `org-link-expand-abbrev'

Returns bracket link string [[...]] or nil if construction failed.

Direct form example:
  (:transclude . \"[[file:/foo.el]]\")
  -> \"[[file:/foo.el]]\"

Type-specific form example (with orgit-file registered):
  (:transclude-type . orgit-file)
  (:orgit-repo . \"~/code/proj\")
  (:orgit-rev . \"main\")
  (:orgit-file . \"core.el\")
  -> \"[[orgit-file:~/code/proj::main::core.el]]\"

Generic component form example:
  (:transclude-type . file)
  (:transclude-path . \"/foo.el\")
  (:transclude-search . \"defun bar\")
  -> \"[[file:/foo.el::defun bar]]\"

Abbreviation form example:
  (:transclude-abbrev . gh)
  (:transclude-tag . user/repo)
  With org-link-abbrev-alist: ((\"gh\" . \"https://github.com/%s\"))
  -> \"[[https://github.com/user/repo]]\"

Uses `org-link-make-string' for bracket wrapping and escaping."
  (or
   ;; Form 1: Direct link - use as-is (highest priority)
   (when-let ((direct (assoc-default :transclude params)))
     (let ((link-str (org-strip-quotes
                      (if (stringp direct) direct (format "%s" direct)))))
       ;; Ensure brackets present
       (if (string-prefix-p "[[" link-str)
           link-str
         (org-link-make-string link-str nil))))

   ;; Form 2: Type-specific components (if type registered)
   (when-let* ((type-raw (assoc-default :transclude-type params))
               (type (if (symbolp type-raw) type-raw
                       (intern (org-strip-quotes
                                (if (stringp type-raw) type-raw
                                  (format "%s" type-raw))))))
               (components (org-transclusion-blocks--extract-type-components type params))
               (constructor (alist-get type org-transclusion-blocks--type-constructors)))
     (when-let ((raw-link (funcall constructor components)))
       (org-link-make-string raw-link nil)))

   ;; Form 3: Generic component construction
   (when-let ((type (assoc-default :transclude-type params))
              (path (assoc-default :transclude-path params)))
     (let* ((type-str (org-strip-quotes
                       (if (stringp type) type (format "%s" type))))
            (path-str (org-strip-quotes
                       (if (stringp path) path (format "%s" path))))
            (search (assoc-default :transclude-search params))
            (search-str (when search
                          (org-strip-quotes
                           (if (stringp search) search (format "%s" search)))))
            (raw-link (if search-str
                          (format "%s:%s::%s" type-str path-str search-str)
                        (format "%s:%s" type-str path-str))))
       (org-link-make-string raw-link nil)))

   ;; Form 4: Abbreviation expansion
   (when-let ((abbrev (assoc-default :transclude-abbrev params))
              (tag (assoc-default :transclude-tag params)))
     (let* ((abbrev-str (org-strip-quotes
                         (if (stringp abbrev) abbrev (format "%s" abbrev))))
            (tag-str (org-strip-quotes
                      (if (stringp tag) tag (format "%s" tag))))
            (abbrev-link (format "%s::%s" abbrev-str tag-str))
            (expanded (org-link-expand-abbrev abbrev-link)))
       (org-link-make-string expanded nil)))))

(defun org-transclusion-blocks--construct-transclude-line (params)
  "Construct complete #+transclude: line from PARAMS.
Returns string or nil."
  (when-let ((link (org-transclusion-blocks--construct-link params)))
    (let ((keywords (assoc-default :transclude-keywords params)))
      (if keywords
          (format "%s %s" link (org-strip-quotes keywords))
        link))))

(defun org-transclusion-blocks--params-to-plist (params lang)
  "Convert PARAMS alist to org-transclusion keyword plist.
LANG currently unused."
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
  "Escape Org syntax in CONTENT string."
  (with-temp-buffer
    (insert content)
    (org-escape-code-in-region (point-min) (point-max))
    (buffer-string)))

;;;; Content Fetching

(defun org-transclusion-blocks--fetch-content (keyword-plist)
  "Fetch transcluded content using KEYWORD-PLIST.

KEYWORD-PLIST is plist with :link property and optional
:thing-at-point, :lines properties.

Delegates to `org-transclusion-add-functions' to obtain payload.
Extracts :src-content from payload.

Returns content string or nil if fetch failed.

Does not create transclusion overlays or modify buffers.
Only extracts content for separate insertion.

Uses `org-transclusion-wrap-path-to-link' to convert link string
to org-element link object before calling payload hooks.

Note: Does not include :src property in keyword-plist to prevent
org-transclusion from wrapping content in #+begin_src/#+end_src
delimiters. Content is fetched as raw text for direct insertion
into existing src block body."
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

Adds `org-transclusion-blocks-timestamp-property' with current time.
Used for future refresh detection to identify outdated content."
  (add-text-properties beg end
                       (list org-transclusion-blocks-timestamp-property
                             (current-time))))

(defun org-transclusion-blocks--show-indicator (element)
  "Display success indicator overlay on block ELEMENT.

Shows checkmark for `org-transclusion-blocks-indicator-duration' seconds.
Displays fetch timestamp in echo area if available.

ELEMENT is org-element block context."
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

See `org-transclusion-blocks-list-types' for available types.

Point must be on or within a supported block type.

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


          ;; Check mode compatibility
          (org-transclusion-blocks--check-mode-compat params)

          (if (not keyword-plist)
              (progn
                (message "No transclusion headers found")
                nil)

            (if-let ((content (org-transclusion-blocks--fetch-content keyword-plist)))
                (progn
                  (setq element (org-element-at-point))

                  ;; Apply escaping if needed
                  (when (org-transclusion-blocks--should-escape-p keyword-plist params)
                    (setq content (org-transclusion-blocks--escape-org-syntax content)))

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




;;;; Built-in Type Registrations

;; Register orgit-file type
;; (org-transclusion-blocks-register-type
;;  'orgit-file
;;  '(:repo :orgit-repo
;;    :rev :orgit-rev
;;    :file :orgit-file
;;    :search :orgit-search)
;;  (lambda (components)
;;    "Construct orgit-file link from COMPONENTS.

;; COMPONENTS is plist with :repo, :rev, :file, and optional :search.

;; Returns link string: orgit-file:REPO::REV::FILE or
;; orgit-file:REPO::REV::FILE::SEARCH"
;;    (let ((repo (expand-file-name (plist-get components :repo)))  ; Expand ~
;;          (rev (plist-get components :rev))
;;          (file (plist-get components :file))
;;          (search (plist-get components :search)))
;;      (if search
;;          (format "orgit-file:%s::%s::%s::%s" repo rev file search)
;;        (format "orgit-file:%s::%s::%s" repo rev file)))))

;; Register id type
;; (org-transclusion-blocks-register-type
;;  'id
;;  '(:uuid :transclude-id)
;;  (lambda (components)
;;    "Construct id link from COMPONENTS.

;; COMPONENTS is plist with :uuid.

;; Returns link string: id:UUID"
;;    (format "id:%s" (plist-get components :uuid))))

;;;; Planned Features

;; TODO 2025-11-01: Refresh functionality deferred to v0.3.0.
;; Planned implementation:
;; - `org-transclusion-blocks-refresh' - refresh block at point
;; - `org-transclusion-blocks-refresh-all' - refresh all blocks
;; - Compare `org-transclusion-blocks-timestamp-property' with file mtime
;; - Indicate outdated blocks via overlay or face property
;; - Optional auto-refresh on file open via find-file-hook

;; TODO 2025-11-15: Diff functionality deferred to v0.3.0.
;; Planned implementation:
;; - `org-transclusion-blocks-diff' - show diff at point
;; - Compare cached content (current block body) with live fetch
;; - Use ediff or diff-mode for presentation
;; - Accept/reject update workflow
;; - Batch diff mode for reviewing multiple outdated blocks

;; todo 2025-11-15: Transclusion controls deferred to v0.3.0.
;; Planned implementation:
;; - `org-transclusion-blocks-set-lines' - sets :lines N1-N2 to desired numbers
;; - `org-transclusion-blocks-set-lines-range' - :lines (N1-N2)=x
;; - `org-transclusion-blocks-expand-lines-range' - :lines (N1+x)-(N2+x)
;; - `org-transclusion-blocks-shrink-lines-range' - :lines (N1-x)-(N2-x)
;; - `org-transclusion-blocks-scroll-up' - :lines (N1+x)-(N2-x)
;; - `org-transclusion-blocks-scroll-down' - :lines (N1-x)-(N2+x)
;; - all functions should immediatly trigger refresh

;; TODO 2025-11-28: Time Machine functionality deferred to v0.4.0.
;; Planned implementation:
;; - `org-transclusion-next/previous-version'
;; - step through versions of transclusion range at different revs
;; - Requires orgit-file/org-transclusion-git
;; - Can go to next/previous or specific rev selected interactively

;; TODO 2025-12-05: Time Machine functionality deferred to v0.5.0.
;; Planned implementation:
;; - `org-transclusion-blocks-dnext/previous-version'
;; - step through versions of transclusion range at different revs
;; - Requires orgit-file and org-transclusion-git (unless transient branch)
;; - Can go to next/previous or specific rev selected interactively

;; TODO 2025-12-05: tree-sitter/imenu integration
;; Planned implementation:
;; - `org-transclusion-blocks-select-element'
;; - interactively set transclusion target from tree-sitter candidates
;; - interactively set transclusion target from imenu candidates
;; - scrolling through candidates previews contents in src block
;; - working imenu prototype present in `org-transclusion-utils' package

;; TODO 2025-12-05: Additional link type registrations welcomed.
;; Users can register custom types via:
;;   (org-transclusion-blocks-register-type TYPE COMPONENTS CONSTRUCTOR)
;;
;; Example for hypothetical git-link type:
;;   (org-transclusion-blocks-register-type
;;    'git
;;    '(:dir :git-dir
;;      :object :git-object)
;;    (lambda (c)
;;      (format "git:%s::%s"
;;              (plist-get c :dir)
;;              (plist-get c :object))))
;;
;; Then use:
;;   #+HEADER: :transclude-type git
;;   #+HEADER: :git-dir /path/to/.git
;;   #+HEADER: :git-object HEAD:src/main.c

;; NOTE 2025-12-05: Transclusion compatibility (v?)
;; Planned implementation:
;; - transclusion-blocks should support same functionality as main package
;;    - TODO add org-transclusion-refresh
;;    - TODO add org-transclusion-open-source
;;    - TODO add org-transclusion-remove
;;    - TODO add org-transclusion text properties
;;    - TODO add org-transclusion-detach (current default behavior) - add attach?
;;    - TODO add org-transclusion-live-sync (might complicate things - v1.0.0?)

(provide 'org-transclusion-blocks)
;;; org-transclusion-blocks.el ends here
