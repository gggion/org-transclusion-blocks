;;; org-transclusion-blocks-types.el --- Type registry for component-based links -*- lexical-binding: t; -*-

;; Author: Gino Cornejo
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/org-transclusion-blocks

;; This file is part of org-transclusion-blocks.

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

;; Type registration system for component-based org-transclusion links.
;;
;; This file provides the machinery for registering link types with
;; semantic components, validators, and constructors.  Types registered
;; here are recognized by `org-transclusion-blocks-add' during header
;; parsing and link construction.
;;
;; Basic registration:
;;
;;     (org-transclusion-blocks-register-type
;;      'my-type
;;      '(:component (:header :my-component
;;                    :validator my-validator-fn
;;                    :required t
;;                    :expand-vars t))
;;      (lambda (components)
;;        (format "my-type:%s" (plist-get components :component))))
;;
;; Component metadata properties:
;;
;; - :header - Header keyword (required)
;; - :validator - Validation function (optional)
;; - :required - Whether component is mandatory (optional)
;; - :shadowed-by - List of components that shadow this one (optional)
;; - :requires - List of required companion components (optional)
;; - :conflicts - List of mutually exclusive components (optional)
;; - :expand-vars - Whether to expand variable references (optional)
;;
;; Variable expansion:
;;
;; When :expand-vars is t, the component's header value supports
;; variable references via $varname or bare varname patterns:
;;
;;     (org-transclusion-blocks-register-type
;;      'my-type
;;      '(:path (:header :my-path
;;               :expand-vars t))
;;      #'my-constructor)
;;
;; Usage:
;;     #+HEADER: :var dir="~/project"
;;     #+HEADER: :transclude-type my-type
;;     #+HEADER: :my-path $dir/file.txt
;;
;; Generic transclusion headers (:transclude, :transclude-lines, etc.)
;; always support expansion regardless of type registration.
;;
;; Validator utilities:
;;
;; - `org-transclusion-blocks-make-non-empty-validator'
;; - `org-transclusion-blocks-make-regexp-validator'
;; - `org-transclusion-blocks-make-predicate-validator'
;; - `org-transclusion-blocks-compose-validators'
;;
;; Introspection:
;;
;; - `org-transclusion-blocks-describe-type'
;; - `org-transclusion-blocks-list-types'
;;
;; See Info node `(org-transclusion-blocks) Type Registration' (NOT YET).

;;; Code:

(require 'org-transclusion)
(require 'org-element)
(require 'ol)
(require 'org-macs)
(require 'cl-lib)

;;;; Type Registry State

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
               #\\='file-exists-p \"file path\")

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
                #\\='file-exists-p \"path\"))

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
      (cl-loop for (_key val) on components by #'cddr
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

;;;; Component Extraction

(defun org-transclusion-blocks--get-all-registered-headers ()
  "Return alist mapping headers to their owning types.

Each entry: (HEADER-KEYWORD . TYPE-SYMBOL)

Used to detect cross-type header contamination.

Example return value:
  ((:orgit-repo . orgit-file)
   (:orgit-rev . orgit-file)
   (:orgit-file . orgit-file)
   (:orgit-search . orgit-file)
   (:file-path . file)
   (:file-search . file))"
  (let ((header-registry nil))
    (cl-loop for (type . component-spec) in org-transclusion-blocks--type-components
             do (cl-loop for (_ meta) on component-spec by #'cddr
                         for header = (plist-get meta :header)
                         do (push (cons header type) header-registry)))
    (nreverse header-registry)))

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

Processing order:
1. Construct base link via direct or type-specific path
2. Append :transclude-lines or :transclude-thing if present
3. Append :transclude-keywords if present

Header values override any conflicting specifications in keywords.

Called by `org-transclusion-blocks--params-to-plist'."
  (when-let ((link (org-transclusion-blocks--construct-link params)))
    (let* ((lines-spec (org-transclusion-blocks--get-lines-spec params))
           (thing-spec (org-transclusion-blocks--get-thing-spec params))
           (keywords (assoc-default :transclude-keywords params))
           ;; Remove conflicting specs from keywords if present
           (cleaned-keywords
            (when keywords
              (let ((kw keywords))
                ;; Remove :lines if we have :transclude-lines header
                (when lines-spec
                  (when-let ((match (org-transclusion-blocks--extract-from-keywords
                                     kw ":lines")))
                    (setq kw (concat (substring kw 0 (car match))
                                     (substring kw (cdr match))))))
                ;; Remove :thing-at-point if we have :transclude-thing header
                (when thing-spec
                  (when-let ((match (org-transclusion-blocks--extract-from-keywords
                                     kw ":thing-at-point")))
                    (setq kw (concat (substring kw 0 (car match))
                                     (substring kw (cdr match))))))
                ;; Clean up double spaces
                (replace-regexp-in-string "  +" " " kw)))))
      ;; Build final keyword line
      (concat link
              (when lines-spec
                (format " :lines %s" (org-strip-quotes lines-spec)))
              (when thing-spec
                (format " :thing-at-point %s" (org-strip-quotes thing-spec)))
              (when (and cleaned-keywords
                         (not (string-empty-p (string-trim cleaned-keywords))))
                (format " %s" (org-strip-quotes cleaned-keywords)))))))

(defun org-transclusion-blocks--params-to-plist (params)
  "Convert PARAMS alist to org-transclusion keyword plist.

PARAMS is alist of header arguments.

Returns plist for `org-transclusion-add-functions' or nil.

Stores constructed link in :constructed-link property for later
retrieval by extension functions.

Called by `org-transclusion-blocks-add'."
  (when-let ((transclude-line
              (org-transclusion-blocks--construct-transclude-line params)))
    (with-temp-buffer
      (let ((org-inhibit-startup t))
        (delay-mode-hooks (org-mode))
        (insert "#+transclude: " transclude-line "\n")
        (goto-char (point-min))
        (let* ((plist (org-transclusion-keyword-string-to-plist))
               (link (plist-get plist :link))
               (escape-org (assoc-default :transclude-escape-org params)))
          ;; Store constructed link for extensions
          (setq plist (plist-put plist :constructed-link link))
          (when escape-org
            (setq plist (plist-put plist :transclude-escape-org t)))
          plist)))))

;;;; Generic Header Extraction
;; Note: These functions are referenced by org-transclusion-blocks.el
;; for generic header processing.  They remain here because they
;; extract values from the params alist, which is the input to
;; link construction.

(defun org-transclusion-blocks--get-lines-spec (params)
  "Extract :lines specification from PARAMS alist.

Returns string value of :transclude-lines header or nil.
Does not parse the specification syntax."
  (assoc-default :transclude-lines params))

(defun org-transclusion-blocks--get-thing-spec (params)
  "Extract :thing-at-point specification from PARAMS alist.

Returns string value of :transclude-thing header or nil.
Also checks :transclude-thingatpt alias."
  (or (assoc-default :transclude-thing params)
      (assoc-default :transclude-thingatpt params)))

(defun org-transclusion-blocks--extract-from-keywords (keywords prop-name)
  "Extract PROP-NAME value from KEYWORDS string.

KEYWORDS is :transclude-keywords header value.
PROP-NAME is property name like \":lines\" or \":thing-at-point\".

Returns cons (START . END) of match region or nil.
START/END are buffer positions in KEYWORDS string for replacement."
  (when (and keywords (stringp keywords))
    (with-temp-buffer
      (insert keywords)
      (goto-char (point-min))
      (when (re-search-forward
             (concat (regexp-quote prop-name)
                     "\\s-+\\([^ \t\n]+\\)")
             nil t)
        (cons (match-beginning 0) (match-end 0))))))

;;;; User Commands

;;;###autoload
(defun org-transclusion-blocks-describe-type (type)
  "Display comprehensive documentation for TYPE.

TYPE is symbol naming registered link type.

Shows:
- Component specifications
- Validators for each component
- Variable expansion support
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
                   for expand-vars = (plist-get meta :expand-vars)
                   do (progn
                        (insert (format "  %-12s -> %-20s" key header))
                        (when required
                          (insert " [REQUIRED]"))
                        (when expand-vars
                          (insert " [expand-vars]"))
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
          (cl-loop for (_key meta) on spec by #'cddr
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

(provide 'org-transclusion-blocks-types)
;;; org-transclusion-blocks-types.el ends here
