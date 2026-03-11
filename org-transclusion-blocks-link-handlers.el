;;; org-transclusion-blocks-link-handlers.el --- Link type resolvers -*- lexical-binding: t; -*-

;; Author: Gino Cornejo
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; URL: https://github.com/gggion/org-transclusion-blocks
;; Keywords: hypermedia vc

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

;; Extends org-transclusion to handle link types beyond file: and id:.
;;
;; org-transclusion's built-in handlers only resolve file: and id: links.
;; This file provides a unified resolution mechanism that converts any
;; link type with a :follow function into transcludable content.
;;
;; Resolution strategy:
;;
;; 1. Call the link type's :follow function from `org-link-parameters'
;; 2. Inspect the resulting buffer:
;;    - File-backed buffer: re-dispatch as file: link (preserves
;;      upstream :lines and :thing-at-point handling)
;;    - Generated buffer: capture content directly with internal
;;      line extraction
;;
;; Zero-configuration usage for types in
;; `org-transclusion-blocks-follow-types' (default: "info"):
;;
;;     #+HEADER: :transclude [[info:emacs]]
;;     #+HEADER: :transclude-lines 1-30
;;     #+begin_src text
;;     #+end_src
;;
;; Other follow-based types (help, denote, var, etc) require adding
;; to `org-transclusion-blocks-follow-types' by the user.
;;
;; Explicit resolver registry for non-follow-based resolution:
;;
;;     (org-transclusion-blocks-register-link-handler
;;      "my-type"
;;      (lambda (path) (my-resolve-to-file path)))
;;
;; Key functions:
;; - `org-transclusion-blocks-register-link-handler' - Register resolver
;; - `org-transclusion-blocks-follow-types' - Defcustom for types

;;; Code:

(require 'org-transclusion)

;;;; Customization

(defcustom org-transclusion-blocks-follow-types
  '("info")
  "Link types resolved via their :follow function for transclusion.

Each entry is a link type string.  When a transclusion targets one of
these types, the handler calls the type's :follow function from
`org-link-parameters' and inspects the resulting buffer:

- File-backed buffer (variable `buffer-file-name' non-nil): re-dispatch as a
  file: link, preserving upstream :lines and :thing-at-point handling
  via `org-transclusion-add-src-lines'.

- Generated buffer (variable `buffer-file-name' nil): capture buffer content
  directly and apply :lines extraction internally.

Types whose :follow function opens an external application (browser,
mail client) are not supported.  Use
`org-transclusion-blocks-register-link-handler' for types needing
custom resolution without calling :follow.

Handled by `org-transclusion-blocks--add-by-follow' at depth -4 in
`org-transclusion-add-functions'.  Explicit resolver handlers at
depth -5 take precedence.

Queried by `org-transclusion-blocks--source-is-org-p' for Org
escape detection."
  :type '(repeat string)
  :group 'org-transclusion-blocks
  :package-version '(org-transclusion-blocks . "0.5.0"))

;;;; Explicit Resolver Registry

(defvar org-transclusion-blocks--link-handlers nil
  "Alist mapping link type strings to resolver functions.

Each entry: (TYPE-STRING . RESOLVER-FUNCTION)

RESOLVER-FUNCTION receives link path string (:: search already stripped)
and returns absolute file path, or nil.

Populated by `org-transclusion-blocks-register-link-handler'.
Queried by `org-transclusion-blocks--add-by-link-handler'.
Queried by `org-transclusion-blocks--source-is-org-p'.")

(defun org-transclusion-blocks-register-link-handler (type resolver)
  "Register RESOLVER for link TYPE in explicit resolver registry.

TYPE is a link type string (e.g. \"denote\").
RESOLVER receives link path string, returns absolute file path or nil.
Check package availability inside RESOLVER via `fboundp'.

Takes precedence over `org-transclusion-blocks-follow-types'.
Populates `org-transclusion-blocks--link-handlers'."
  (setf (alist-get type org-transclusion-blocks--link-handlers
                   nil nil #'string=)
        resolver))

;;;; Path Splitting

(defun org-transclusion-blocks--split-path-search (link)
  "Split path and search option from LINK element.

LINK is an `org-element' link object.

`org-element' does not parse :: separators for non-file link types.
The entire \"path::search\" string goes into :path with
:search-option nil.

Return cons (PATH . SEARCH-OPTION) where SEARCH-OPTION may be nil.

For file: links where `org-element' already splits correctly, return
the existing :path and :search-option values.

Called by `org-transclusion-blocks--add-by-follow',
`org-transclusion-blocks--add-by-link-handler', and
`org-transclusion-blocks--add-org-id-with-search'."
  (let ((full-path (org-element-property :path link))
        (search-option (org-element-property :search-option link)))
    (if search-option
        ;; org-element already split (file: links)
        (cons full-path search-option)
      ;; Manual split for non-file link types
      (if (string-match "\\`\\(.*?\\)::\\(.+\\)\\'" full-path)
          (cons (match-string 1 full-path)
                (match-string 2 full-path))
        (cons full-path nil)))))

;;;; Follow-Based Resolution

(defun org-transclusion-blocks--invoke-follow (type path)
  "Call :follow function for link TYPE with PATH string.

TYPE is a link type string registered in `org-link-parameters'.
PATH is the link path with :: search option already stripped.

Return the resulting buffer, or nil if :follow is not registered,
signals an error, or the resulting buffer is the same as the
buffer before invocation (indicating :follow had no effect).

Try calling with two arguments (PATH ARG) first, falling back to
one argument (PATH) for :follow functions that do not accept an
optional prefix argument.

Uses `save-window-excursion' to contain window side effects.

Called by `org-transclusion-blocks--add-by-follow' and
`org-transclusion-blocks--source-is-org-p'."
  (when-let* ((follow-fn (org-link-get-parameter type :follow)))
    (let ((orig-buf (current-buffer)))
      (condition-case nil
          (save-window-excursion
            (save-excursion
              (condition-case nil
                  (funcall follow-fn path nil)
                (wrong-number-of-arguments
                 (funcall follow-fn path)))
              (let ((result-buf (current-buffer)))
                ;; Return nil if :follow did not switch buffers
                (unless (eq result-buf orig-buf)
                  result-buf))))
        (error nil)))))

(defun org-transclusion-blocks--extract-lines-from-buffer (buf plist)
  "Extract content from BUF according to PLIST specifications.

BUF is a buffer (possibly generated, not file-backed).
PLIST is the keyword plist containing :lines and :thing-at-point.

Apply :lines extraction when present.  :thing-at-point is not
supported for generated buffers because the point position after
:follow is not meaningful for semantic unit extraction.

Return payload plist with :src-content, :src-buf, :src-beg,
:src-end, compatible with `org-transclusion-add-functions' protocol.

Called by `org-transclusion-blocks--add-by-follow'."
  (with-current-buffer buf
    (let* ((lines-spec (plist-get plist :lines))
           (range (when lines-spec (split-string lines-spec "-")))
           (lbeg (if range (string-to-number (car range)) 0))
           (lend (if range (string-to-number (cadr range)) 0))
           (beg (if (> lbeg 0)
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- lbeg))
                      (point))
                  (point-min)))
           (end (if (> lend 0)
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- lend))
                      (end-of-line)
                      (min (1+ (point)) (point-max)))
                  (point-max)))
           (content (buffer-substring-no-properties beg end)))
      (list :src-content content
            :src-buf buf
            :src-beg beg
            :src-end end
            :tc-type "follow-capture"))))

(defun org-transclusion-blocks--add-by-follow (link plist)
  "Resolve LINK via :follow function and return payload for PLIST.

LINK is an `org-element' link object.
PLIST is keyword plist passed unchanged to handlers.

Handle link types listed in `org-transclusion-blocks-follow-types'.
Return payload plist, or nil if link type is not listed.

Buffer cleanup is NOT performed here.  See implementation notes.

Installed at depth -4 in `org-transclusion-add-functions'."
  ;; This function is on a public hook where callers expect :src-buf
  ;; to remain live for overlay creation and live-sync tracking.
  ;; Buffer cleanup is the responsibility of the calling site:
  ;; - `org-transclusion-blocks--fetch-content' for block transclusions
  ;; - `org-transclusion--add' for standard #+transclude: usage
  ;;
  ;; Resolution strategy:
  ;; 1. Split :: from path via `org-transclusion-blocks--split-path-search'
  ;; 2. Call :follow via `org-transclusion-blocks--invoke-follow'
  ;; 3. Inspect `buffer-file-name' on resulting buffer:
  ;;    - Non-nil: re-dispatch via `org-transclusion-blocks--redispatch-as-file'
  ;;    - Nil: capture via `org-transclusion-blocks--extract-lines-from-buffer'
  (let ((type (org-element-property :type link)))
    (when (member type org-transclusion-blocks-follow-types)
      (pcase-let ((`(,path . ,search)
                   (org-transclusion-blocks--split-path-search link)))
        (let ((result-buf (org-transclusion-blocks--invoke-follow type path)))
          (cond
           ;; :follow did not produce a buffer
           ((not result-buf)
            (message
             "org-transclusion-blocks: %s link %S could not be resolved"
             type path)
            '(:src-content nil))

           ;; File-backed buffer: re-dispatch as file: link
           ((buffer-file-name result-buf)
            (org-transclusion-blocks--redispatch-as-file
             (buffer-file-name result-buf) search plist))

           ;; Generated buffer: capture content directly
           (t
            (org-transclusion-blocks--extract-lines-from-buffer
             result-buf plist))))))))

;;;; Explicit Resolver Dispatch

(defun org-transclusion-blocks--add-by-link-handler (link plist)
  "Resolve LINK via explicit resolver registry and re-dispatch for PLIST.

LINK is an `org-element' link object.
PLIST is keyword plist passed unchanged to handlers.

Check `org-transclusion-blocks--link-handlers' for a resolver matching
the link type.  Return payload plist, or nil if no handler matches.

Uses `org-transclusion-blocks--split-path-search' for :: splitting.
Delegates to `org-transclusion-blocks--redispatch-as-file'.

Installed at depth -5 in `org-transclusion-add-functions'."
  (let* ((type (org-element-property :type link))
         (resolver (alist-get type org-transclusion-blocks--link-handlers
                              nil nil #'string=)))
    (when resolver
      (pcase-let ((`(,path . ,search)
                   (org-transclusion-blocks--split-path-search link)))
        (let ((file-path (condition-case err
                             (funcall resolver path)
                           (error
                            (message
                             "org-transclusion-blocks: %s resolver error: %s"
                             type (error-message-string err))
                            nil))))
          (if (not (and file-path (file-exists-p file-path)))
              (progn
                (message
                 "org-transclusion-blocks: %s link %S could not be resolved"
                 type path)
                '(:src-content nil))
            (org-transclusion-blocks--redispatch-as-file
             file-path search plist)))))))

;;;; Re-Dispatch Utility

(defun org-transclusion-blocks--redispatch-as-file (file-path search plist)
  "Construct file: link to FILE-PATH with SEARCH and re-dispatch for PLIST.

FILE-PATH is absolute path to target file.
SEARCH is search option string or nil.
PLIST is keyword plist passed unchanged to handlers.

Return payload plist from the successful handler, or nil.

Called by `org-transclusion-blocks--add-by-follow',
`org-transclusion-blocks--add-by-link-handler', and
`org-transclusion-blocks--add-org-id-with-search'."
  (let* ((link-target (if search
                          (concat "file:" file-path "::" search)
                        (concat "file:" file-path)))
         (file-link-str (org-link-make-string link-target))
         (file-link (org-transclusion-wrap-path-to-link file-link-str)))
    (run-hook-with-args-until-success
     'org-transclusion-add-functions
     file-link
     plist)))

;;;; id: Links with Search Options

(defun org-transclusion-blocks--add-org-id-with-search (link plist)
  "Handle id: LINK containing :: search option for PLIST.

LINK is an `org-element' link object.
PLIST is keyword plist passed unchanged to handlers.

Return nil for id: links without :: to let upstream handle those.
Return (:src-content nil) on UUID lookup failure to prevent
fallthrough to upstream `org-transclusion-add-org-id'.

Uses `org-transclusion-blocks--split-path-search' for :: splitting.
Resolves UUID via `org-id-find'.
Delegates to `org-transclusion-blocks--redispatch-as-file'.

Installed at depth -10 in `org-transclusion-add-functions'."
  (when (string= "id" (org-element-property :type link))
    (pcase-let ((`(,id . ,search)
                 (org-transclusion-blocks--split-path-search link)))
      (when search
        (let ((found (ignore-errors (org-id-find id))))
          (if (not found)
              (progn
                (message
                 "No transclusion done for ID %s at point %d, line %d"
                 id (point) (org-current-line))
                '(:src-content nil))
            (org-transclusion-blocks--redispatch-as-file
             (car found) search plist)))))))

;;;; Installation

(defun org-transclusion-blocks--install-link-handlers ()
  "Install link handler functions into `org-transclusion-add-functions'.

Prepend `org-transclusion-blocks--add-org-id-with-search' at depth -10
before all other handlers to fix id: :: search option splitting.

Install `org-transclusion-blocks--add-by-link-handler' at depth -5
for explicit resolver registry.

Install `org-transclusion-blocks--add-by-follow' at depth -4
for unified follow-based resolution.

Both resolver handlers must precede `org-transclusion-add-src-lines'
\(depth 0), which does not check link type and would attempt to open
non-file paths directly via `find-file-noselect' when :lines is in
the plist.

Dedicated third-party handlers at depth 0 handle file: links
produced by re-dispatch.  Idempotent; safe to call multiple times.

Called at load time by the top-level form at end of file."
  ;; Remove old handler if present (renamed from generic-follow)
  (remove-hook 'org-transclusion-add-functions
               'org-transclusion-blocks--add-by-generic-follow)
  (unless (memq 'org-transclusion-blocks--add-org-id-with-search
                org-transclusion-add-functions)
    (add-hook 'org-transclusion-add-functions
              #'org-transclusion-blocks--add-org-id-with-search
              -10))
  (unless (memq 'org-transclusion-blocks--add-by-link-handler
                org-transclusion-add-functions)
    (add-hook 'org-transclusion-add-functions
              #'org-transclusion-blocks--add-by-link-handler
              -5))
  (unless (memq 'org-transclusion-blocks--add-by-follow
                org-transclusion-add-functions)
    (add-hook 'org-transclusion-add-functions
              #'org-transclusion-blocks--add-by-follow
              -4)))

;; Install on load
(org-transclusion-blocks--install-link-handlers)

(provide 'org-transclusion-blocks-link-handlers)
;;; org-transclusion-blocks-link-handlers.el ends here
