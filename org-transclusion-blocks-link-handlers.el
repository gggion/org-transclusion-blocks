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
;; This file adds two resolution mechanisms that convert any file-backed
;; link type to a file: link and re-dispatch, preserving :lines,
;; :thing-at-point, and all other transclusion keywords.
;;
;; Generic follow-based resolution (zero configuration):
;;
;;     (add-to-list 'org-transclusion-blocks-generic-link-types "my-type")
;;
;; Works for any link type whose :follow function opens a file buffer.
;; Defaults to ("denote" "roam").
;;
;; Explicit resolver registry (for non-file-backed links):
;;
;;     (org-transclusion-blocks-register-link-handler
;;      "my-type"
;;      (lambda (path) (my-resolve-to-file path)))
;;
;; Also fixes id: links with ::search options.  `org-id-open' splits
;; :: from the path internally, but `org-transclusion-add-org-id'
;; passes the unsplit :path directly to `org-id-find', which fails.
;;
;; Key functions:
;; - `org-transclusion-blocks-register-link-handler' - Register resolver
;; - `org-transclusion-blocks-generic-link-types' - Defcustom for types

;;; Code:

(require 'org-transclusion)

;;;; Customization

(defcustom org-transclusion-blocks-generic-link-types '("denote" "roam")
  "Link types resolved via their :follow function from `org-link-parameters'.

Each entry is a link type string.  The type's :follow function must open
a file-visiting buffer.  Types whose :follow opens a browser or external
application are not supported; use
`org-transclusion-blocks-register-link-handler' instead.

Handled by `org-transclusion-blocks--add-by-generic-follow' at depth 90
in `org-transclusion-add-functions'.  Dedicated handlers at depth 0
take precedence."
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

Takes precedence over `org-transclusion-blocks-generic-link-types'.
Populates `org-transclusion-blocks--link-handlers'."
  (setf (alist-get type org-transclusion-blocks--link-handlers
                   nil nil #'string=)
        resolver))

;;;; Path Splitting

(defun org-transclusion-blocks--split-path-search (link)
  "Split path and search option from LINK element.

`org-element' does not parse :: separators for non-file link types.
The entire \"path::search\" string goes into :path with
:search-option nil.

Return cons (PATH . SEARCH-OPTION) where SEARCH-OPTION may be nil.

For file: links where org-element already splits correctly, return
the existing :path and :search-option values."
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

;;;; Generic Follow-Based Resolution

(defun org-transclusion-blocks--resolve-link-via-follow (type path)
  "Resolve link of TYPE with PATH to a file path via :follow function.

Call the :follow function registered in `org-link-parameters' for TYPE,
capture the resulting buffer, and return its variable `buffer-file-name'.

Return nil if:
- No :follow function registered for TYPE
- :follow function errors
- Resulting buffer has no file association

Uses `save-window-excursion' to contain side effects of :follow.
Called by `org-transclusion-blocks--add-by-generic-follow'."
  (when-let* ((follow-fn (org-link-get-parameter type :follow)))
    (condition-case nil
        (save-window-excursion
          (save-excursion
            (funcall follow-fn path nil)
            buffer-file-name))
      (error nil))))

(defun org-transclusion-blocks--add-by-generic-follow (link plist)
  "Resolve LINK via :follow function and re-dispatch for PLIST.

Handle link types listed in `org-transclusion-blocks-generic-link-types'.
Resolve to a file path by calling the link type's :follow function
from `org-link-parameters', construct a file: link preserving any
search option, and re-dispatch through `org-transclusion-add-functions'.

Split :: from path manually via `org-transclusion-blocks--split-path-search'
because `org-element' does not parse :: separators for non-file link types.

Return payload plist, or nil if link type is not in
`org-transclusion-blocks-generic-link-types' or resolution fails.

Uses `org-transclusion-blocks--split-path-search' for :: splitting.
Uses `org-transclusion-blocks--resolve-link-via-follow' for resolution.
Delegates to `org-transclusion-blocks--redispatch-as-file'."
  (let ((type (org-element-property :type link)))
    (when (member type org-transclusion-blocks-generic-link-types)
      (pcase-let ((`(,path . ,search)
                   (org-transclusion-blocks--split-path-search link)))
        (let ((file-path
               (org-transclusion-blocks--resolve-link-via-follow type path)))
          (if (not (and file-path (file-exists-p file-path)))
              (progn
                (message
                 "org-transclusion-blocks: %s link %S could not be resolved to a file"
                 type path)
                '(:src-content nil))
            (org-transclusion-blocks--redispatch-as-file
             file-path search plist)))))))

;;;; Explicit Resolver Dispatch

(defun org-transclusion-blocks--add-by-link-handler (link plist)
  "Resolve LINK via explicit resolver registry and re-dispatch for PLIST.

Check `org-transclusion-blocks--link-handlers' for a resolver matching
the link type.  Return payload plist, or nil if no handler matches.

Uses `org-transclusion-blocks--split-path-search' for :: splitting.
Delegates to `org-transclusion-blocks--redispatch-as-file'.

Installed at depth 80 in `org-transclusion-add-functions'."
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

Called by `org-transclusion-blocks--add-by-generic-follow',
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
before the upstream id: handler to fix :: search option splitting.

Append `org-transclusion-blocks--add-by-link-handler' at depth 80
for explicit resolver registry (takes precedence over generic).

Append `org-transclusion-blocks--add-by-generic-follow' at depth 90
as fallback for `org-transclusion-blocks-generic-link-types'.

Idempotent; safe to call multiple times."
  (unless (memq 'org-transclusion-blocks--add-org-id-with-search
                org-transclusion-add-functions)
    (add-hook 'org-transclusion-add-functions
              #'org-transclusion-blocks--add-org-id-with-search
              -10))
  (unless (memq 'org-transclusion-blocks--add-by-link-handler
                org-transclusion-add-functions)
    (add-hook 'org-transclusion-add-functions
              #'org-transclusion-blocks--add-by-link-handler
              80))
  (unless (memq 'org-transclusion-blocks--add-by-generic-follow
                org-transclusion-add-functions)
    (add-hook 'org-transclusion-add-functions
              #'org-transclusion-blocks--add-by-generic-follow
              90)))

;; Install on load
(org-transclusion-blocks--install-link-handlers)

(provide 'org-transclusion-blocks-link-handlers)
;;; org-transclusion-blocks-link-handlers.el ends here
