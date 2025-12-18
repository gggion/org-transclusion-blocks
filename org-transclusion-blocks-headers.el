;;; org-transclusion-blocks-headers.el --- Header argument manipulation framework -*- lexical-binding: t; -*-

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

;; Framework for manipulating header arguments on Org blocks.
;;
;; Handles three locations where header arguments can appear:
;; 1. #+HEADER: lines above blocks (highest precedence)
;; 2. Inline arguments on #+begin_ lines (medium precedence)
;; 3. :HEADER-ARGS: property inheritance (lowest precedence)
;;
;; Primary API:
;;
;; - `org-transclusion-blocks-header-get' - Read argument value
;; - `org-transclusion-blocks-header-set' - Write argument value
;; - `org-transclusion-blocks-header-update-partial' - Transform value
;;
;; All operations work on block at point only.  No buffer-wide scanning.
;;
;; Precedence rules:
;; - Reading: Return highest-precedence value
;; - Writing: Update highest-precedence existing location
;; - Property-inherited arguments are read-only by default
;;
;; Example usage:
;;
;;     ;; Read current value
;;     (org-transclusion-blocks-header-get :transclude-lines)
;;     ;; => "10-20"
;;
;;     ;; Update existing value
;;     (org-transclusion-blocks-header-set :transclude-lines "15-25")
;;
;;     ;; Update or insert as #+HEADER:
;;     (org-transclusion-blocks-header-set :transclude-lines "15-25"
;;                                         'update-or-insert-header)
;;
;;     ;; Partial update
;;     (org-transclusion-blocks-header-update-partial
;;      :results
;;      (lambda (current)
;;        (replace-regexp-in-string "output" "value" current)))

;;; Code:

(require 'org-element)
(require 'ob-core)

;;;; Data Structures

(cl-defstruct org-transclusion-blocks-header-location
  "Location of a header argument within a block.

Slots:
- block-element: org-element for the block
- arg-name: keyword symbol like :transclude-lines
- location-type: symbol - one of header, inline, property
- line-number: buffer line where arg appears (nil for property)
- value-bounds: cons (BEG . END) of value region in buffer
- value: current string value
- precedence: integer (3=header, 2=inline, 1=property)"
  block-element
  arg-name
  location-type
  line-number
  value-bounds
  value
  precedence)

;;;; Location Discovery

(defun org-transclusion-blocks-header--find-arg-on-line (arg-name)
  "Find ARG-NAME on current line.

ARG-NAME is keyword symbol like :transclude-lines.

Returns cons (BEG . END) of value region, or nil if not found.
Point must be on #+HEADER: or #+begin_ line."
  (let ((line-end (line-end-position))
        (arg-string (substring (symbol-name arg-name) 1))) ; Remove leading :
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward
             (concat ":" (regexp-quote arg-string) "[ \t]+\\([^ \t\n]+\\)")
             line-end t)
        (cons (match-beginning 1) (match-end 1))))))

(defun org-transclusion-blocks-header--find-locations-in-block (arg-name)
  "Find locations where ARG-NAME appears in block at point.

ARG-NAME is keyword symbol like :transclude-lines.

Returns list of `org-transclusion-blocks-header-location' structs
sorted by precedence (highest first).

Searches in order:
1. #+HEADER: lines above block
2. Inline args on #+begin_ line
3. :HEADER-ARGS: property (inherited)

Returns nil if ARG-NAME not found anywhere."
  (let ((element (org-element-at-point))
        (locations nil))
    
    (unless (memq (org-element-type element)
                  '(src-block quote-block example-block export-block
                    special-block verse-block center-block comment-block))
      (user-error "Not on a supported block"))
    
    ;; Search #+HEADER: lines
    (save-excursion
      (goto-char (org-element-property :begin element))
      (while (looking-at "^[ \t]*#\\+HEADER:")
        (when-let* ((bounds (org-transclusion-blocks-header--find-arg-on-line arg-name)))
          (push (make-org-transclusion-blocks-header-location
                 :block-element element
                 :arg-name arg-name
                 :location-type 'header
                 :line-number (line-number-at-pos)
                 :value-bounds bounds
                 :value (buffer-substring-no-properties (car bounds) (cdr bounds))
                 :precedence 3)
                locations))
        (forward-line 1)))
    
    ;; Search inline args on #+begin_ line
    (save-excursion
      (goto-char (org-element-property :begin element))
      (while (looking-at "^[ \t]*#\\+HEADER:")
        (forward-line 1))
      (when (looking-at "^[ \t]*#\\+begin_")
        (when-let* ((bounds (org-transclusion-blocks-header--find-arg-on-line arg-name)))
          (push (make-org-transclusion-blocks-header-location
                 :block-element element
                 :arg-name arg-name
                 :location-type 'inline
                 :line-number (line-number-at-pos)
                 :value-bounds bounds
                 :value (buffer-substring-no-properties (car bounds) (cdr bounds))
                 :precedence 2)
                locations))))
    
    ;; Search property inheritance
    (when-let* ((prop-value (org-entry-get nil "HEADER-ARGS" t))
               (parsed (org-babel-parse-header-arguments prop-value))
               (value (cdr (assoc arg-name parsed))))
      (push (make-org-transclusion-blocks-header-location
             :block-element element
             :arg-name arg-name
             :location-type 'property
             :line-number nil
             :value-bounds nil
             :value value
             :precedence 1)
            locations))
    
    ;; Sort by precedence descending
    (sort locations
          (lambda (a b)
            (> (org-transclusion-blocks-header-location-precedence a)
               (org-transclusion-blocks-header-location-precedence b))))))

;;;; Primary API

(defun org-transclusion-blocks-header-get (arg-name)
  "Get value of ARG-NAME for block at point.

ARG-NAME is keyword symbol like :transclude-lines.

Returns highest-precedence value string, or nil if not found.

Precedence: #+HEADER: > inline > property inheritance."
  (when-let* ((locations (org-transclusion-blocks-header--find-locations-in-block arg-name)))
    (org-transclusion-blocks-header-location-value (car locations))))

(defun org-transclusion-blocks-header-set (arg-name new-value &optional mode)
  "Set ARG-NAME to NEW-VALUE for block at point.

ARG-NAME is keyword symbol like :transclude-lines.
NEW-VALUE is string value.

MODE controls update behavior:
- nil or \\='update-existing: Update highest-precedence existing location.
                          Error if ARG-NAME not found.
- \\='insert-header: Insert as #+HEADER: if not found.
- \\='insert-inline: Insert inline if not found.
- \\='update-or-insert-header: Update existing or insert as #+HEADER:.
- \\='update-or-insert-inline: Update existing or insert inline.

Returns location struct of updated/inserted argument.

Signals error if MODE is \\='update-existing and ARG-NAME not found.
Signals error if attempting to update property-inherited arg."
  (let ((locations (org-transclusion-blocks-header--find-locations-in-block arg-name))
        (mode (or mode 'update-existing)))
    
    (cond
     ;; Update existing location
     (locations
      (let ((target (car locations))) ; Highest precedence
        (when (eq (org-transclusion-blocks-header-location-location-type target)
                  'property)
          (user-error "Cannot modify property-inherited header argument %s" arg-name))
        
        (org-transclusion-blocks-header--update-location target new-value)
        target))
     
     ;; Insert new location
     ((memq mode '(insert-header update-or-insert-header))
      (org-transclusion-blocks-header--insert-header arg-name new-value))
     
     ((memq mode '(insert-inline update-or-insert-inline))
      (org-transclusion-blocks-header--insert-inline arg-name new-value))
     
     ;; Error on missing
     (t
      (user-error "Header argument %s not found and MODE does not allow insertion" arg-name)))))

(defun org-transclusion-blocks-header-update-partial (arg-name update-fn &optional mode)
  "Update part of ARG-NAME's value using UPDATE-FN.

ARG-NAME is keyword symbol.
UPDATE-FN receives current value string, returns new value string.
MODE is same as `org-transclusion-blocks-header-set'.

Example:
  (org-transclusion-blocks-header-update-partial
   :results
   (lambda (current)
     (if (string-match-p \"output\" current)
         (replace-regexp-in-string \"output\" \"value\" current)
       (concat current \" value\"))))

Returns updated location struct."
  (let ((current (org-transclusion-blocks-header-get arg-name)))
    (unless current
      (user-error "Cannot partially update non-existent header argument %s" arg-name))
    
    (let ((new-value (funcall update-fn current)))
      (org-transclusion-blocks-header-set arg-name new-value mode))))

;;;; Internal Manipulation

(defun org-transclusion-blocks-header--update-location (location new-value)
  "Update LOCATION to NEW-VALUE.

LOCATION is `org-transclusion-blocks-header-location' struct.
NEW-VALUE is string.

Modifies buffer at location's value-bounds."
  (let ((bounds (org-transclusion-blocks-header-location-value-bounds location)))
    (unless bounds
      (error "Cannot update location without value-bounds"))
    
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (goto-char (car bounds))
      (insert new-value))
    
    ;; Update struct
    (setf (org-transclusion-blocks-header-location-value location) new-value)
    (setf (org-transclusion-blocks-header-location-value-bounds location)
          (cons (car bounds) (+ (car bounds) (length new-value))))))

(defun org-transclusion-blocks-header--insert-header (arg-name value)
  "Insert ARG-NAME with VALUE as #+HEADER: line.

ARG-NAME is keyword symbol.
VALUE is string.

Inserts before block's #+begin_ line.
Returns new location struct."
  (let ((element (org-element-at-point))
        (begin (org-element-property :begin (org-element-at-point)))
        (arg-string (substring (symbol-name arg-name) 1)))
    
    (save-excursion
      (goto-char begin)
      (let ((insert-pos (point)))
        (insert (format "#+HEADER: :%s %s\n" arg-string value))
        
        (make-org-transclusion-blocks-header-location
         :block-element element
         :arg-name arg-name
         :location-type 'header
         :line-number (line-number-at-pos insert-pos)
         :value-bounds (cons (+ insert-pos 11 (length arg-string))
                             (+ insert-pos 11 (length arg-string) (length value)))
         :value value
         :precedence 3)))))

(defun org-transclusion-blocks-header--insert-inline (arg-name value)
  "Insert ARG-NAME with VALUE as inline argument.

ARG-NAME is keyword symbol.
VALUE is string.

Appends to #+begin_ line.
Returns new location struct."
  (let ((element (org-element-at-point))
        (begin (org-element-property :begin (org-element-at-point)))
        (arg-string (substring (symbol-name arg-name) 1)))
    
    (save-excursion
      (goto-char begin)
      ;; Skip #+HEADER: lines
      (while (looking-at "^[ \t]*#\\+HEADER:")
        (forward-line 1))
      
      (unless (looking-at "^[ \t]*#\\+begin_")
        (error "Expected #+begin_ line"))
      
      (end-of-line)
      (let ((insert-pos (point)))
        (insert (format " :%s %s" arg-string value))
        
        (make-org-transclusion-blocks-header-location
         :block-element element
         :arg-name arg-name
         :location-type 'inline
         :line-number (line-number-at-pos)
         :value-bounds (cons (+ insert-pos 3 (length arg-string))
                             (+ insert-pos 3 (length arg-string) (length value)))
         :value value
         :precedence 2)))))

(provide 'org-transclusion-blocks-headers)
;;; org-transclusion-blocks-headers.el ends here
