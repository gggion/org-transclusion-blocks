;;; org-transclusion-blocks-lines.el --- Line range manipulation -*- lexical-binding: t; -*-

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

;; Interactive commands for manipulating :lines and :transclude-lines
;; properties on transclusion blocks via transient menu interface.
;;
;; Entry point: M-x org-transclusion-blocks-lines-menu
;;
;; Commands:
;; - Scroll: Move range up/down (both boundaries)
;; - Expand/Shrink: Symmetric boundary adjustment
;; - Expand Up/Down: Asymmetric single-boundary expansion
;; - Shrink Up/Down: Asymmetric single-boundary contraction
;; - Set: Direct range specification

;;; Code:

(require 'org-transclusion-blocks)
(require 'transient)

;;;; Customization

(defgroup org-transclusion-blocks-lines nil
  "Line range manipulation for org-transclusion-blocks."
  :group 'org-transclusion-blocks
  :prefix "org-transclusion-blocks-lines-")

(defcustom org-transclusion-blocks-lines-default-increment 1
  "Default increment for range adjustments.

Used when no prefix argument provided to transient suffixes."
  :type 'integer
  :group 'org-transclusion-blocks-lines)

;;;; Range Validation

(defun org-transclusion-blocks-lines--at-upper-boundary-p ()
  "Return non-nil if range end is at source maximum line.

Used to prevent downward scroll/expansion when already at bottom.

Returns nil if no metadata available."
  (let* ((current-range (org-transclusion-blocks-lines--get-current-range))
         (current-end (cdr current-range))
         (element (org-element-at-point))
         (bounds (org-transclusion-blocks--get-content-bounds element))
         (beg (car bounds))
         (max-line (get-text-property beg 'org-transclusion-blocks-max-line)))
    (and current-end max-line (>= current-end max-line))))

(defun org-transclusion-blocks-lines--at-lower-boundary-p ()
  "Return non-nil if range start is at line 1.

Used to prevent upward scroll/expansion when already at top.

Returns nil if no metadata available."
  (let* ((current-range (org-transclusion-blocks-lines--get-current-range))
         (current-start (car current-range)))
    (and current-start (<= current-start 1))))

;;;; Range Parsing and Formatting

(defun org-transclusion-blocks-lines--parse-range (range-string)
  "Parse RANGE-STRING into cons (START . END).

RANGE-STRING formats:
  \"10-20\" → (10 . 20)
  \"10-\"   → (10 . nil)
  \"-20\"   → (nil . 20)
  \"10-10\" → (10 . 10)

Returns nil if RANGE-STRING is malformed."
  (when (and range-string (string-match "^\\([0-9]+\\)?-\\([0-9]+\\)?$" range-string))
    (let ((start (match-string 1 range-string))
          (end (match-string 2 range-string)))
      (cons (when start (string-to-number start))
            (when end (string-to-number end))))))

(defun org-transclusion-blocks-lines--format-range (start end)
  "Format START and END into range string.

START and END can be nil (open-ended).

Returns string like \"10-20\", \"10-\", \"-20\", or \"10-10\"."
  (concat (if start (number-to-string start) "")
          "-"
          (if end (number-to-string end) "")))

;;;; Header Property Access

(defun org-transclusion-blocks-lines--get-current-range ()
  "Return current line range as cons (START . END) or nil.

Checks :transclude-lines first (generic header), then :lines
property (src-block specific).

START/END can be nil for open-ended ranges."
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))
    (cond
     ;; For src-blocks, check both via Babel params
     ((eq type 'src-block)
      (let* ((info (org-babel-get-src-block-info 'no-eval))
             (params (nth 2 info))
             (transclude-lines (assoc-default :transclude-lines params))
             (lines (assoc-default :lines params)))
        (org-transclusion-blocks-lines--parse-range
         (or transclude-lines lines))))

     ;; For other blocks, parse :header property
     (t
      (let ((headers (org-element-property :header element)))
        (catch 'found
          (dolist (header-str headers)
            (when (string-match "^:transclude-lines[ \t]+\\(.+\\)$" header-str)
              (throw 'found (org-transclusion-blocks-lines--parse-range
                             (match-string 1 header-str)))))
          nil))))))

(defun org-transclusion-blocks-lines--update-range (new-start new-end)
  "Update line range to NEW-START and NEW-END with coherence validation.

NEW-START and NEW-END can be nil for open-ended ranges.

Validates logical coherence:
- START >= 1
- END >= START

Directional boundary checking happens in calling commands
via `org-transclusion-blocks-lines--at-upper-boundary-p' and
`org-transclusion-blocks-lines--at-lower-boundary-p'.

Updates :transclude-lines if present, otherwise :lines.
Refreshes transclusion after update."
  (let* ((element (org-element-at-point))
         (type (org-element-type element)))

    ;; Enforce logical coherence
    (when new-start
      (setq new-start (max 1 new-start)))

    (when (and new-start new-end (> new-start new-end))
      (user-error "Cannot make start line (%d) exceed end line (%d)"
                  new-start new-end))

    (let ((new-range (org-transclusion-blocks-lines--format-range new-start new-end)))
      (save-excursion
        (goto-char (org-element-property :begin element))

        (cond
         ;; For src-blocks, update via header modification
         ((eq type 'src-block)
          (let ((found-transclude nil)
                (found-lines nil)
                (begin (org-element-property :begin element)))
            ;; Start at beginning of element
            (goto-char begin)

            ;; Scan through all #+HEADER: lines before #+begin_src
            (while (looking-at "^[ \t]*#\\+HEADER:")
              (cond
               ;; Found :transclude-lines
               ((looking-at "^\\([ \t]*#\\+HEADER:[ \t]+\\):transclude-lines[ \t]+\\(.+\\)$")
                (setq found-transclude t)
                (replace-match (concat "\\1:transclude-lines " new-range) nil nil nil))

               ;; Found :lines (only update if no :transclude-lines)
               ((and (not found-transclude)
                     (looking-at "^\\([ \t]*#\\+HEADER:[ \t]+\\):lines[ \t]+\\(.+\\)$"))
                (setq found-lines t)
                (replace-match (concat "\\1:lines " new-range) nil nil nil)))

              ;; Move to next line
              (forward-line 1))

            ;; If neither found, insert :transclude-lines before #+begin_src
            (unless (or found-transclude found-lines)
              (goto-char begin)
              (insert (format "#+HEADER: :transclude-lines %s\n" new-range)))))

         ;; For other blocks, update :transclude-lines in headers
         (t
          (let ((found nil)
                (begin (org-element-property :begin element)))
            ;; Start at beginning of element
            (goto-char begin)

            ;; Scan through all #+HEADER: lines before #+begin_XXX
            (while (looking-at "^[ \t]*#\\+HEADER:")
              (when (looking-at "^\\([ \t]*#\\+HEADER:[ \t]+\\):transclude-lines[ \t]+\\(.+\\)$")
                (setq found t)
                (replace-match (concat "\\1:transclude-lines " new-range) nil nil nil))

              ;; Move to next line
              (forward-line 1))

            ;; If not found, insert before block begin
            (unless found
              (goto-char begin)
              (insert (format "#+HEADER: :transclude-lines %s\n" new-range)))))))

      ;; Refresh transclusion
      (org-transclusion-blocks-add))))

;;;; Interactive Commands - Core Operations

;;;###autoload
(defun org-transclusion-blocks-set-lines (start end)
  "Set line range to START-END.

START and END are line numbers. Enter empty string for open-ended.

Examples:
  10 20  → :lines 10-20
  10 \"\"  → :lines 10-
  \"\" 20  → :lines -20"
  (interactive
   (list (read-string "Start line (empty for beginning): ")
         (read-string "End line (empty for end of file): ")))
  (let ((start-num (if (string-empty-p start) nil (string-to-number start)))
        (end-num (if (string-empty-p end) nil (string-to-number end))))
    (org-transclusion-blocks-lines--update-range start-num end-num)))

;;;###autoload
(defun org-transclusion-blocks-set-lines-range (width)
  "Set line range with WIDTH centered on current range.

WIDTH is number of lines to include.

If current range is 10-20, setting WIDTH to 5 produces 13-17
(centered on midpoint 15)."
  (interactive "nRange width: ")
  (let* ((current (org-transclusion-blocks-lines--get-current-range))
         (start (or (car current) 1))
         (end (or (cdr current) (+ start width)))
         (midpoint (/ (+ start end) 2))
         (half-width (/ width 2))
         (new-start (max 1 (- midpoint half-width)))
         (new-end (+ new-start width -1)))
    (org-transclusion-blocks-lines--update-range new-start new-end)))

;;;; Interactive Commands - Symmetric Operations

;;;###autoload
(defun org-transclusion-blocks-expand-lines-range (amount)
  "Expand line range by AMOUNT lines on both ends.

AMOUNT defaults to `org-transclusion-blocks-lines-default-increment'.
Moves start earlier and end later.

If range is 10-20, expanding by 2 produces 8-22."
  (interactive "p")
  (let* ((current (org-transclusion-blocks-lines--get-current-range))
         (start (car current))
         (end (cdr current)))
    (unless current
      (user-error "No line range found. Use `org-transclusion-blocks-set-lines' first"))
    (org-transclusion-blocks-lines--update-range
     (when start (max 1 (- start amount)))
     (when end (+ end amount)))))

;;;###autoload
(defun org-transclusion-blocks-shrink-lines-range (amount)
  "Shrink line range by AMOUNT lines on both ends.

AMOUNT defaults to `org-transclusion-blocks-lines-default-increment'.
Moves start later and end earlier.

If range is 10-20, shrinking by 2 produces 12-18.
Errors if range becomes invalid (start > end)."
  (interactive "p")
  (let* ((current (org-transclusion-blocks-lines--get-current-range))
         (start (car current))
         (end (cdr current)))
    (unless current
      (user-error "No line range found. Use `org-transclusion-blocks-set-lines' first"))
    (let ((new-start (when start (+ start amount)))
          (new-end (when end (- end amount))))
      (when (and new-start new-end (> new-start new-end))
        (user-error "Cannot shrink range: would make start (%d) > end (%d)"
                    new-start new-end))
      (org-transclusion-blocks-lines--update-range new-start new-end))))

;;;###autoload
(defun org-transclusion-blocks-scroll-down (amount)
  "Scroll line range down by AMOUNT lines.

AMOUNT defaults to `org-transclusion-blocks-lines-default-increment'.
Moves both start and end later.

If range is 10-20, scrolling down by 3 produces 13-23.

Prevented when end already at source maximum line."
  (interactive "p")
  (if (org-transclusion-blocks-lines--at-upper-boundary-p)
      (let* ((element (org-element-at-point))
             (bounds (org-transclusion-blocks--get-content-bounds element))
             (beg (car bounds))
             (max-line (get-text-property beg 'org-transclusion-blocks-max-line)))
        (message "Cannot scroll beyond end of source (line %d)" max-line))
    (let* ((current (org-transclusion-blocks-lines--get-current-range))
           (start (car current))
           (end (cdr current)))
      (unless current
        (user-error "No line range found. Use `org-transclusion-blocks-set-lines' first"))
      (org-transclusion-blocks-lines--update-range
       (when start (+ start amount))
       (when end (+ end amount))))))

;;;###autoload
(defun org-transclusion-blocks-scroll-up (amount)
  "Scroll line range up by AMOUNT lines.

AMOUNT defaults to `org-transclusion-blocks-lines-default-increment'.
Moves both start and end earlier.

If range is 10-20, scrolling up by 3 produces 7-17.
Start cannot go below 1.

Prevented when start already at line 1."
  (interactive "p")
  (if (org-transclusion-blocks-lines--at-lower-boundary-p)
      (message "Cannot scroll before beginning of source")
    (let* ((current (org-transclusion-blocks-lines--get-current-range))
           (start (car current))
           (end (cdr current)))
      (unless current
        (user-error "No line range found. Use `org-transclusion-blocks-set-lines' first"))
      (org-transclusion-blocks-lines--update-range
       (when start (max 1 (- start amount)))
       (when end (- end amount))))))

;;;; Interactive Commands - Asymmetric Operations

;;;###autoload
(defun org-transclusion-blocks-expand-up (amount)
  "Expand range upward by AMOUNT lines.

AMOUNT defaults to `org-transclusion-blocks-lines-default-increment'.
Moves start earlier, end unchanged.

Example:
  Before: 50-100
  After (amount=20): 30-100

Prevented when start already at line 1."
  (interactive "p")
  (if (org-transclusion-blocks-lines--at-lower-boundary-p)
      (message "Cannot expand before beginning of source")
    (let* ((current (org-transclusion-blocks-lines--get-current-range))
           (start (car current))
           (end (cdr current)))
      (unless current
        (user-error "No line range found. Use `org-transclusion-blocks-set-lines' first"))
      (org-transclusion-blocks-lines--update-range
       (when start (max 1 (- start amount)))
       end))))

;;;###autoload
(defun org-transclusion-blocks-expand-down (amount)
  "Expand range downward by AMOUNT lines.

AMOUNT defaults to `org-transclusion-blocks-lines-default-increment'.
Moves end later, start unchanged.

Example:
  Before: 50-100
  After (amount=20): 50-120

Prevented when end already at source maximum line."
  (interactive "p")
  (if (org-transclusion-blocks-lines--at-upper-boundary-p)
      (let* ((element (org-element-at-point))
             (bounds (org-transclusion-blocks--get-content-bounds element))
             (beg (car bounds))
             (max-line (get-text-property beg 'org-transclusion-blocks-max-line)))
        (message "Cannot expand beyond end of source (line %d)" max-line))
    (let* ((current (org-transclusion-blocks-lines--get-current-range))
           (start (car current))
           (end (cdr current)))
      (unless current
        (user-error "No line range found. Use `org-transclusion-blocks-set-lines' first"))
      (org-transclusion-blocks-lines--update-range
       start
       (when end (+ end amount))))))

;;;###autoload
(defun org-transclusion-blocks-shrink-up (amount)
  "Shrink range from top by AMOUNT lines.

AMOUNT defaults to `org-transclusion-blocks-lines-default-increment'.
Moves start later, end unchanged.

Example:
  Before: 50-100
  After (amount=10): 60-100

Errors if start would exceed end."
  (interactive "p")
  (let* ((current (org-transclusion-blocks-lines--get-current-range))
         (start (car current))
         (end (cdr current)))
    (unless current
      (user-error "No line range found. Use `org-transclusion-blocks-set-lines' first"))
    (let ((new-start (when start (+ start amount))))
      (when (and new-start end (> new-start end))
        (user-error "Cannot shrink from top: would make start (%d) > end (%d)"
                    new-start end))
      (org-transclusion-blocks-lines--update-range new-start end))))

;;;###autoload
(defun org-transclusion-blocks-shrink-down (amount)
  "Shrink range from bottom by AMOUNT lines.

AMOUNT defaults to `org-transclusion-blocks-lines-default-increment'.
Moves end earlier, start unchanged.

Example:
  Before: 50-100
  After (amount=10): 50-90

Errors if end would go below start."
  (interactive "p")
  (let* ((current (org-transclusion-blocks-lines--get-current-range))
         (start (car current))
         (end (cdr current)))
    (unless current
      (user-error "No line range found. Use `org-transclusion-blocks-set-lines' first"))
    (let ((new-end (when end (- end amount))))
      (when (and start new-end (< new-end start))
        (user-error "Cannot shrink from bottom: would make end (%d) < start (%d)"
                    new-end start))
      (org-transclusion-blocks-lines--update-range start new-end))))

;;;; Helper functions
(defun org-transclusion-blocks--lines-menu-cleanup ()
  "Cleanup function for lines menu transient exit.
Applies overlays and removes itself from hook."
  (org-transclusion-blocks--ensure-overlays-applied)
  (remove-hook 'transient-exit-hook
               #'org-transclusion-blocks--lines-menu-cleanup
               t))

;;;; Transient Menu

;;;###autoload
(defun org-transclusion-blocks-lines-menu ()
  "Adjust line range for transclusion at point.

All commands accept prefix argument for custom increment.
Default increment is `org-transclusion-blocks-lines-default-increment'.

Suppresses overlay creation during adjustment via
`org-transclusion-blocks--suppress-overlays' to improve
performance.  Overlays are created once on menu exit."
  (interactive)
  ;; Set suppression flag before entering transient
  (setq org-transclusion-blocks--suppress-overlays t)

  ;; Add exit hook for this buffer only
  (add-hook 'transient-exit-hook
            #'org-transclusion-blocks--lines-menu-cleanup
            nil t)

  ;; Enter the transient menu
  (transient-setup 'org-transclusion-blocks-lines-menu-impl))

(transient-define-prefix org-transclusion-blocks-lines-menu-impl ()
  "Implementation of line range adjustment menu.

Do not call directly; use `org-transclusion-blocks-lines-menu'."
  :refresh-suffixes t

  ["Current Range"
   (:info (lambda ()
            (if-let ((range (org-transclusion-blocks-lines--get-current-range)))
                (format "Lines: %s-%s"
                        (or (car range) "∞")
                        (or (cdr range) "∞"))
              "No range set")))]

  [["Scroll"
    ("p" "Up" org-transclusion-blocks-scroll-up :transient t)
    ("n" "Down" org-transclusion-blocks-scroll-down :transient t)]

   ["Symmetric"
    ("+" "Expand" org-transclusion-blocks-expand-lines-range :transient t)
    ("-" "Shrink" org-transclusion-blocks-shrink-lines-range :transient t)]

   ["Expand Edge"
    ("u" "↑ Top" org-transclusion-blocks-expand-up :transient t)
    ("d" "↓ Bottom" org-transclusion-blocks-expand-down :transient t)]

   ["Shrink Edge"
    ("U" "↑ Top" org-transclusion-blocks-shrink-up :transient t)
    ("D" "↓ Bottom" org-transclusion-blocks-shrink-down :transient t)]

   ["Set"
    ("s" "Absolute" org-transclusion-blocks-set-lines)
    ("w" "Width" org-transclusion-blocks-set-lines-range)]

   ["Exit"
    ("q" "Quit" transient-quit-one)]])

(provide 'org-transclusion-blocks-lines)
;;; org-transclusion-blocks-lines.el ends here
