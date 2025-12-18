;;; orgit-file-type.el --- Description -*- lexical-binding: t; -*-

;; Link Type Headers: orgit-file
;; NOTE 2025-12-10: orgit-file registration demonstrates framework usage.
;; Provides validators and constructor as examples for user-defined types.

;;; Code:
(require 'org-transclusion-blocks)

(defun org-transclusion-blocks--orgit-file-validate-search (value header-key type)
  "Validate :orgit-search value for orgit-file links.

VALUE is header value string.
HEADER-KEY is :orgit-search.
TYPE is \\='orgit-file.

Accepts strings or complete s-expressions.
Signals user-error for incomplete sexps that will cause Babel errors.

Returns VALUE unchanged if valid.

This is user-defined validation - not framework behavior."
  (cond
   ;; Already quoted string or doesn't look like sexp - pass through
   ((and (stringp value)
         (or (string-prefix-p "\"" value)
             (not (string-prefix-p "(" value))))
    value)

   ;; Looks like sexp - check if complete
   ((and (stringp value) (string-prefix-p "(" value))
    (condition-case err
        (progn
          (read-from-string value)
          value)  ; Parseable - accept
      (error
       (user-error "%s"
                   (org-transclusion-blocks-format-validation-error
                    header-key
                    (format "incomplete s-expression will fail in Babel: %s"
                            (error-message-string err))
                    value
                    (format "Quote it as string: \"(%s\"" (substring value 1))
                    "Or complete the s-expression syntax")))))

   ;; Other - pass through
   (t value)))

(defun org-transclusion-blocks--git-ref-p (value)
  "Return non-nil if VALUE is valid git reference format.

Accepts:
- Branch names: main, develop, feature/new-thing
- Tag names: v1.0.0, release-2023
- Commit hashes: 7-40 character hex strings
- Special refs: HEAD, HEAD~N

Does not validate that ref exists in repository."
  (and (stringp value)
       (string-match-p
        (rx bos
            (or
             ;; HEAD with optional ~N suffix
             (: "HEAD" (opt "~" (+ digit)))
             ;; Short or full commit hash with optional ~N
             (: (>= 7 (any "a-f" "A-F" "0-9")) (opt "~" (+ digit)))
             ;; Branch or tag name
             (: (+ (any "a-z" "A-Z" "0-9" "-" "_" "/" ".")))))
        value)))

(defun org-transclusion-blocks--orgit-file-validate-ref (value header-key type)
  "Validate git ref format for orgit-file links.

VALUE is header value string.
HEADER-KEY is :orgit-rev.
TYPE is \\='orgit-file.

Signals user-error if VALUE is not valid git ref format.

Returns VALUE unchanged if valid.

This is user-defined validation - not framework behavior."
  (unless (org-transclusion-blocks--git-ref-p value)
    (user-error "%s"
                (org-transclusion-blocks-format-validation-error
                 header-key
                 "invalid git ref format"
                 value
                 "Expected: branch name, tag, commit hash, or HEAD~N"
                 "Examples: main, v1.0.0, abc1234, HEAD~3")))
  value)

;; Register orgit-file type with user-defined validators and constructor
(org-transclusion-blocks-register-type
 'orgit-file
 '(:repo (:header :orgit-repo
          :required t
          :expand-vars t)
   :rev (:header :orgit-rev
         :required t
         :validator org-transclusion-blocks--orgit-file-validate-ref
         :expand-vars t)
   :file (:header :orgit-file
          :required t
          :expand-vars t)
   :search (:header :orgit-search
            :validator org-transclusion-blocks--orgit-file-validate-search
            :expand-vars nil))
 (lambda (components)
   "Construct orgit-file link from COMPONENTS plist.

COMPONENTS has :repo, :rev, :file, and optional :search.

Returns raw link string: orgit-file:REPO::REV::FILE or
orgit-file:REPO::REV::FILE::SEARCH"
   (let ((repo (expand-file-name (plist-get components :repo)))
         (rev (plist-get components :rev))
         (file (plist-get components :file))
         (search (plist-get components :search)))
     (if search
         (format "orgit-file:%s::%s::%s::%s" repo rev file search)
       (format "orgit-file:%s::%s::%s" repo rev file)))))


(provide 'orgit-file-type)
;;; orgit-file-type.el ends here
