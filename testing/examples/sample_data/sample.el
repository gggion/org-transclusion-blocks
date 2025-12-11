;;; sample.el --- Sample Elisp file for testing -*- lexical-binding: t; -*-

;; Sample variable
(defvar my-config
  '((name . "test")
    (version . "1.0")
    (enabled . t))
  "Sample configuration variable.")

(defun greet (name)
  "Greet NAME with a friendly message.
Returns formatted greeting string."
  (format "Hello, %s! Welcome to the test." name))

(defun calculate-sum (numbers)
  "Calculate sum of NUMBERS list.
NUMBERS should be a list of integers or floats.
Returns the total sum as a number."
  (apply #'+ numbers))

(defun process-data (data &optional transform-fn)
  "Process DATA with optional TRANSFORM-FN.
DATA can be any Lisp object.
TRANSFORM-FN is a function that transforms DATA.
If TRANSFORM-FN is nil, returns DATA unchanged."
  (if transform-fn
      (funcall transform-fn data)
    data))

(defun filter-even (numbers)
  "Filter even numbers from NUMBERS list.
Returns new list containing only even numbers."
  (seq-filter (lambda (n) (= 0 (mod n 2))) numbers))

(defun create-user (name email &optional role)
  "Create user record with NAME and EMAIL.
ROLE is optional and defaults to 'user.
Returns property list representing the user."
  (list :name name
        :email email
        :role (or role 'user)
        :created (current-time)))

;;; sample.el ends here
