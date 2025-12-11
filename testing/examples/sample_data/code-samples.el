;;; code-samples.el --- Sample code for transclusion testing

;; Sample function 1
(defun greet (name)
  "Greet NAME with a friendly message.
Returns formatted greeting string."
  (format "Hello, %s! Welcome to the test." name))

;; Sample function 2
(defun process-data (data)
  "Process DATA list and return filtered results.
Filters out nil values and applies transformation."
  (let ((filtered (remove nil data)))
    (mapcar (lambda (x) (* x 2)) filtered)))

;; Sample function 3
(defun calculate-statistics (numbers)
  "Calculate basic statistics for NUMBERS.
Returns plist with :mean, :min, :max."
  (let* ((count (length numbers))
         (sum (apply #'+ numbers))
         (mean (/ sum count))
         (sorted (sort (copy-sequence numbers) #'<))
         (min-val (car sorted))
         (max-val (car (last sorted))))
    (list :mean mean :min min-val :max max-val :count count)))

;; Sample variable
(defvar sample-data '(10 20 30 40 50)
  "Sample data for testing purposes.")

;; Sample macro
(defmacro with-timing (label &rest body)
  "Execute BODY and print timing with LABEL."
  `(let ((start-time (current-time)))
     (prog1 (progn ,@body)
       (message "%s took %.3f seconds"
                ,label
                (float-time (time-since start-time))))))

;;; code-samples.el ends here
