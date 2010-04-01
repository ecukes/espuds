;;; espuds-rest.el --- Rest related definitions


;; Loads CONTENTS with Emacs load command.
;;
;; Usage:
;;   Given the following is loaded:
;;   """
;;   CONTENTS
;;   """
(Given "^the following is loaded:$"
       (lambda (contents)
         (espuds-fake-eval contents)))

;; Creates a new temp file called FILE and opens it.
;;
;; Usage:
;;   When I am in the temp file "SOME FILE"
(When "^I am in the temp file \"\\(.+\\)\"$"
      (lambda (file)
        (find-file (make-temp-file file))))

;; Asserts that MESSAGE has been printed.
;;
;; Usage:
;;   Then I should see message "MESSAGE"
(Then "^I should see message \"\\(.+\\)\"$"
      (lambda (message)
        (assert
         (member message ecukes-messages) nil
         (concat "Expected \"" message "\" to be included in the list of printed messages."))))


(provide 'espuds-rest)

;;; espuds-rest.el ends here
