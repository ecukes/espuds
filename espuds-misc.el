;;; espuds-misc.el --- Definitions that don't fit in any other file

;; Turns on some mode.
;;
;; Usage:
;;   When I turn on ruby-mode
(When "^I turn on \\(.+\\)$"
      (lambda (mode)
        (let ((v (vconcat [?\C-u 1 ?\M-x] (string-to-vector mode))))
          (execute-kbd-macro v))))

;; Loads CONTENTS with Emacs load command.
;;
;; Usage:
;;   When I load the following:
;;   """
;;   CONTENTS
;;   """
(When "^I load the following:$"
      (lambda (contents)
        (espuds-fake-eval contents)))

;; Creates a new temp file called FILE and opens it.
;;
;; Usage:
;;   When I open temp file "SOME FILE"
(When "^I open temp file \"\\(.+\\)\"$"
      (lambda (file)
        (find-file (make-temp-file file))))

;; Asserts that MESSAGE has been printed.
;;
;; Usage:
;;   Then I should see message "MESSAGE"
(Then "^I should see message \"\\(.+\\)\"$"
      (lambda (message)
        (save-excursion
          (set-buffer "*Messages*")
          (let ((search (search message (espuds-buffer-contents))))
            (assert
             search nil "Expected '%s' to be included in the list of printed messages, but was not." message)))))


(provide 'espuds-misc)

;;; espuds-misc.el ends here
