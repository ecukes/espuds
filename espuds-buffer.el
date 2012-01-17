;;; espuds-buffer.el --- Buffer related definitions


;; Switches buffer in the current window to BUFFER.
;;
;; Usage:
;;   Given I am in buffer "*scratch*"
(Given "^I am in buffer \"\\(.+\\)\"$"
       (lambda (buffer)
         (let ((v (vconcat [?\C-x ?b] (string-to-vector buffer))))
           (execute-kbd-macro v))))

;; Asserts that the current buffer is BUFFER.
;;
;; Usage:
;;   Then I should be in buffer "*scratch*"
(Then "^I should be in buffer \"\\(.+\\)\"$"
      (lambda (buffer)
        (should (equal buffer (buffer-name)))))

;; Asserts that the current buffer is connected to FILE.
;;
;; Usage:
;;   Then I should be in file "/path/to/some/file"
(Then "^I should be in file \"\\(.+\\)\"$"
      (lambda (file)
        (let ((file-name (buffer-file-name)))
          (assert file-name nil "Expected file to be '%s', but not visiting any file." file)
          (let ((match (string-match-p (concat file "$") file-name)))
            (assert match nil "Expected file to be '%s', but was '%s'." file file-name)))))


;; Clears all text in the current buffer.
;;
;; Usage:
;;   Given the buffer is empty
;;   When I clear the buffer
(Given "^the buffer is empty$\\|^I clear the buffer$"
       (lambda ()
         (erase-buffer)))

;; Switches to BUFFER.
;;
;; Usage:
;;   When I switch to buffer "Foo"
(When "^I switch to buffer \"\\(.+\\)\"$"
      (lambda (buffer)
        (let ((v (vconcat [?\C-x ?b] (string-to-vector buffer))))
          (execute-kbd-macro v))))


(provide 'espuds-buffer)

;;; espuds-buffer.el ends here
