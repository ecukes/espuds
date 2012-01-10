;;; espuds-text.el --- Text related definitions


;; Inserts CONTENTS into the current buffer.
;;
;; Usage:
;;   When I insert "CONTENTS"
;;
;;   When I insert:
;;     """
;;     CONTENTS
;;     """
(When "^I insert\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (contents)
        (insert contents)))


;; Asserts that the current buffer includes some text.
;;
;; Usage:
;;   Then I should see "CONTENTS"
;;
;;   Then I should see:
;;   """
;;   CONTENTS
;;   """
(Then "^I should see\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-buffer-contents)))
          (assert
           (search expected actual)))))

;; Asserts that the current buffer does not include some text.
;;
;; Usage:
;;   Then I should not see "CONTENTS"
;;
;;   Then I should not see:
;;   """
;;   CONTENTS
;;   """
(Then "^I should not see\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-buffer-contents)))
          (assert
           (not (search expected actual))))))


;; Asserts that the current buffer matches some text.
;;
;; Usage:
;;   Then I should see pattern "CONTENTS"
;;
;;   Then I should see pattern:
;;   """
;;   CONTENTS
;;   """
(Then "^I should see pattern\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-buffer-contents)))
          (assert
           (string-match-p expected actual)))))

;; Asserts that the current buffer does not match some text.
;;
;; Usage:
;;   Then I should not see pattern "CONTENTS"
;;
;;   Then I should not see pattern:
;;   """
;;   CONTENTS
;;   """
(Then "^I should not see pattern\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-buffer-contents)))
          (assert
           (not (string-match-p expected actual))))))

;; Selects TEXT if found.
;;
;; Usage:
;;   When I select "SOME TEXT"
(When "^I select \"\\(.+\\)\"$"
      (lambda (text)
        (goto-char (point-min))
        (unless (re-search-forward text nil t)
          (assert nil nil (concat "The text \"" text "\" does not exist in the current buffer.")))
        (set-mark (point))
        (re-search-backward text)))

;; Asserts that there nothing to see in the current buffer.
;;
;; Usage:
;;   Then I should not see anything
(Then "I should not see anything"
      (lambda ()
        (assert (equal (buffer-size) 0))))


(provide 'espuds-text)

;;; espuds-text.el ends here
