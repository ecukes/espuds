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


;; Asserts that the current buffer includes or not includes some text.
;;
;; Usage:
;;   Then I should see "CONTENTS"
;;
;;   Then I should not see "CONTENTS"
;;
;;   Then I should see:
;;   """
;;   CONTENTS
;;   """
;;
;;   Then I should not see:
;;   """
;;   CONTENTS
;;   """
(Then "^I should\\( not \\| \\)see\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (see expected)
        (should-or-should-not-see see expected 'search)))

;; Asserts that the current buffer matches or don't matches some text.
;;
;; Usage:
;;   Then I should see pattern "CONTENTS"
;;
;;   Then I should not see pattern "CONTENTS"
;;
;;   Then I should see pattern:
;;   """
;;   CONTENTS
;;   """
;;
;;   Then I should not see pattern:
;;   """
;;   CONTENTS
;;   """
(Then "^I should\\( not \\| \\)see pattern\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (see expected)
        (should-or-should-not-see see expected 'string-match-p)))

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


(provide 'espuds-text)

;;; espuds-text.el ends here
