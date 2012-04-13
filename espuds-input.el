;;; espuds-input.el --- Input related definitions


(defvar espuds-action-chain '()
  "List of actions to execute.")

(defvar espuds-chain-active nil
  "Is t if chaining is active, nil otherwise.")


;; Starts an action chain.
;;
;; Usage:
;;   When I start an action chain
(When "^I start an action chain$"
      (lambda ()
        (setq espuds-action-chain '())
        (setq espuds-chain-active t)))

;; Executes the action chain.
;;
;; Usage:
;;   When I execute the action chain
(When "^I execute the action chain$"
      (lambda ()
        (execute-kbd-macro espuds-action-chain)
        (setq espuds-chain-active nil)))

;; If action chaining is active. Add KEYBINDING to the action
;; chain. Otherwise execute the function that KEYBINDING is bound to.
;;
;; Usage:
;;   When I press "C-h e"
(When "^I press \"\\(.+\\)\"$"
      (lambda (keybinding)
        (let ((macro (edmacro-parse-keys keybinding)))
          (if espuds-chain-active
              (setq espuds-action-chain (vconcat espuds-action-chain macro))
            (if (equal keybinding "C-g")
                (espuds-quit)
              (execute-kbd-macro macro))))))

;; Quit without signal.
;;
;; Usage:
;;   When I quit
(When "^I quit$" 'espuds-quit)

;; If action chaining is active. Add TYPING to the action
;; chain. Otherwise simulate the TYPING.
;;
;; Usage:
;;   When I type "TYPING"
(Given "^I type \"\\(.+\\)\"$"
       (lambda (typing)
         (if espuds-chain-active
             (setq espuds-action-chain (vconcat espuds-action-chain (string-to-vector typing)))
           (execute-kbd-macro (string-to-vector typing)))))


(provide 'espuds-input)

;;; espuds-input.el ends here
