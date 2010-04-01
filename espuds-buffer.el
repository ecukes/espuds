;;; espuds-buffer.el --- Buffer related definitions


;; Switches buffer in the current window to BUFFER.
;;
;; Usage:
;;   Given I am in the buffer "*scratch*"
(Given "^I am in the buffer \"\\(.+\\)\"$"
       (lambda (buffer)
         (switch-to-buffer (get-buffer-create buffer))))

;; Clears all text in the current buffer.
;;
;; Usage:
;;   Given the buffer is empty
(Given "^the buffer is empty$"
       (lambda ()
         (erase-buffer)))


(provide 'espuds-buffer)

;;; espuds-buffer.el ends here
