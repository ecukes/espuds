;;; espuds-buffer.el --- Buffer related definitions


;; Switches buffer in the current window to BUFFER.
;;
;; Usage:
;;   Given I am in buffer "*scratch*"
(Given "^I am in buffer \"\\(.+\\)\"$"
       (lambda (buffer)
         (let ((v (vconcat [?\C-x ?b] (string-to-vector buffer) (vector 'return))))
           (execute-kbd-macro v))))

;; Clears all text in the current buffer.
;;
;; Usage:
;;   Given the buffer is empty
(Given "^the buffer is empty$"
       (lambda ()
         (erase-buffer)))

;; Clears all text in the current buffer.
;;
;; Usage:
;;   When I clear the buffer
(When "I clear the buffer"
      (lambda ()
        (erase-buffer)))


(provide 'espuds-buffer)

;;; espuds-buffer.el ends here
