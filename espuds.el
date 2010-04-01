;;; espuds.el --- Ecukes step definitions

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: test
;; URL: http://github.com/rejeep/espuds

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'edmacro)
(require 'cl)


(defvar espuds-action-chain '()
  "List of actions to execute.")

(defvar espuds-chain-active nil
  "t if chaining is active, nil otherwise.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BUFFERS START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (let* ((actual (espuds-buffer-contents))
               (found (search expected actual)))
          (assert
           (if (string-match-p "not" see) (not found) found) nil
           (concat
            "Expected \"" actual "\" to include \"" expected "\"")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BUFFERS END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYBOARD START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (if espuds-chain-active
            (setq espuds-action-chain (vconcat espuds-action-chain (edmacro-parse-keys keybinding)))
          (execute-kbd-macro (edmacro-parse-keys keybinding)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYBOARD END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEXT START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEXT END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MOVEMENT START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Goes to LINE if it exist.
;;
;; Usage:
;;   When I go to line "12"
(When "^I go to line \"\\([0-9]+\\)\"$"
      (lambda (line)
        (let ((num-lines (count-lines (point-min) (point-max)))
              (line-num (string-to-number line)))
          (if (> line-num num-lines)
              (assert nil nil (concat "Tried to go to line " line ", but buffer does only have " (number-to-string num-lines) " lines."))
            (goto-line line-num)))))

;; Goes to POINT if it exist.
;;
;; Usage:
;;   When I go to point "12"
(When "^I go to point \"\\([0-9]+\\)\"$"
      (lambda (point)
        (let ((size (buffer-size)) (point-num (string-to-number point)))
          (if (> point-num size)
              (assert nil nil (concat "Tried to go to point " point ", but buffer does only have " (number-to-string size) " characters."))
            (goto-char point-num)))))

;; Goes to WORD if it exist and places the cursor in the middle of it.
;;
;; Usage:
;;   When I go to word "SOME WORD"
(When "^I go to word \"\\(.+\\)\"$"
      (lambda (word)
        (goto-char (point-min))
        (unless (re-search-forward (concat "\\b" word "\\b") nil t)
          (assert nil nil (concat "The word \"" word "\" does not exist in the current buffer.")))
        (backward-char (/ (length word) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MOVEMENT END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGION START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Deactivates mark.
;;
;; Usage:
;;   Given there is no region selected
(Given "^there is no region selected$"
       (lambda ()
         (deactivate-mark)))

;; Activates transient mark mode.
;;
;; Usage:
;;   Given transient mark mode is active
;;   Given transient mark mode is inactive
(Given "^transient mark mode is \\(active\\|inactive\\)$"
       (lambda (status)
         (transient-mark-mode
          (if (string= status "active") 1 -1))))

;; Sets the mark at point.
;;
;; Usage:
;;   When I set the mark
(When "^I set the mark$"
      (lambda ()
        (set-mark (point))))

;; Asserts that the selected region is same as EXPECTED.
;;
;; Usage:
;;   Then selected region should be "REGION"
;;
;;   Then selected region should be:
;;   """
;;   REGION
;;   """
(Then "^selected region should be\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-region)))
          (assert
           (equal expected actual) nil
           (concat "Expected region \"" actual "\" to include \"" expected "\"")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGION END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OTHER START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OTHER END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPERS START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun espuds-fake-eval (contents)
  "Dumps contents to a temp file and then loads it."
  (let ((file (make-temp-file "ecukes-")))
    (with-temp-file file
      (insert contents))
    (load file)))

(defun espuds-buffer-contents ()
  "Returns all text in current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun espuds-region ()
  "Returns the text selected by region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPERS END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'espuds)

;;; espuds.el ends here
