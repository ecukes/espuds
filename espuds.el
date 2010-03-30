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


(Given "^I am in the buffer \"\\(.+\\)\"$"
       (lambda (buffer)
         (switch-to-buffer (get-buffer-create buffer))))

(Given "^the buffer is empty$"
       (lambda ()
         (erase-buffer)))

(When "I insert this contents \"\\(.+\\)\""
      (lambda (contents)
        (insert contents)))

(When "^I insert this contents:$"
      (lambda (contents)
        (insert contents)))

(When "^I select \"\\(.+\\)\"$"
      (lambda (text)
        (goto-char (point-min))
        (re-search-forward text)
        (set-mark (point))
        (re-search-backward text)))

(When "^I start an action chain$"
      (lambda ()
        (setq espuds-action-chain '())
        (setq espuds-chain-active t)))

(When "^I execute the action chain$"
      (lambda ()
        (execute-kbd-macro espuds-action-chain)
        (setq espuds-chain-active nil)))

(When "^I press \"\\(.+\\)\"$"
      (lambda (keybinding)
        (if espuds-chain-active
            (setq espuds-action-chain (vconcat espuds-action-chain (edmacro-parse-keys keybinding)))
          (execute-kbd-macro (edmacro-parse-keys keybinding)))))

(Given "^I type \"\\(.+\\)\"$"
       (lambda (typing)
         (if espuds-chain-active
             (setq espuds-action-chain (vconcat espuds-action-chain (string-to-vector typing)))
           (execute-kbd-macro (string-to-vector typing)))))

(Then "^I should\\( not \\| \\)see\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (see contents)
        (let ((hit (search contents (espuds-buffer-contents))))
          (assert
           (if (string-match-p "not" see) (not hit) hit)))))

(Then "^I should\\( not \\| \\)see pattern\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (see contents)
        (let ((hit (string-match-p contents (espuds-buffer-contents))))
          (assert
           (if (string-match-p "not" see) (not hit) hit)))))

(Given "^there is no region selected$"
       (lambda ()
         (deactivate-mark)))

(Given "^transient mark mode is active$"
       (lambda ()
         (transient-mark-mode 1)))

(Given "^the following is loaded:$"
       (lambda (contents)
         (espuds-fake-eval contents)))

(When "I am in the temp file \"\\(.+\\)\""
      (lambda (file)
        (find-file (make-temp-file file))))

(Given "^I am on line \"\\([0-9]+\\)\"$"
       (lambda (line)
         (goto-line (string-to-number line))))

(Then "^I should see message \"\\(.+\\)\"$"
      (lambda (message)
        (save-excursion
          (set-buffer "*Messages*")
          (assert (search message (buffer-substring-no-properties (point-min) (point-max)))))))

(When "I set the mark"
      (lambda ()
        (set-mark (point))))

(When "I go to point \"\\([0-9]+\\)\""
      (lambda (point)
        (goto-char (string-to-number point))))

(When "I go to word \"\\(.+\\)\""
      (lambda (word)
        (goto-char (point-min))
        (re-search-forward (concat "\\b" word "\\b"))
        (backward-char (/ (length word) 2))))

(Then "selected region should be\\(?: \"\\(.+\\)\"\\|:\\)"
      (lambda (region)
        (assert (equal (buffer-substring-no-properties (region-beginning) (region-end)) region))))


(defun espuds-fake-eval (contents)
  "Dumps contents to a temp file and then loads it."
  (let ((file (make-temp-file "ecukes-")))
    (with-temp-file file
      (insert contents))
    (load file)))

(defun espuds-buffer-contents ()
  "Returns all text in current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(provide 'espuds)

;;; espuds.el ends here
