(defvar espuds-steps nil)

(defalias 'Given 'espuds-define-or-call-step)
(defalias 'When 'espuds-define-or-call-step)
(defalias 'Then 'espuds-define-or-call-step)
(defalias 'And 'espuds-define-or-call-step)
(defalias 'But 'espuds-define-or-call-step)

(defun espuds-define-or-call-step (name &rest args)
  (when (= (length args) 2)
    (setq args (cdr args)))
  (let ((arg (car args)))
    (if (functionp arg)
        (espuds-define-step name arg)
      (espuds-call-step name args))))

(defun espuds-define-step (regex fn)
  (add-to-list 'espuds-steps `(,regex . ,fn)))

(defun espuds-call-step (name args)
  (let ((matches)
        (matching
         (catch 'break
           (mapc
            (lambda (step)
              (if (string-match-p (car step) name)
                  (throw 'break step)))
            espuds-steps))))
    (unless matching
      (error "No matching step for '%s'" name))
    (unless args
      (when (string-match (car matching) name)
        (let ((i 1))
          (while (match-string i name)
            (add-to-list 'matches (match-string i name) t 'eq)
            (setq i (1+ i))))))
    (apply (cdr matching) (or args matches))))

(defmacro with-playground (&rest body)
  `(let ((buffer-name "*espuds*"))
     (if (get-buffer buffer-name)
         (kill-buffer buffer-name))
     (switch-to-buffer (get-buffer-create buffer-name))
     ,@body))
