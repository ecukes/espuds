(require 'f)
(require 'el-mock)
(eval-when-compile
  (require 'cl)) ;; for el-mock

(defvar espuds-test/test-path
  (f-dirname load-file-name))

(defvar espuds-test/root-path
  (f-parent espuds-test/test-path))

(defvar espuds-test/vendor-path
  (f-expand "vendor" espuds-test/root-path))

(defmacro with-playground (&rest body)
  `(let ((buffer-name "*espuds*"))
     (if (get-buffer buffer-name)
         (kill-buffer buffer-name))
     (switch-to-buffer (get-buffer-create buffer-name))
     ,@body))

(load (f-expand "espuds-steps" espuds-test/test-path))

(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" espuds-test/vendor-path)))

(require 'espuds (f-expand "espuds" espuds-test/root-path))


;; This fixes an issue in emacs 25 where the debugger would be invoked
;; incorrectly, breaking various things in ert and espuds. In real usage the
;; issue is fixed by an advice in ecukes.
(when (and (= emacs-major-version 25))
  (require 'cl-preloaded)
  (setf (symbol-function 'cl--assertion-failed)
        (lambda (form &optional string sargs args)
          "This function has been modified by espuds to remove an incorrect manual call
to the debugger in emacs 25.1. The modified version should only be used for
running the espuds tests."
          (if string
              (apply #'error string (append sargs args))
            (signal 'cl-assertion-failed `(,form ,@sargs))))))
