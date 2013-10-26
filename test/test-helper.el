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
