(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)
(require 'edmacro)

(defvar espuds-test/test-path
  (f-dirname load-file-name))

(defvar espuds-test/root-path
  (f-parent espuds-test/test-path))

(defvar espuds-test/vendor-path
  (f-expand "vendor" espuds-test/root-path))

(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" espuds-test/vendor-path)))

(add-to-list 'load-path espuds-test/root-path)
