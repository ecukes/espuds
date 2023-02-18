(package "espuds"
         "0.3.3"
         "Ecukes step definitions")

(website-url "http://github.com/ecukes/espuds")
(keywords "test")

(package-file "espuds.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "25")
(depends-on "dash")
(depends-on "f")
(depends-on "s")

(development
 (depends-on "el-mock")
 (depends-on "ert-runner"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
