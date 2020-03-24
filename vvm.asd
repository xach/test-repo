;;;; vvm.asd

(asdf:defsystem #:vvm
  :serial t
  :depends-on (#:zpb-ttf)
  :components ((:file "vvm")))
