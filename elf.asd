(defpackage :elf.system
  (:use :cl :asdf))

(in-package :elf.system)

(defsystem :elf
  :depends-on (:alexandria :bintype)
  :components
  ((:file "elf")))
