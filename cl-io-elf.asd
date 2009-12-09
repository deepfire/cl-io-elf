;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :cl-io-elf
  :depends-on (:alexandria :bintype :iterate :pergamum)
  :components
  ((:file "packages")
   (:file "elf" :depends-on ("packages"))))
