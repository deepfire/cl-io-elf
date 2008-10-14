;;; -*- Mode: Lisp -*-

(defpackage :cl-io-elf.system
  (:use :cl :asdf))

(in-package :cl-io-elf.system)

(defsystem :cl-io-elf
  :depends-on (:alexandria :bintype :iterate :pergamum)
  :components
  ((:file "packages")
   (:file "elf" :depends-on ("packages"))))
