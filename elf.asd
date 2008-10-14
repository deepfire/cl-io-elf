;;; -*- Mode: Lisp -*-

(defpackage :elf.system
  (:use :cl :asdf))

(in-package :elf.system)

(defsystem :elf
  :depends-on (:alexandria :bintype :iterate :pergamum)
  :components
  ((:file "packages")
   (:file "elf" :depends-on ("packages"))))
