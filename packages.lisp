(defpackage elf
  (:use :common-lisp :alexandria :bintype :iterate :pergamum)
  (:export
   #:phdr #:shdr #:ehdr
   #:section #:simple-section #:standard-section #:section-name #:section-executable-p #:section-file-offset
   #:shdr-loadable-p #:shdr-executable-p
   #:ehdr-sections #:elf-file-section))
