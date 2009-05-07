;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ELF; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2008 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :elf)

(defbintype phdr-32 ()
  (:documentation "32-bit ELF program header")
  (:prefix phdr-)
  (:fields
   (match type		(unsigned-byte 32)
	 		((#x0 :pt-null) (#x1 :pt-load) (#x2 :pt-dynamic) (#x3 :pt-interp)
			 (#x4 :pt-note) (#x5 :pt-shlib) (#x6 :pt-phdr) (#x7 :pt-tls)
			 (#x6474e550 :pt-gnu-eh-frame) (#x6474e551 :pt-gnu-stack)
			 (#x6474e552 :pt-gnu-relro)
			 (#x60000000 :pt-loos) (#x6fffffff :pt-hios)
			 (#x70000000 :pt-loproc) (#x7fffffff :pt-hiproc)))
   (value offt		(unsigned-byte 32))
   (value vaddr		(unsigned-byte 32))
   (value paddr		(unsigned-byte 32))
   (value filesz	(unsigned-byte 32))
   (value memsz		(unsigned-byte 32))
   (value flags		(unsigned-byte 32))
   (value align		(unsigned-byte 32))))

(defbintype phdr-64 ()
  (:documentation "64-bit ELF program header")
  (:prefix phdr-)
  (:fields
   (match type		(unsigned-byte 32)
          ((#x0 :pt-null) (#x1 :pt-load) (#x2 :pt-dynamic) (#x3 :pt-interp)
           (#x4 :pt-note) (#x5 :pt-shlib) (#x6 :pt-phdr) (#x7 :pt-tls)
           (#x6474e550 :pt-gnu-eh-frame) (#x6474e551 :pt-gnu-stack)
           (#x6474e552 :pt-gnu-relro)
           (#x60000000 :pt-loos) (#x6fffffff :pt-hios)
           (#x70000000 :pt-loproc) (#x7fffffff :pt-hiproc)))
   (value flags		(unsigned-byte 32))
   (value offt		(unsigned-byte 64))
   (value vaddr		(unsigned-byte 64))
   (value paddr		(unsigned-byte 64))
   (value filesz	(unsigned-byte 64))
   (value memsz		(unsigned-byte 64))
   (value align		(unsigned-byte 64))))

(defbintype shdr-32 ()
  (:documentation "32-bit ELF section header")
  (:prefix shdr-)
  (:fields
   (indirect name	(unsigned-byte 32)
      (value name	(terminated-symbol 32 0 :elf)
			:out-of-stream-offset  (ash (+ (path-value *self* :parent
                                                                   (path-value *self* :parent :parent 'shstrndx) 'offt)
                                                       *direct-value*)
                                                    3)))
   (match type		(unsigned-byte 32)
	 		((#x0 :sht-null) (#x1 :sht-progbits) (#x2 :sht-symtab)
			 (#x3 :sht-strtab) (#x4 :sht-rela) (#x5 :sht-hash)
			 (#x6 :sht-dynamic) (#x7 :sht-note) (#x8 :sht-nobits)
			 (#x9 :sht-rel) (#xa :sht-shlib) (#xb :sht-dynsym)
			 (#xe :sht-init-array) (#xf :sht-fini-array) (#x10 :sht-preinit-array)
			 (#x60000000 :sht-loos) (#x6ffffff6 :sht-gnu-hash) (#x6ffffff7 :sht-gnu-liblist)
			 (#x6ffffffd :sht-gnu-verdef) (#x6ffffffe :sht-gnu-verneed)(#x6fffffff :sht-gnu-versym)
;; 			 (#x70000000 :sht-loproc)
                         (#x70000000 :sht-mips-liblist) (#x70000002 :sht-mips-conflict) (#x70000003 :sht-mips-gptab)
                         (#x70000004 :sht-mips-ucode)
			 (#x70000005 :sht-mips-debug) (#x70000006 :sht-mips-reginfo) (#x7000001e :sht-mips-dwarf)
			 (#x7fffffff :sht-hiproc)
			 (#x80000000 :sht-louser) (#xffffffff :sht-hiuser)))
   (flag shf-write)
   (flag shf-alloc)
   (flag shf-execinstr)
   (value shf-pad        (unsigned-byte 25) :ignore t)
   (flag shf-mips-gprel)
   (value shf-pad        (unsigned-byte 3) :ignore t)
   (value addr		 (unsigned-byte 32))
   (value offt		 (unsigned-byte 32))
   (value size		 (unsigned-byte 32))
   (value link		 (unsigned-byte 32))
   (value info		 (unsigned-byte 32))
   (value addralign	 (unsigned-byte 32))
   (value entsize	 (unsigned-byte 32))
   (value data           (displaced-u8-vector (if (eq (path-value *self* 'type) :sht-nobits) 0 (path-value *self* 'size))) :out-of-stream-offset (ash (path-value *self* 'offt) 3))))

(defbintype shdr-64 ()
  (:documentation "64-bit ELF section header")
  (:prefix shdr-)
  (:fields
   (indirect name	(unsigned-byte 32)
             (value name	(terminated-symbol 32 0 :elf)
                    :out-of-stream-offset  (ash (+ (path-value *self* :parent
                                                               (path-value *self* :parent :parent 'shstrndx) 'offt)
                                                   *direct-value*)
                                                3)))
   (match type		(unsigned-byte 32)
          ((#x0 :sht-null) (#x1 :sht-progbits) (#x2 :sht-symtab)
           (#x3 :sht-strtab) (#x4 :sht-rela) (#x5 :sht-hash)
           (#x6 :sht-dynamic) (#x7 :sht-note) (#x8 :sht-nobits)
           (#x9 :sht-rel) (#xa :sht-shlib) (#xb :sht-dynsym)
           (#xe :sht-init-array) (#xf :sht-fini-array) (#x10 :sht-preinit-array)
           (#x60000000 :sht-loos) (#x6ffffff6 :sht-gnu-hash) (#x6ffffff7 :sht-gnu-liblist)
           (#x6ffffffd :sht-gnu-verdef) (#x6ffffffe :sht-gnu-verneed)(#x6fffffff :sht-gnu-versym)
           ;; 			 (#x70000000 :sht-loproc)
           (#x70000000 :sht-mips-liblist) (#x70000002 :sht-mips-conflict) (#x70000003 :sht-mips-gptab)
           (#x70000004 :sht-mips-ucode)
           (#x70000005 :sht-mips-debug) (#x70000006 :sht-mips-reginfo) (#x7000001e :sht-mips-dwarf)
           (#x7fffffff :sht-hiproc)
           (#x80000000 :sht-louser) (#xffffffff :sht-hiuser)))
   (flag shf-write)
   (flag shf-alloc)
   (flag shf-execinstr)
   (value shf-pad        (unsigned-byte 61) :ignore t)
   (value addr		 (unsigned-byte 64))
   (value offt		 (unsigned-byte 64))
   (value size		 (unsigned-byte 64))
   (value link		 (unsigned-byte 32))
   (value info		 (unsigned-byte 32))
   (value addralign	 (unsigned-byte 64))
   (value entsize	 (unsigned-byte 64))
   (value data           (displaced-u8-vector (if (eq (path-value *self* 'type) :sht-nobits) 0 (path-value *self* 'size))) :out-of-stream-offset (ash (path-value *self* 'offt) 3))))

(defbintype ehdr-body-32 ()
  (:documentation "ELF header body, 32 bit version")
  (:prefix ehdr-)
  (:fields
   (match type		(unsigned-byte 16)
          ((#x0 :et-none) (#x1 :et-rel) (#x2 :et-exec) (#x3 :et-dyn)
           (#x4 :et-core) (#xff00 :et-loproc) (#xffff :et-hiproc)))
   (match machine	(unsigned-byte 16)
          ((0 :em-none) (1 :em-m32) (2 :em-sparc) (3 :em-386)
           (4 :em-68k) (5 :em-88k) (7 :em-860) (8 :em-mips)
           (9 :em-s370) (10 :em-mips-rs4-be) (11 :em-rs6000)
           (15 :em-parisc) (16 :em-ncube) (17 :em-vpp500)
           (18 :em-sparc32plus) (19 :em-960) (20 :em-ppc)
           (36 :em-v800) (37 :em-fr20) (38 :em-rh32)
           (39 :em-mma) (40 :em-arm) (41 :em-fake-alpha)
           (42 :em-sh) (43 :em-sparcv9) (44 :em-tricore)
           (45 :em-arc) (46 :em-h8-300) (47 :em-h8-300h)
           (48 :em-h8s) (49 :em-h8-500) (50 :em-ia-64)
           (51 :em-mips-x) (52 :em-coldfire) (53 :em-68hc12) (62 :em-x86-64)))
   (match version	(unsigned-byte 32)
          ((0 :none) (1 :1)))
   (value entry		(unsigned-byte 32))
   (value phoff		(unsigned-byte 32))
   (value shoff		(unsigned-byte 32))
   (flag ef-mips-noreorder)
   (flag ef-mips-pic)
   (flag ef-mips-cpic)
   (flag ef-mips-arch)
   (value ef-pad        (unsigned-byte 28) :ignore t)
   (value ehsize	(unsigned-byte 16))
   (value phentsize	(unsigned-byte 16))
   (value phnum		(unsigned-byte 16))
   (value shentsize	(unsigned-byte 16))
   (value shnum		(unsigned-byte 16))
   (value shstrndx	(unsigned-byte 16))
   (value phdrs		(sequence (path-value *self* 'phnum) :element-type phdr-32 :stride (ash (path-value *self* 'phentsize) 3) :format :list)
          :out-of-stream-offset (ash (path-value *self* 'phoff) 3))
   (value shdrs		(sequence (path-value *self* 'shnum) :element-type shdr-32 :stride (ash (path-value *self* 'shentsize) 3) :format :list)
          :out-of-stream-offset (ash (path-value *self* 'shoff) 3))))

(defbintype ehdr-body-64 ()
  (:documentation "ELF header body, 64 bit version")
  (:prefix ehdr-)
  (:fields
   (match type		(unsigned-byte 16)
          ((#x0 :et-none) (#x1 :et-rel) (#x2 :et-exec) (#x3 :et-dyn)
           (#x4 :et-core) (#xff00 :et-loproc) (#xffff :et-hiproc)))
   (match machine	(unsigned-byte 16)
          ((0 :em-none) (1 :em-m32) (2 :em-sparc) (3 :em-386)
           (4 :em-68k) (5 :em-88k) (7 :em-860) (8 :em-mips)
           (9 :em-s370) (10 :em-mips-rs4-be) (11 :em-rs6000)
           (15 :em-parisc) (16 :em-ncube) (17 :em-vpp500)
           (18 :em-sparc32plus) (19 :em-960) (20 :em-ppc)
           (36 :em-v800) (37 :em-fr20) (38 :em-rh32)
           (39 :em-mma) (40 :em-arm) (41 :em-fake-alpha)
           (42 :em-sh) (43 :em-sparcv9) (44 :em-tricore)
           (45 :em-arc) (46 :em-h8-300) (47 :em-h8-300h)
           (48 :em-h8s) (49 :em-h8-500) (50 :em-ia-64)
           (51 :em-mips-x) (52 :em-coldfire) (53 :em-68hc12) (62 :em-x86-64)))
   (match version	(unsigned-byte 32)
          ((0 :none) (1 :1)))
   (value entry		(unsigned-byte 64))
   (value phoff		(unsigned-byte 64))
   (value shoff		(unsigned-byte 64))
   (flag ef-mips-noreorder)
   (flag ef-mips-pic)
   (flag ef-mips-cpic)
   (flag ef-mips-arch)
   (value ef-pad        (unsigned-byte 28) :ignore t)
   (value ehsize	(unsigned-byte 16))
   (value phentsize	(unsigned-byte 16))
   (value phnum		(unsigned-byte 16))
   (value shentsize	(unsigned-byte 16))
   (value shnum		(unsigned-byte 16))
   (value shstrndx	(unsigned-byte 16))
   (value phdrs		(sequence (path-value *self* 'phnum) :element-type phdr-64 :stride (ash (path-value *self* 'phentsize) 3) :format :list)
          :out-of-stream-offset (ash (path-value *self* 'phoff) 3))
   (value shdrs		(sequence (path-value *self* 'shnum) :element-type shdr-64 :stride (ash (path-value *self* 'shentsize) 3) :format :list)
          :out-of-stream-offset (ash (path-value *self* 'shoff) 3))))

(defbintype ehdr ()
  (:documentation "ELF header")
  (:type :structure)
  (:fields
   (match id-magic	(sequence 4 :element-type (unsigned-byte 8) :stride 8 :format :list)
          ((#(#x7f #x45 #x4c #x46))) :ignore t)
   (match id-class	(unsigned-byte 8)
          ((#x0 :none) (#x1 :32) (#x2 :64)))
   (match id-data	(unsigned-byte 8)
          ((#x0 :none)
           (#x1 (set-endianness :little-endian) :lsb)
           (#x2 (set-endianness :big-endian) :msb)))
   (value id-version	(unsigned-byte 8))
   (value nil		(unsigned-byte 8) :ignore t)
   (value id-name	(sequence 8 :element-type (unsigned-byte 8) :stride 8 :format :vector))
   (value body          (typecase (path-value *self* 'id-class)
                          (:32 ehdr-body-32)
                          (:64 ehdr-body-64)))))

(defun shdr-loadable-p (shdr)
  (with-slots (size shf-write shf-alloc shf-execinstr type) shdr
    (not (or (and (not (or shf-write shf-alloc shf-execinstr)))
             (zerop size) (eq type :sht-nobits)))))

(defun shdr-executable-p (shdr)
  (shdr-shf-execinstr shdr))

(mapc (compose #'export-bintype #'bintype) '(ehdr ehdr-body-32 phdr-32 shdr-32 ehdr-body-64 phdr-64 shdr-64))

;; (let ((test-file-name "/home/deepfire/bin/ls"))
;;   (format t "testing ~S:~%~S~%"
;; 	  test-file-name
;; 	  (with-open-file (str test-file-name :element-type '(unsigned-byte 8))
;; 	    (let ((seq (captured-stream:make-captured-stream str)))
;; 	      (parse 'ehdr seq)))))

;;;
;;; No better place for it, as of now -- need for a common executable framework...
;;;
(defclass section ()
  ((name :accessor section-name :initarg :name)
   (executable-p :accessor section-executable-p :initarg :executable-p)
   (file-offset :accessor section-file-offset :initarg :file-offset))
  (:documentation "Base section class, do not instantiate."))

(defclass simple-section (section baseless-extent)
  ()
  (:documentation "Section with no specified base."))

(defclass standard-section (section extent)
  ()
  (:documentation "Standard section with specified base."))

(defun ehdr-sections (ehdr predicate &aux (relocatable-p (eq (ehdr-type (ehdr-body ehdr)) :et-rel)))
  (iter (for shdr in (ehdr-shdrs (ehdr-body ehdr)))
        (when (funcall predicate shdr)
          (collect (apply #'make-instance (if relocatable-p 'simple-section 'standard-section)
                    :data (shdr-data shdr) :name (shdr-name shdr) :executable-p (shdr-executable-p shdr)
                    :file-offset (shdr-offt shdr) (unless relocatable-p `(:base ,(shdr-addr shdr))))))))

(defun elf-file-section (file name)
  (let ((sections (ehdr-sections (parse 'ehdr (file-as-vector file)) #'identity)))
   (or (find name sections :key #'section-name)
       (error "section ~S not found in ~S, candidates: ~S" name file (mapcar #'section-name sections)))))
