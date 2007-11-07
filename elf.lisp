(defpackage elf
  (:use :common-lisp :alexandria :bintype)
  (:export
   #:phdr #:shdr #:ehdr))

(in-package :elf)

(defbintype phdr ()
  (:documentation "ELF program header")
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

(defbintype shdr ()
  (:documentation "ELF section header")
  (:fields
   (indirect		(unsigned-byte 32)
      (value name	(zero-terminated-symbol 32 :elf)
			:out-of-stream-offset  (+ (path-value *self* :parent
							      (path-value *self* '(:typed-parent ehdr) 'shstrndx) 'offt)
						  *direct-value*)))
   (match type		(unsigned-byte 32)
	 		((#x0 :sht-null) (#x1 :sht-progbits) (#x2 :sht-symtab)
			 (#x3 :sht-strtab) (#x4 :sht-rela) (#x5 :sht-hash)
			 (#x6 :sht-dynamic) (#x7 :sht-note) (#x8 :sht-nobits)
			 (#x9 :sht-rel) (#xa :sht-shlib) (#xb :sht-dynsym)
			 (#xe :sht-init-array) (#xf :sht-fini-array)
			 (#x10 :sht-preinit-array) (#x60000000 :sht-loos)
			 (#x6ffffff6 :sht-gnu-hash) (#x6ffffff7 :sht-gnu-liblist)
			 (#x6ffffffd :sht-gnu-verdef) (#x6ffffffe :sht-gnu-verneed)
			 (#x6fffffff :sht-gnu-versym) (#x70000000 :sht-loproc)
			 (#x7fffffff :sht-hiproc) (#x80000000 :sht-louser)
			 (#xffffffff :sht-hiuser)))
   (value flags		(unsigned-byte 32))
   (value addr		(unsigned-byte 32))
   (value offt		(unsigned-byte 32))
   (value size		(unsigned-byte 32))
   (value link		(unsigned-byte 32))
   (value info		(unsigned-byte 32))
   (value addralign	(unsigned-byte 32))
   (value entsize	(unsigned-byte 32))))

(defbintype ehdr ()
  (:documentation "ELF header")
  (:fields
   (match id-magic	(sequence 4 :element-type (unsigned-byte 8) :stride 1 :format :list)
	 		(((#x7f #x45 #x4c #x46))) :ignore t)
   (match id-class	(unsigned-byte 8)
	 		((#x0 :none) (#x1 :32) (#x2 :64)))
   (match id-data	(unsigned-byte 8)
	 		((#x0 :none)
			 (#x1 (set-endianness :little-endian) :lsb)
			 (#x2 (set-endianness :big-endian) :msb)))
   (value id-version	(unsigned-byte 8))
   (value nil		(unsigned-byte 8) :ignore t)
   (value id-name	(sequence 8 :element-type (unsigned-byte 8) :stride 1 :format :vector))
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
			 (51 :em-mips-x) (52 :em-coldfire) (53 :em-68hc12)))
   (match version	(unsigned-byte 32)
			((0 :none) (1 :1)))
   (value entry		(unsigned-byte 32))
   (value phoff		(unsigned-byte 32))
   (value shoff		(unsigned-byte 32))
   (value flags		(unsigned-byte 32))
   (value ehsize	(unsigned-byte 16))
   (value phentsize	(unsigned-byte 16))
   (value phnum		(unsigned-byte 16))
   (value shentsize	(unsigned-byte 16))
   (value shnum		(unsigned-byte 16))
   (value shstrndx	(unsigned-byte 16))
   (value phdrs		(sequence (path-value *self* 'phnum) :element-type phdr :stride (path-value *self* 'phentsize) :format :list)
	  		:out-of-stream-offset (path-value *self* 'phoff))
   (value shdrs		(sequence (path-value *self* 'shnum) :element-type shdr :stride (path-value *self* 'shentsize) :format :list)
	  		:out-of-stream-offset (path-value *self* 'shoff))))

(mapc (compose #'export-bintype-accessors #'bintype) '(ehdr phdr shdr))

;; (let ((test-file-name "/home/deepfire/bin/ls"))
;;   (format t "testing ~S:~%~S~%"
;; 	  test-file-name
;; 	  (with-open-file (str test-file-name :element-type '(unsigned-byte 8))
;; 	    (let ((seq (captured-stream:make-captured-stream str)))
;; 	      (parse (bintype 'ehdr) seq)))))
