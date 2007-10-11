(defpackage elf
  (:use :common-lisp :alexandria :bintype))

(in-package :elf)

(defbintype phdr
    "ELF program header"
  (:imm	type	(enum :unsigned-byte-32
		      '((#x0 :pt-null) (#x1 :pt-load) (#x2 :pt-dynamic) (#x3 :pt-interp)
			(#x4 :pt-note) (#x5 :pt-shlib) (#x6 :pt-phdr) (#x7 :pt-tls)
			(#x6474e550 :pt-gnu-eh-frame) (#x6474e551 :pt-gnu-stack)
			(#x6474e552 :pt-gnu-relro)
			(#x60000000 :pt-loos) (#x6fffffff :pt-hios)
			(#x70000000 :pt-loproc) (#x7fffffff :pt-hiproc))))
  (:imm	offt	(plain :unsigned-byte-32))
  (:imm	vaddr	(plain :unsigned-byte-32))
  (:imm	paddr	(plain :unsigned-byte-32))
  (:imm	filesz	(plain :unsigned-byte-32))
  (:imm	memsz	(plain :unsigned-byte-32))
  (:imm	flags	(plain :unsigned-byte-32))
  (:imm	align	(plain :unsigned-byte-32)))

;; aspects:
;;  1 in-flow?
;;  1.1 yes -- effect on the flow
;;  1.2 no -- offset
;;  2 storage type
;;  3 final type
;;  4 dependency
;;  4.1 offset in the dependency

(defbintype shdr
    "ELF section header"
  (:dep	name	  ;(plain :null-terminated-string)		;; final type
	          ;(ref (parent) 'shdr (ref (parent) 'shstrndx))	;; dependency
		  (plain :unsigned-byte-32))
		  ;)
  (:imm	type	  (enum :unsigned-byte-32 '((#x0 :sht-null) (#x1 :sht-progbits) (#x2 :sht-symtab)
					    (#x3 :sht-strtab) (#x4 :sht-rela) (#x5 :sht-hash)
					    (#x6 :sht-dynamic) (#x7 :sht-note) (#x8 :sht-nobits)
					    (#x9 :sht-rel) (#xa :sht-shlib) (#xb :sht-dynsym)
					    (#xe :sht-init-array) (#xf :sht-fini-array)
					    (#x10 :sht-preinit-array) (#x60000000 :sht-loos)
					    (#x6ffffff6 :sht-gnu-hash) (#x6ffffff7 :sht-gnu-liblist)
					    (#x6ffffffd :sht-gnu-verdef) (#x6ffffffe :sht-gnu-verneed)
					    (#x6fffffff :sht-gnu-versym) (#x70000000 :sht-loproc)
					    (#x7fffffff :sht-hiproc) (#x80000000 :sht-louser)
					    (#xffffffff :sht-hiuser))))
  (:imm	flags	  (plain :unsigned-byte-32))
  (:imm	addr	  (plain :unsigned-byte-32))
  (:imm	offt	  (plain :unsigned-byte-32))
  (:imm	size	  (plain :unsigned-byte-32))
  (:imm	link	  (plain :unsigned-byte-32))
  (:imm	info	  (plain :unsigned-byte-32))
  (:imm	addralign (plain :unsigned-byte-32))
  (:imm	entsize   (plain :unsigned-byte-32)))

(defbintype ehdr
    "ELF header"
  (:mag id-magic	(sequence :unsigned-byte-8 :count 4 :stride 1 :format 'list)
			'(#x7f #x45 #x4c #x46))
  (:imm id-class	(enum :unsigned-byte-8 '((#x0 :none) (#x1 :32) (#x2 :64))))
  (:imm id-data		(enum :unsigned-byte-8 '((#x0 :none) 
						 (#x1 (:lsb (set-endianness :little-endian)))
						 (#x2 (:msb (set-endianness :big-endian))))))
  (:imm id-version	(plain :unsigned-byte-8))
  (:pad nil		(seek 1))
  (:imm id-name		(zero-terminate-string (sequence :unsigned-byte-8 :count 8 :stride 1
							 :format 'vector)))
  (:imm type		(enum :unsigned-byte-16
			      '((#x0 :et-none) (#x1 :et-rel) (#x2 :et-exec) (#x3 :et-dyn)
				(#x4 :et-core) (#xff00 :et-loproc) (#xffff :et-hiproc))))
  (:imm machine		(enum :unsigned-byte-16 '((0 :em-none) (1 :em-m32) (2 :em-sparc) (3 :em-386)
						  (4 :em-68k) (5 :em-88k) (7 :em-860) (8 :em-mips)
						  (9 :em-s370) (10 :em-mips-rs4-be) (11 :em-rs6000)
						  (15 :em-parisc) (16 :em-ncube) (17 :em-vpp500)
						  (18 :em-sparc32plus) (19 :em-960) (20 :em-ppc)
						  (36 :em-v800) (37 :em-fr20) (38 :em-rh32)
						  (39 :em-mma) (40 :em-arm) (41 :em-fake-alpha)
						  (42 :em-sh) (43 :em-sparcv9) (44 :em-tricore)
						  (45 :em-arc) (46 :em-h8-300) (47 :em-h8-300h)
						  (48 :em-h8s) (49 :em-h8-500) (50 :em-ia-64)
						  (51 :em-mips-x) (52 :em-coldfire) (53 :em-68hc12))))
  (:imm version		(enum :unsigned-byte-32 '((0 :none) (1 :1))))
  (:imm entry		(plain :unsigned-byte-32))
  (:imm phoff		(plain :unsigned-byte-32))
  (:imm shoff		(plain :unsigned-byte-32))
  (:imm flags		(plain :unsigned-byte-32))
  (:imm ehsize		(plain :unsigned-byte-16))
  (:imm phentsize	(plain :unsigned-byte-16))
  (:imm phnum		(plain :unsigned-byte-16))
  (:imm shentsize	(plain :unsigned-byte-16))
  (:imm shnum		(plain :unsigned-byte-16))
  (:imm shstrndx	(plain :unsigned-byte-16))
  (:seq phdrs		(out-of-stream-absolute (sref 'phoff)
			   (sequence 'phdr :count (sref 'phnum) :format 'list
					   :stride (sref 'phentsize))))
  (:seq shdrs		(out-of-stream-absolute (sref 'shoff)
			   (sequence 'shdr :count (sref 'shnum) :format 'list
					   :stride (sref 'shentsize)))))

(mapc (compose #'export-bintype-accessors #'bintype) '(ehdr phdr shdr))

;; (let ((test-file-name "/bin/ls"))
;;   (format t "testing ~S:~%~S~%"
;; 	  test-file-name
;; 	  (with-open-file (str test-file-name :element-type '(unsigned-byte 8))
;; 	    (let ((seq (captured-stream:make-captured-stream str)))
;; 	      (parse-binary seq (bintype 'ehdr))))))
