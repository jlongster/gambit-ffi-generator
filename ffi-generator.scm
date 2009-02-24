#!/usr/local/bin/gsi-script


(include "type-parser.scm")
(include "ssax-sxml.scm")


;;;
;;;; gambit ffi base types (incomplete)
;;;


(define ffi-base-types
  '(int short char double float long unsigned-int unsigned-short unsigned-char unsigned-double unsigned-float unsigned-long char-string))


;;;
;;;; declare insertions
;;;


(define inserts '())


(define (add-insert str)
  (set! inserts (append inserts (list str))))


;;;
;;;; type resolving
;;;


(define type-table (make-table))


(define (get-type-expansion type)
  (table-ref type-table type #f))


(define (add-type-expansion type expansion)
  (table-set! type-table type expansion))


(define (expand-type type)
  (if (list? type)
      (let ((first (car type))
	    (rest (cdr type)))
	(cond ((memq first '(struct union enum))
	       type)
	      ((memq first '(pointer const))
	       (list first (expand-type rest)))
	      ((eq? first 'array)
	       (list 'array (car rest) (expand-type (cdr rest))))))
      (let ((expansion (get-type-expansion type)))
	(if expansion
	    (expand-type expansion)
	    type))))


;;;
;;;; sxml generation
;;;


(define (open-swig-port)
  (open-process
   (list 
    path: "swig"
    arguments: (list "-xml" "-o" "/dev/stdout" "/dev/stdin")
    directory: (current-directory)
    stdin-redirection: #t
    stdout-redirection: #t
    stderr-redirection: #f)))

  
(define (generate-sxml port)
  (ssax:xml->sxml port '()))


;;;
;;;; Types predicates
;;;


(define (type-is? type kind)
  (and (pair? type)
       (eq? (car type) kind)))


(define (struct-type? type)
  (type-is? type 'struct))


(define (pointer-type? type)
  (type-is? type 'pointer))


(define (array-type? type)
  (type-is? type 'array))


(define (enum-type? type)
  (type-is? type 'enum))


(define (base-type? type)
  (or (memq type ffi-base-types)
      (pointer-type? type)
      (enum-type? type)))


;;;
;;;; SWIG sxml helper functions
;;;


(define swig:attributes
  (node-join (select-kids (ntype?? 'attributelist))
             (select-kids (ntype?? 'attribute))))



(define (swig:node-has-attribute-with attr)
  (sxml:filter (node-join swig:attributes
			  (select-kids (ntype?? '@))
			  (select-kids (node-equal? attr)))))



(define (swig:attribute-with attr)
  (node-join swig:attributes
	     (sxml:filter (node-join (select-kids (ntype?? '@))
				     (select-kids (node-equal? attr))))))


(define (swig:attribute-with-name name)
  (swig:attribute-with `(name ,name)))


(define (swig:attribute-attr name)
  (node-join (select-kids (ntype?? '@))
	     (select-kids (ntype?? name))
	     sxml:child-nodes))


(define swig:attribute-value-attr
  (swig:attribute-attr 'value))


(define (swig:named-attribute-value name)
  (node-join (swig:attribute-with-name name)
	     swig:attribute-value-attr))


(define (swig:has-named-attribute-with-value name value)
  (sxml:filter (node-join (swig:attribute-with-name name)
			  (select-kids (ntype?? '@))
			  (select-kids (node-equal? `(value ,value))))))


(define swig:name-attribute-value
  (swig:named-attribute-value "name"))


(define swig:type-attribute-value
  (swig:named-attribute-value "type"))


(define swig:decl-attribute-value
  (swig:named-attribute-value "decl"))


(define (has-attribute name)
  (sxml:filter (node-join (select-kids (ntype?? 'attributelist))
                          (select-kids (ntype?? 'attribute))
                          (select-kids (ntype?? '@))
                          (select-kids (node-equal? `(name ,name))))))


(define (has-not-attribute name)
  (sxml:filter (sxml:complement (node-join (select-kids (ntype?? 'attributelist))
                                           (select-kids (ntype?? 'attribute))
                                           (select-kids (ntype?? '@))
					   (select-kids (node-equal? `(name ,name)))))))


;;;
;;;; Constants generation
;;;


(define filter-constants
  (sxml:filter (ntype?? 'constant)))


(define (generate-constant name type)
  `(define-constant ,(string->symbol name) ,type))


(define (generate-constants nodes)
  (let ((constants (filter-constants nodes)))
    (let ((names (swig:name-attribute-value constants))
	  (types (map parse-type ((swig:named-attribute-value "type") constants))))
      (map generate-constant names types))))


;;;
;;;; Enums generation
;;;


(define filter-enums
  (sxml:filter (ntype?? 'enum)))


(define (enum-constant-names enum-name)
  (node-join (swig:node-has-attribute-with `(value ,enum-name))
	     (select-kids (ntype?? 'enumitem))
	     swig:name-attribute-value))


(define (generate-enum enum-name constant-names)
  (let ((type `(enum ,enum-name)))
    (map (lambda (x) (generate-constant x type)) constant-names)))


(define (generate-enums nodes)
  (let* ((enums (filter-enums nodes))
	 (names (swig:name-attribute-value enums))
	 (constant-names (map (lambda (name) 
				((enum-constant-names name) enums))
			      names)))
    (apply append (map generate-enum names constant-names))))


;;;
;;;; Typedefs generation
;;;


(define filter-typedefs
  (node-join (sxml:filter (ntype?? 'cdecl))
	     (swig:has-named-attribute-with-value "kind" "typedef")))


(define (generate-typedef name type)
  (let ((name-sym (string->symbol name)))
    (add-type-expansion name-sym type)
    `(c-define-type ,name-sym ,type)))


(define (generate-typedefs nodes)
  (let* ((typedefs (filter-typedefs nodes))
	 (parsed-types (map (lambda (decl type)
			      (parse-type (string-append decl type)))
			    (swig:decl-attribute-value typedefs)
			    (swig:type-attribute-value typedefs))))
    (map generate-typedef (swig:name-attribute-value typedefs) parsed-types)))


;;;
;;;; Structs generation
;;;


(define (generate-getter struct-name struct-type member-name member-type expanded-member-type)
  (let ((sym (string->symbol (string-append struct-name "-" member-name)))
	(str (cond ((or (pointer-type? expanded-member-type)
			(array-type? expanded-member-type))
		    (string-append "___result_voidstar = ___arg1->" member-name ";"))
		   ((base-type? expanded-member-type)
		    (string-append "___result = ___arg1->" member-name ";"))
		   (else
		    (string-append "___result_voidstar = &___arg1->" member-name ";")))))
    `(define ,sym (c-lambda ((pointer ,struct-type)) ,member-type ,str))))
	


(define (generate-setter struct-name struct-type member-name member-type expanded-member-type)
  (if (or (pointer-type? expanded-member-type)
	  (base-type? expanded-member-type))
      (let ((sym (string->symbol (string-append struct-name "-" member-name "-set!")))
	    (str (string-append "___arg1->" member-name " = ___arg2;")))
	`((define ,sym (c-lambda ((pointer ,struct-type) ,member-type) void ,str))))
      ;; if member is a struct or array, it doesn't make sense to have a setter
      '()))


(define (generate-struct-accessors struct-name struct-type member-name member-type)
  (let ((expanded-member-type (let ((expanded-type (expand-type type)))
				(if (base-type? expanded-type) type `(pointer ,type)))))
    (cons (generate-getter struct-name struct-type member-name member-type expanded-member-type)
	  (generate-setter struct-name struct-type member-name member-type expanded-member-type))))


(define (generate-struct name type c-type-string member-names member-types)
  (let ((alloc `(define ,(string->symbol (string-append "alloc-" name))
		  (c-lambda () (pointer ,type)
		    ,(string-append "___result_voidstar = malloc(sizeof(" c-type-string "));")))))
    (apply append (list alloc) (map (lambda (member-name member-type)
				      (generate-struct-accessors name type member-name member-type))
				    member-names
				    member-types))))


(define (generate-named-struct name kind member-names member-types)
  (generate-struct name (list (string->symbol kind) name) (string-append kind " " name) member-names (map parse-type member-types)))


(define (generate-unnamed-struct name member-names member-types)
  (let* ((name-sym (string->symbol name))
	 (typedef  `(c-define-type ,name-sym (type ,name))))
    (cons typedef (generate-struct name name-sym name member-names (map parse-type member-types)))))



(define (build-type-strings struct)
  (let ((members ((select-kids (ntype?? 'cdecl)) struct)))
    (map (lambda (mem)
	   (let ((type (swig:type-attribute-value mem))
		 (decl (swig:decl-attribute-value mem)))
	     (cond ((and (null? type)
			 (null? decl))
		    "")
	           ((null? type) (car decl))
		   ((null? decl) (car type))
		   (else (string-append (car decl) (car type))))))
	 members)))


(define filter-structs
  (sxml:filter (ntype?? 'class)))


(define (struct-members name)
  (node-join (sxml:filter (node-join (swig:attribute-with-name "name")
				     (select-kids (ntype?? '@))
				     (select-kids (node-equal? `(value ,name)))))
	     (select-kids (ntype?? 'cdecl))))

 
(define (struct-members-names name)
  (node-join (struct-members name)
	     (swig:named-attribute-value "name")))

 
(define (struct-members-types name)
  (node-join (struct-members name)
	     swig:type-attribute-value))


(define (struct-members-decls name)
  (node-join (struct-members name)
	     swig:decl-attribute-value))


(define struct-inserts-code
  (node-join (select-kids (ntype?? 'insert))
	     (select-kids (ntype?? 'attributelist))
	     (select-kids (ntype?? 'attribute))
	     (sxml:attribute (ntype?? 'value))
	     sxml:child-nodes))


(define (generate-unnamed-structs structs names member-names type-strings)
  (apply append (map generate-unnamed-struct
		     names
		     member-names
		     type-strings)))
    

(define (generate-named-structs structs names member-names type-strings)
  (let ((kinds ((swig:named-attribute-value "kind") structs)))
    (apply append (map generate-named-struct
		       names
		       kinds
		       member-names
		       type-strings))))
    

(define (extract-structs-and-generate structs generator)
  (let ((names (swig:name-attribute-value structs))
	(type-strings (map build-type-strings structs)))
    (let ((member-names (map (lambda (name)
			       ((struct-members-names name) structs))
			     names)))
      (generator structs names member-names type-strings))))


(define (generate-structs nodes)
  (let ((structs (filter-structs nodes)))
    ;; Implicitly inserted code for internal structures and such
    (for-each add-insert (struct-inserts-code structs))
    (append (extract-structs-and-generate ((has-attribute "storage") structs) generate-unnamed-structs)
	    (extract-structs-and-generate ((has-not-attribute "storage") structs) generate-named-structs))))


;;;
;;;; Functions generation
;;;


(define filter-functions
  (node-join (sxml:filter (ntype?? 'cdecl))
	     (swig:has-named-attribute-with-value "kind" "function")))


(define (get-parm-types name)
  (node-join (swig:has-named-attribute-with-value "name" name)
	     (select-kids (ntype?? 'attributelist))
	     (select-kids (ntype?? 'parmlist))
	     (select-kids (ntype?? 'parm))
	     (swig:named-attribute-value "type")))


(define (generate-function name type)
  (let ((function-sym (string->symbol name))
	(parameters-type (let ((types (cadr type)))
			   (if (equal? types '(void)) '() types)))
	(return-type (caddr type)))
    `(define ,function-sym (c-lambda ,parameters-type ,return-type ,name))))


(define (generate-functions nodes)
  (let* ((functions (filter-functions nodes))
	 (types (map (lambda (type decl)
		       ;; Only a pointer to a function makes sense here
		       (parse-type (string-append "p." decl type)))
		     ((swig:named-attribute-value "type") functions)
		     ((swig:named-attribute-value "decl") functions))))
    (map generate-function 
	 (swig:name-attribute-value functions)
	 types)))


;;;
;;;; ffi generation
;;;


(define filter-user-includes
  (node-join (select-kids (ntype?? 'top))
	     (select-kids (ntype?? 'include))
	     (swig:node-has-attribute-with '(value "/dev/stdin"))
	     (select-kids (ntype?? 'include))))


(define filter-data-nodes
  (select-kids (sxml:complement (ntype-names?? '(@ attributelist)))))


(define (generate-ffi node)                     
  (let ((nodes (filter-data-nodes (filter-user-includes node))))
    (append '((include "ffi-prelude.scm"))
            (generate-constants nodes)
	    (generate-enums nodes)
            (generate-typedefs nodes)
	    (generate-structs nodes)
	    (generate-functions nodes))))


(define (main . arguments)
  (let ((port (open-swig-port)))
    (display "%module mod" port)
    (newline port)
    (for-each (lambda (file)
                (display (string-append "%include " file) port)
                (newline port))
              arguments)
    (force-output port)
    (close-output-port port)
    (let* ((ptree (generate-sxml port))
           (ffi   (generate-ffi ptree))
           (includes (cons "#include <stdlib.h>" (map (lambda (h) (string-append "#include \"" h "\"")) arguments)))
           (declares (map (lambda (declare)
                            `(c-declare ,declare))
                          (append includes inserts))))
      (for-each (lambda (x)
                  (write x)
                  (newline))
                (append declares ffi)))))
    
  
