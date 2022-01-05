(module um mzscheme
  (provide (all-defined))

  ;;  Limitations of the mzscheme impl:
  ;;   Failures to meet spec:
  ;;    * array and finger size limited by running out of memory
  ;;
  ;;  Inefficiencies
  ;;    * parsing the program is really slow
  
  ;; ---------------------------------------------------------------------------
  ;; scheme-implementation-dependent code
  ;; mzscheme implementations
  
  ;; random integer in any range
  (require (only (lib "27.ss" "srfi") random-integer))

  (require (only (lib "43.ss" "srfi") vector-copy))

  (define my-error error)
  
  ;; word is represented as a bignum in the range [0, 2^32)
  
  (define *2^32* (expt 2 32))

  (define word0 0)
  (define word- -)
  (define word= =)
  (define word<= <=)
  ;; for efficiency, don't check whether it's actually an int
  ;; let the bignum go through and we'll get an error later if it was wrong
  (define int->word (lambda (x) x))
  (define word->int (lambda (x) x)) 
  (define (bytes->word s) (integer-bytes->integer (apply bytes s) #f #t))
  
  ;; word -> int
  ;; get the integer representing the given bits of the word
  ;; inclusive of the given bits
  (define (get-bits hi-bit-index low-bit-index n)
    (let ((first-shift (- 31 hi-bit-index)))
      (word->int 
       (arithmetic-shift (modulo (arithmetic-shift n first-shift) *2^32*)
			 (- (+ low-bit-index first-shift))))))

  ;; the sign of the modulo is the sign of the second arg, so this should work
  (define (add32 x y) (modulo (+ x y) *2^32*))
  (define (sub32 x y) (modulo (- x y) *2^32*))
  (define (mult32 x y) (modulo (* x y) *2^32*))
  (define (div32 x y) (quotient x y)) ;; need to do the mod?
  
  ;; word -> word
  (define (convert-2comp-to-unsigned n)
    (if (>= n 0) n (+ n *2^32*)))
  
  (define (unsigned-bitwise-nand n1 n2) 
    (convert-2comp-to-unsigned (bitwise-not (bitwise-and n1 n2))))
  
  ;; ----------------------------------------------------------------------
  ;; SCHEME-IMPLEMENTATION-INDEPENDENT CODE BEGINS HERE
  ;; copy and paste-able between all scheme ums
  ;; ----------------------------------------------------------------------
  
  ;; ----------------------------------------------------------------------
  ;; portable primitives
  
  (define first car)
  (define second cadr)  
  (define third caddr)
    
  ;; ----------------------------------------------------------------------
  ;; machine impl
   
  ;; word vector * word vector * int
  ;;   registers is a vector of 8 words
  ;;   heap is a hash-table of array "options" (might be #f)
  ;;   finger is the current index into the 0 array
  ;; structs are mutable, so these are all implicitly refs
  ;; (define-struct machine (registers heap finger))
  (define (make-machine registers heap finger) 
    (list registers heap finger))
  (define machine-registers first)
  (define machine-heap second)
  (define machine-finger third)
  (define (set-machine-finger! m new-f)
    (set-car! (cdr (cdr m)) new-f))
  
  (define (initial-machine code)
    (make-machine (make-vector 8 word0)
                  (let ((ht (make-hash-table)))
                    (begin 
                      (hash-table-put! ht word0 code)
                      ht))
                  0))

  ;; register index is an int
  ;; heap index is a word
  ;; platter index (into an array) is an int  

  ;; word -> symbol
  (define (decode-op w)
    (let ((opcode (get-bits 31 28 w)))
      (cond 
        ((= opcode 0) 'cmove)
        ((= opcode 1) 'array_index)
        ((= opcode 2) 'array_amend)
        ((= opcode 3) 'add)
        ((= opcode 4) 'mult)      
        ((= opcode 5) 'div)
        ((= opcode 6) 'nand)
        ((= opcode 7) 'halt)
        ((= opcode 8) 'alloc)
        ((= opcode 9) 'abandon)
        ((= opcode 10) 'output)
        ((= opcode 11) 'input)
        ((= opcode 12) 'load_prog)
        ((= opcode 13) 'load_to_reg)
        (else (my-error "bad opcode")))))
  
  ;; w word -> list of 3 register-indices
  (define (decode-registers w)
    (list 
     (get-bits 8 6 w)
     (get-bits 5 3 w)
     (get-bits 2 0 w)))
  
  ;; word -> register-index and word 
  (define (decode-load_to_reg-data w)
    (values (get-bits 27 25 w)
            (int->word (get-bits 24 0 w))))

  ;; machine * register-index -> word
  (define (register-get m i) 
    (vector-ref (machine-registers m) i))
  
  ;; machine * register-index * word -> "void"
  (define (register-set! mach set-this to-this)
    (vector-set! (machine-registers mach) set-this to-this))
  
  ;; machine * word -> array
  (define (heap-get-array m array-number)
    (hash-table-get (machine-heap m) array-number))
  
  ;; machine * word * platter-index -> word
  (define (heap-get-platter m array-number offset)
    (vector-ref (heap-get-array m array-number)
		offset))
  
  ;; machine * word * word * platter-index -> "void"
  (define (heap-set-platter! m array-number offset new-value)
    (vector-set! (heap-get-array m array-number) 
		 offset
		 new-value))
  
  ;; machine * word -> "void"
  (define (heap-set-array! m array-number new-array)
    (hash-table-put! (machine-heap m) array-number new-array))
  
  ;; machine * word -> "void"
  (define (heap-remove-array! m array-number)
    (hash-table-remove! (machine-heap m) array-number))
  
  ;; machine -> word
  (define (fresh-heap-index m)
    (let ((new (random-integer *2^32*)))
      (if ;; test whether the key is present or not
       (hash-table-get (machine-heap m) new (lambda () #f))
       (fresh-heap-index m)
       (int->word new))))
  
  ;; machine -> "void"
  (define (print-machine m)
    (printf "Machine: ~n"))
  
  ;; machine -> "void"
  (define print-step-info
    (let ((count 0))
      (lambda (m w)
        (begin
          (set! count (+ count 1))
          ;; print how many steps we've taken so far
          (if (= (modulo count 1000) 0)
              (begin (printf "Step number: ~a~n" count)
                     #t)
              #t)
          ;; print current operation and registers
          (printf "Machine-lang op: ~x~nOpcode: ~a~nOperation Register Indices:~a~nMachine Register Values:~a~n~n" 
                  w
                  (decode-op w)
                  (decode-registers w)
                  (machine-registers m))
          ))))
  
  ;; machine -> "void"
  ;; most failures just trip scheme errors
  ;; the intention is that all errors at least get trapped somehow, rather than failing silently
  (define (step m)
    (let* ((w (heap-get-platter m word0 (machine-finger m)))
           
           (op (decode-op w))
           (reg-indices (decode-registers w))
           (reg-values (map (lambda (i) (register-get m i)) reg-indices))
           
           (ai (first reg-indices))
           (bi (second reg-indices))
           (ci (third reg-indices))
           
           (av (first reg-values))
           (bv (second reg-values))
           (cv (third reg-values)))
      (begin 
        ;; DEBUG
        ;; (print-step-info m w)
        
        ;; increment the finger
        (set-machine-finger! m (+ (machine-finger m) 1))
        
        ;; do the op
        ;; the order of these alternatives be tuned to match the instruction frequency
        (cond 
          ((eq? op 'load_to_reg)
           (call-with-values (lambda () (decode-load_to_reg-data w))
                             (lambda (reg data)
                               (register-set! m reg data))))
          ((eq? op 'cmove) 
           (if (word= cv word0)
               (if #f 1) ;; void
               (register-set! m ai bv))
           )
          ((eq? op 'array_index)
           (register-set! m ai (heap-get-platter m bv (word->int cv))))
          ((eq? op 'array_amend)
           (heap-set-platter! m av (word->int bv) cv))
          ((eq? op 'add)
           (register-set! m ai (add32 bv cv)))
          ((eq? op 'mult)
           (register-set! m ai (mult32 bv cv)))
          ((eq? op 'div)
           (register-set! m ai (div32 bv cv)))
          ((eq? op 'nand)
           (register-set! m ai (unsigned-bitwise-nand bv cv)))
          ((eq? op 'halt)
           (exit))
          ((eq? op 'alloc)
           (let ((new-index (fresh-heap-index m))
                 (new-array (make-vector (word->int cv) word0)))
             (begin 
               (heap-set-array! m new-index new-array)
               (register-set! m bi new-index))))
          ((eq? op 'abandon)
           (heap-remove-array! m cv))
          ((eq? op 'output) 
           (if (and (word<= word0 cv) (word<= cv (int->word 255)))
               (begin (printf "~a" (integer->char (word->int cv)))
                      (flush-output)) ;; drscheme is fine without this,
               ;; but mzscheme doesn't print before reads
               ;; strange, since the docs say you can't usefully flush stdout
               (my-error "bad character")))
          ((eq? op 'input) 
           (let ((in (read-char)))
	     (if (eof-object? in)
		 (word- *2^32* (int->word 1)) ; all 1's
		 (let ((in-int (char->integer in)))
		   (if (and (<= 0 in-int) (<= in-int 255))
		       (register-set! m ci (int->word in-int))
		       (my-error "input not in range"))))))
          ((eq? op 'load_prog)
           (begin
             (heap-set-array! m word0 (if (= bv word0)
					  ;; optimization: don't copy if it's already the 0 array
					  ;; this saves a lot of work because jumps are defined in terms of load_prog
					  (heap-get-array m bv)
					  (vector-copy (heap-get-array m bv))
					  ))
             (set-machine-finger! m (word->int cv))))
          (else (my-error "invalid opcode"))
          )
        ;; increment the finger
        )))
  
  (define (step-to-halt m)
    (begin (step m)
           (step-to-halt m)))
  
  ;; stack overflow on big input files when this wasn't tail-recursive
  (define (read-program input)
    (letrec ((loop 
               (lambda (acc)
                 (let ((four-bytes (list (read-byte input) (read-byte input) (read-byte input) (read-byte input))))
                   (if (eof-object? (first four-bytes))
                       acc
		       ;; assumes that none are eof
		       (loop (cons (bytes->word four-bytes) acc)))))))
      (list->vector (reverse (loop '())))))

  (define (run filename)
    (let* ((input (open-input-file filename))
           (initial-prog 
	    (begin 
	      (printf "Reading program file...~n")
	      (read-program input)))
	   (the-machine (initial-machine initial-prog)))
      (begin 
	(printf "Starting machine...~n")
	(step-to-halt the-machine))))
  
  ;; ----------------------------------------------------------------------
  ;; tests
  
  (define (test-decode-op)
    (let ((dec-eq (lambda (x y) (eq? (decode-op x) y))))
      (list 
       (dec-eq #x0000012B 'cmove)
       (dec-eq #x10000046 'array_index)
       (dec-eq #x20000032 'array_amend)
       (dec-eq #x300000DC 'add)
       (dec-eq #x40000081 'mult)
       (dec-eq #x500001FD 'div)
       (dec-eq #x60000176 'nand)
       (dec-eq #x70000000 'halt)
       (dec-eq #x80000002 'alloc)
       (dec-eq #x90000000 'abandon)
       (dec-eq #xA0000000 'output)
       (dec-eq #xB0000003 'input)
       (dec-eq #xC0000031 'load_prog)
       (dec-eq #xD4000004 'load_to_reg)
       )))
  
  (define (test-decode-registers)
    (list
     ;; 20000188   upd zz[bb] <- aa
     (equal? (decode-registers #x20000188) '(??? 1 0))
     ;; 300001EB   add hh <- ff + dd
     (equal? (decode-registers #x300001EB) '(7 5 3))))
  
  (define (test-decode-load_reg)
    (list 
     ;; D2004E20   literal bb <- 4E20
     (equal? (decode-load_to_reg-data #xD2004E20) '(1 #x4E20))))

  ;; ----------------------------------------------------------------------
  ;; SCHEME-IMPLEMENATION-INDEPENDENT CODE ENDS HERE
  ;; ----------------------------------------------------------------------

)
  
;; ----------------------------------------------------------------------
;; running

(require um)

;; run particular ones
;; (run "../../test-binaries/internal-challenge.um")
;; (run "../../test-binaries/umix.269.um")
;; (run "../tests/cat.um")

;; for mzscheme command line
(run (vector-ref (current-command-line-arguments) 0))