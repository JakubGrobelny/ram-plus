#lang racket

(require racket/cmdline)
(require racket/hash)

(define instructions
  '(ADD MULT DIV SUB
    READ WRITE STORE LOAD
    JUMP JZERO JGTZ JLTZ
    PUSH POP CALL RET
    HALT))

(define (label? l)
  (and (symbol? l)
       (char-lower-case? (string-ref (symbol->string l) 0))))

(define (halt? i)
  (match i
    ((list _ 'HALT) #t)
    ((list 'HALT) #t)
    (_ #f)))

(define (argument? arg)
  (or (label? arg)
      (integer? arg)
      (match arg
        ((list '= x) (integer? x))
        ((list '* x) (integer? x))
        (_ #f))))

(define (instruction? i)
  (if (member i instructions)
      #t
      #f))

(define (validate-program program)
  (map (lambda (p)
            (if (match p
                  ('() #t)
                  ('(HALT) #t)
                  ('(RET) #t)
                  ((list label instr arg)
                   (and (label? label)
                    (instruction? instr)
                    (argument? arg)))
                  ((list label 'HALT)
                   (label? label))
                  ((list instr arg)
                   (and (instruction? instr)
                        (argument? arg)))
                  (_ #f))
                #t
                (error (format "Invalid instruction ~a!" p))))
          program))

(define (build-label-table program)
  (let ([table (make-hash)])
    (for ([i (in-range 0 (vector-length program))])
      (match (vector-ref program i)
        ((cons label _)
         (if (label? label)
             (begin
               (hash-set! table label i)
               (vector-set! program i (cdr (vector-ref program i))))
             (void)))
        (_ (void))))
    table))

(define (build-executable program)
  (validate-program program)
  (let* ([program (list->vector program)]
         [labels (build-label-table program)])
    (cons program labels)))

(define (fetch-arg instr)
  (if (eq? (car instr) 'HALT)
      '()
      (second instr)))
 
(define (deref-arg arg memory labels)
  (match arg
    ((list '= x) x)
    ((list '* x) (hash-ref! memory (hash-ref! memory x 0) 0))
    (x #:when (label? x)
       (hash-ref! labels x -1))
    (x (hash-ref! memory x 0))))

(define (half-deref-arg arg memory labels)
  (match arg
    ((list '= x) x)
    ((list '* x) (hash-ref! memory x 0))
    (x #:when (label? x)
       (hash-ref! labels x -1))
    (x x)))

(define (fetch-mnemonic instr)
  (car instr))

(define (run executable tape)
  (define (loop code labels memory stack output-tape instr-ptr)
    (if (or (>= instr-ptr (vector-length code))
            (halt? (vector-ref code instr-ptr)))
        output-tape
        (let* ([instr (vector-ref code instr-ptr)]
               [arg (fetch-arg instr)]
               [mnemonic (fetch-mnemonic instr)])
          (match mnemonic
            ('ADD
             (hash-set! memory 0
                        (+ (hash-ref! memory 0 0)
                           (deref-arg arg memory labels))))
            ('SUB
             (hash-set! memory 0
                        (- (hash-ref! memory 0 0)
                           (deref-arg arg memory labels))))
            ('DIV
             (hash-set! memory 0
                        (quotient (hash-ref! memory 0 0)
                                  (deref-arg arg memory labels))))
            ('MULT
             (hash-set! memory 0
                        (* (hash-ref! memory 0 0)
                           (deref-arg arg memory labels))))
            ('READ
             (hash-set! memory arg
                        (if (null? tape)
                            (error "No input to read!")
                            (let ([val (car tape)])
                              (set! tape (cdr tape))
                              val))))
            ('WRITE
             (set! output-tape
                   (cons
                    (deref-arg arg memory labels)
                    output-tape)))
            ('STORE
             (hash-set! memory
                        (half-deref-arg arg memory labels)
                        (hash-ref! memory 0 0)))
            ('LOAD
             (hash-set! memory
                        0
                        (hash-ref! memory (half-deref-arg arg memory labels) 0)))
            ('JUMP
             (set! instr-ptr (- (half-deref-arg arg memory labels) 1)))
            ('JZERO
             (if (= 0 (hash-ref! memory 0 0))
                 (set! instr-ptr (- (half-deref-arg arg memory labels) 1))
                 (void)))
            ('JGTZ
             (if (< 0 (hash-ref! memory 0 0))
                 (set! instr-ptr (- (half-deref-arg arg memory labels) 1))
                 (void)))
            ('JLTZ
             (if (> 0 (hash-ref! memory 0 0))
                 (set! instr-ptr (- (half-deref-arg arg memory labels) 1))
                 (void)))
            ('PUSH
             (set! stack
                   (cons (half-deref-arg arg memory labels)
                         stack)))
            ('POP
             (if (null? stack)
                 (error "Stack underflow!")
                 (begin
                   (hash-set! memory
                              (half-deref-arg arg memory) (car stack))
                   (set! stack
                         (cdr stack)))))
            ('CALL
             (begin
               (set! stack (cons instr-ptr stack))
               (set! instr-ptr (- arg 1))))
            ('RET
             (begin
               (set! instr-ptr (- (car stack) 1))
               (set! stack (cdr stack)))))
          (loop code labels memory stack output-tape (+ 1 instr-ptr)))))
  (let ([output-tape '()]
        [labels (cdr executable)]
        [code (car executable)]
        [stack '()]
        [memory (make-hash)])
    (loop code labels memory stack output-tape 0)))

(define (main)
  (let* ([file-to-run (command-line
                       #:program "ram+"
                       #:args (filename)
                       filename)]
         [file (open-input-file
                file-to-run
                #:mode 'text)]
         [program (read file)]
         [executable (build-executable program)])
    (printf "~a\n" (run executable (read)))
    (close-input-port file)))

(main)