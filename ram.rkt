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
  (and (instruction? i)
       (match i
         ((list _ 'HALT) #t)
         ((list 'HALT) #t)
         (_ #f))))

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
        ((list label _ _)
         (hash-set! table label i))
        (_ (void))))
    table))

(define (build-executable program)
  (validate-program program)
  (let* ([program (list->vector program)]
         [labels (build-label-table program)])
    (cons program labels)))

(define (run executable tape)
  (let ([output-tape '()]
        [labels (cdr executable)]
        [code (car executable)]
        [instr-ptr 0]
        [stack-ptr 0]
        [memory (make-hash)])
    (for ([i (in-range 0 (vector-length code))]
          #:break (halt? (vector-ref code i)))
      (print "xD\n"))))

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
    (run executable '())
    (close-input-port file)))

(main)