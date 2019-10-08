#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; Wai Chun Leung
;; CruzID: 1611864
;;
;; $Id: sbi.scm,v 1.10 2019-01-15 14:10:54-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

; ===================================================
; ================== STARTER CODE ===================
; ===================================================

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

; ===================================================
; =================== HASH TABLES ===================
; ===================================================
(define *label-table* (make-hash))

(define *array-table* (make-hash))

(define *variable-table* (make-hash))
(for-each
    (lambda (item) (hash-set! *variable-table* (car item) (cadr item)))
    `((nan ,(/ 0.0 0.0))
      (eof ,0.0)
      (pi ,(acos -1.0))
      (e, (exp 1.0))))

(define *function-table* (make-hash))
(for-each
    (lambda (item) (hash-set! *function-table* (car item) (cadr item)))
    `((+ ,+)
      (- ,-)
      (* ,*)
      (/ ,/)
      (% ,(lambda (x y) (- x (* (truncate (/ x y)) y))))
      (= ,=)
      (< ,<)
      (> ,>)
      (>= ,>=)
      (<= ,<=)
      (<> ,(lambda (x y) (not (= x y))))
      (^ ,expt)
      (abs ,abs)
      (acos ,acos)
      (asin ,asin)
      (atan ,atan)
      (ceil ,ceiling)
      (cos ,cos)
      (exp ,exp)
      (floor ,floor)
      (log ,log)
      (log10 ,(lambda (x) (/ (log x) (log 10))))
      (log2 ,(lambda (x) (/ (log x) (log 10))))
      (round ,round)
      (sin ,sin)
      (sqrt ,sqrt)
      (tan ,tan)
      (trunc ,truncate)))

; ===================================================
; ============== FUNCTION STATEMENTS ================
; ===================================================
(define (func-print args)
    (cond
        ((null? args) (printf "~n")) ; catch if args are empty
        ((null? (cdr args)) ; only 1 argument left
            (display (int-expression(car args)))
            (printf "~n") ;print new line
        )
        (else
            (display (int-expression (car args)))
            (func-print (cdr args)))))


(define (func-let args)
    (if (symbol? (car args)) ;check if first arg is variable or array
        (hash-set! *variable-table* 
            (car args) 
            (int-expression (cadr args)))
        (vector-set! (hash-ref *array-table* 
            (cadar args))
            (exact-round (- (int-expression (caddar args)) 1.0))
            (int-expression (cadr args)))))


(define (func-if args)
    ; (printf "testing if: ~s~n" (car args))
    (if (int-expression (car args))
        (int-program (hash-ref *label-table* (cadr args)))
        (void))) ;nothing happens


(define (func-input args)
    (let ((object (read)))
        (cond [(eof-object? object)
                    (hash-set! *variable-table* (car args) 
                    (hash-ref *variable-table* 'nan))
                    (hash-set! *variable-table* 'eof 1)]
              [(number? object) 
                    (hash-set! *variable-table* (car args) object)]
              [else (hash-set! *variable-table* (car args)
                    (hash-ref *variable-table* 'nan))
                    (hash-ref *variable-table* 'nan)]))
    (if (not (null? (cdr args)))
        (func-input (cdr args))
        (void)))

(define (func-goto args)
    (int-program (hash-ref *label-table* (car args))))

(define (func-dim args)
    (hash-set! *array-table* 
        (cadar args)
        (make-vector (exact-round (int-expression (caddar args))))))

; ===================================================
; ================= MAIN FUNCTIONS =================
; ===================================================

(define (int-statement line)
    (cond [(eq? (caar line) 'print) (func-print (cdar line))]
          [(eq? (caar line) 'let) (func-let (cdar line))]
          [(eq? (caar line) 'if) (func-if (cdar line))]
          [(eq? (caar line) 'input) (func-input (cdar line))]
          [(eq? (caar line) 'goto) (func-goto (cdar line))]
          [(eq? (caar line) 'dim) (func-dim (cdar line))]
          [else (die `("Invalid Statement: " ,line))] ))


; Reference: evalexpr.scm
(define (int-expression args)
    (cond
        [(number? args) (+ 0.0 args)] ;argument is only a single number
        [(not(pair? args)) ;there exists only one argument
            (if (symbol? args) 
                (hash-ref *variable-table* args) args)]
        [else
            (if (eq? (car args) 'asub)
                (vector-ref 
                    (hash-ref *array-table* (cadr args))
                    (- (exact-round (int-expression (caddr args))) 1))
                (let ((fn (hash-ref *function-table* (car args)))
                        (args (map int-expression (cdr args))))
                        (apply fn args)))]))
        

(define (int-program program)
    (cond [not (null? (car program))
        (define line (car program))
        (cond [(null? (cdr line))] ; no label no statement
              [(not (null? (cddr line))) ; label + statement
                    (int-statement (cddr line))]
              [(symbol? (cadr line))] ; label
              [else (int-statement (cdr line))]) ; statement
        (if (not (null? (cdr program)))
            (int-program (cdr program)) ;tail recursion
            (exit 0))])) ;call exit if next line is null
    

(define (add-labels program)
    (cond
        (not (null? (car program))
            (define line (car program))
            (cond [(null? (cdr line))]
                  [(symbol? (cadr line)) 
                      (hash-set! *label-table* (cadr line) program)])

            (if (not (null? (cdr program)))
                (add-labels (cdr program))
                (void)))))

        
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile)))
                (add-labels program)
                (int-program program))))


; ===================================================
; =================== SOURCE CODE ===================
; ===================================================
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1))

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename")))

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(main (vector->list (current-command-line-arguments)))
